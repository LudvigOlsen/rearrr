

#   __________________ #< b374fd49354d1eddc7d25f2e43608f7c ># __________________
#   Fixed groups pipeline                                                   ####

# Describe how this pipeline requires grouping the data frame beforehand
# Also that the transformations are applied to groups separately
# why a function that requires group_cols (e.g. cluster_groups) will get an ungrouped data frame
# and one should set group_cols in the args

# Reminder: Side-effect R6 methods should always return self invisibly!

# Applies one transformation at a time
# With different arguments per group


#' @title Chain multiple transformations with different argument values per group
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Build a pipeline of transformations to be applied sequentially.
#'
#'  Specify different argument values for each group in a fixed set of groups.
#'  E.g. if your \code{data.frame} contains 5 groups, you provide 5 argument values
#'  for each of the non-constant arguments (see \code{`var_args`}).
#'
#'  The number of expected groups is specified during initialization and the input
#'  \code{`data`} must be grouped such that it contains that exact number of groups.
#'
#'  Transformations are applied to groups separately, why the given transformation function
#'  only receives the subset of \code{`data`} belonging to the current group.
#'
#'  \strong{Standard workflow}: Instantiate pipeline -> Add transformations -> Apply to data
#'
#'  To apply the same arguments to all groups, see
#'  \code{\link[rearrr:Pipeline]{Pipeline}}.
#'
#'  To apply generated argument values to an arbitrary number of groups,
#'  see \code{\link[rearrr:GeneratedPipeline]{GeneratedPipeline}}.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family pipelines
#' @examples
#' # Attach package
#' library(rearrr)
#' library(dplyr)
#'
#' # Create a data frame
#' # We group it by G so we have 3 groups
#' df <- data.frame(
#'   "Index" = 1:12,
#'   "A" = c(1:4, 9:12, 15:18),
#'   "G" = rep(1:3, each = 4)
#' ) %>%
#'   dplyr::group_by(G)
#'
#' # Create new pipeline
#' pipe <- FixedGroupsPipeline$new(num_groups = 3)
#'
#' # Add 2D rotation transformation
#' pipe$add_transformation(
#'   fn = rotate_2d,
#'   args = list(
#'     x_col = "Index",
#'     y_col = "A",
#'     suffix = "",
#'     overwrite = TRUE
#'   ),
#'   var_args = list(
#'     degrees = list(45, 90, 180),
#'     origin = list(c(0, 0), c(1, 2), c(-1, 0))
#'   ),
#'   name = "Rotate"
#' )
#'
#' # Add the `cluster_group` transformation
#' # As the function is fed an ungrouped subset of `data`,
#' # i.e. the rows of that group, we need to specify `group_cols` in `args`
#' # That is specific to `cluster_groups()` though
#' # Also note `.apply` in `var_args` which tells the pipeline *not*
#' # to apply this transformation to the second group
#' pipe$add_transformation(
#'   fn = cluster_groups,
#'   args = list(
#'     cols = c("Index", "A"),
#'     suffix = "",
#'     overwrite = TRUE,
#'     group_cols = "G"
#'   ),
#'   var_args = list(
#'     multiplier = list(0.5, 1, 5),
#'     .apply = list(TRUE, FALSE, TRUE)
#'   ),
#'   name = "Cluster"
#' )
#'
#' # Check pipeline object
#' pipe
#'
#' # Apply pipeline to already grouped data.frame
#' # Enable `verbose` to print progress
#' pipe$apply(df, verbose = TRUE)
#'
FixedGroupsPipeline <- R6::R6Class(
  "FixedGroupsPipeline",
  inherit = Pipeline,
  public = list(

    #' @field transformations \code{list} of transformations to apply.
    transformations = list(),

    #' @field names Names of the transformations.
    names = character(),

    #' @field num_groups Number of groups the pipeline will be applied to.
    num_groups = NULL,

    #' @description
    #'  Initialize the pipeline with the number of groups the
    #'  pipeline will be applied to.
    #' @param num_groups Number of groups the pipeline will be applied to.
    initialize = function(num_groups) {
      checkmate::assert_integerish(num_groups, lower = 1, any.missing = FALSE, len = 1)
      self$num_groups <- num_groups
    },

    #' @description
    #'  Add a transformation to the pipeline.
    #' @param fn Function that performs the transformation.
    #' @param args Named \code{list} with arguments for the \code{`fn`} function.
    #' @param var_args Named \code{list} of arguments with \code{list} of differing
    #'   values for each group.
    #'
    #'   E.g. \code{list("a" = list(1, 2, 3), "b" = list("a", "b", "c"))} given 3 groups.
    #'
    #'   By adding \code{".apply"} with a list of \code{TRUE}/\code{FALSE} flags, the transformation
    #'   can be disabled for a specific group.
    #'
    #'   E.g. \code{list(".apply" = list(TRUE, FALSE, TRUE), ...}.
    #' @param name Name of the transformation step. Must be unique.
    #' @return The pipeline. To allow chaining of methods.
    add_transformation = function(fn, args, var_args, name) {
      if (name %in% self$names) {
        stop(paste0("the `name`, ", name, ", already exists. Names must be unique."))
      }

      # Add transformation to pipeline
      self$names <- append(self$names, name)
      # Convert to transformation object
      transformation <- FixedGroupsTransformation$new(
        fn = fn,
        args = args,
        var_args = var_args,
        name = name
      )
      # Check transformation has correct number of groups setting
      if (transformation$num_groups != self$num_groups) {
        stop(
          paste0(
            "the transformation must have the same number of groups (see",
            " `var_args`) as the `FixedGroupsPipeline`."
          )
        )
      }
      self$transformations <- c(self$transformations,
                                setNames(list(transformation), name))

      # Return object invisibly to allow method chaining
      invisible(self)
    },

    #' @description
    #'  Apply the pipeline to a \code{data.frame}.
    #' @param data \code{data.frame} with the same number of groups as pre-registered
    #'  in the pipeline.
    #'
    #'  You can find the number of groups in \code{`data`} with \code{`dplyr::n_groups(data)`}.
    #'  The number of groups expected by the pipeline can be accessed with \code{`pipe$num_groups`}.
    #' @param verbose Whether to print the progress.
    #' @return Transformed version of \code{`data`}.
    apply = function(data, verbose = FALSE) {
      if (dplyr::n_groups(data) != self$num_groups){
        stop(paste0("`data` did not have exactly ", self$num_groups, " groups as expected."))
      }
      super$apply(data = data, verbose = verbose)
    },

    #' @description
    #'  Print an overview of the pipeline.
    #' @param ... further arguments passed to or from other methods.
    #' @return The pipeline. To allow chaining of methods.
    print = function(...) {
      cat("FixedGroupsPipeline: \n")
      cat("  No. expected groups: ", self$num_groups, "\n", sep = "")
      for (name in self$names){
        print(self$transformations[[name]], indent = 2, show_class = FALSE)
      }
      # Return object invisibly to allow method chaining
      invisible(self)
    }
  ),
  private = list(
    # Groupings in the input data will be used
    warn_grouped_input = FALSE
  )
)


##  .................. #< cc7000f69b35bea0385d03803f3444b0 ># ..................
##  Fixed group transformation                                              ####


# A transformation to be applied with
# different argument values to each group

#' @title FixedGroupsTransformation
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Container for the type of transformation used in
#'  \code{\link[rearrr:FixedGroupsPipeline]{FixedGroupsPipeline}}.
#'
#'  \strong{Note}: For internal use.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family transformation classes
FixedGroupsTransformation <- R6::R6Class(
  "FixedGroupsTransformation",
  inherit = Transformation,
  public = list(

    #' @field name Name of transformation.
    name = NULL,

    #' @field fn Transformation function.
    fn = NULL,

    #' @field args \code{list} of constant arguments for \code{`fn`}.
    args = NULL,

    #' @field var_args \code{list} of arguments for \code{`fn`} with different values per group.
    var_args = NULL,

    #' @field num_groups Number of groups that the transformation expects.
    num_groups = NULL,

    #' @field apply_arg \code{list} of \code{TRUE}/\code{FALSE} flags indicating
    #'  whether the transformation should be applied to each of the groups.
    #'
    #'  When \code{`NULL`}, the transformation is applied to all groups.
    apply_arg = NULL,

    #' @description
    #'  Initialize transformation.
    #' @param fn Transformation function.
    #' @param args \code{list} of constant arguments for \code{`fn`}.
    #' @param var_args \code{list} of arguments for \code{`fn`} with different values per group.
    #'  Each argument should have a list of values (one per group).
    #'
    #'  By adding \code{".apply"} with a list of \code{TRUE}/\code{FALSE} flags, the transformation
    #'   can be disabled for a specific group.
    #'
    #'   E.g. \code{list(".apply" = list(TRUE, FALSE, TRUE), ...}.
    #' @param name Name of transformation.
    initialize = function(fn, args, var_args, name = NULL) {
      # Check arguments
      private$check_initialize_args(
        fn = fn,
        args = args,
        var_args = var_args,
        name = name
      )

      # Assign to object
      self$fn <- fn
      self$args <- args
      self$name <- name

      # Extract potential apply argument
      if (".apply" %in% names(var_args)){
        self$apply_arg <- var_args[[".apply"]]
        if (checkmate::test_logical(self$apply_arg, any.missing = FALSE)){
          stop("The apply argument must be a logical (a `TRUE`/`FALSE` per group).")
        }
        var_args <- var_args[names(var_args) != ".apply"]
      }

      # Assign rest to object
      self$var_args <- var_args
      self$num_groups <- length(var_args[[1]])
    },

    #' @description
    #'  Get arguments for specific group ID.
    #' @param group_id ID of the group to get arguments for.
    #' @return \code{list} of arguments.
    get_group_args = function(group_id) {
      checkmate::assert_integerish(
        group_id,
        lower = 1,
        upper = self$num_groups,
        any.missing = FALSE,
        len = 1
      )
      # Get var args values for current group
      var_args <- sapply(self$var_args, `[[`, group_id)
      var_args <- setNames(var_args, names(self$var_args))
      c(self$args, var_args)
    },

    #' @description
    #'  Apply the transformation to a \code{data.frame}.
    #' @param data \code{data.frame} with the expected number of groups.
    #' @return Transformed version of \code{`data`}.
    apply = function(data) {
      # Ensure we have the right number of groups
      # E.g. if `data` was somehow ungrouped between transformations
      if (dplyr::n_groups(data) != self$num_groups) {
        stop(
          paste0(
            "`data` did not have the right number of groups: ",
            dplyr::n_groups(data),
            " != ",
            self$num_groups,
            "."
          )
        )
      }
      super$apply(data = data)
    },

    #' @description
    #'  Print an overview of the transformation.
    #' @param ... further arguments passed to or from other methods.
    #' @param indent How many spaces to indent when printing.
    #' @param show_class Whether to print the transformation class name.
    #' @return The pipeline. To allow chaining of methods.
    print = function(...,
                     indent = 0,
                     show_class = TRUE) {
      # Create string with indentation spaces
      indentation_str <- paste0(rep(" ", indent), collapse = "")

      # Extract name
      name <- self$name
      if (is.null(name)) {
        name <- "NoName"
      }

      # Print transformation class name
      if (isTRUE(show_class)){
        cat(indentation_str, "FixedGroupsTransformation: \n")
        indentation_str <- paste0(rep(" ", indent + 2), collapse = "")
      }

      # Print name of transformation
      cat(indentation_str, name, "\n", sep = "")

      # Print arguments
      args_str <- private$args_to_string(self$args)
      cat(indentation_str, "  Constant arguments:  ", args_str, "\n", sep = "")

      # Print varying arguments
      var_args_str <- private$args_to_string(self$var_args)
      cat(indentation_str, "  Varying arguments:   ", var_args_str, "\n", sep = "")

      # Print which groups this transformation should *not* be applied to
      if (!is.null(self$apply_arg)){
        group_ids <- seq_len(self$num_groups)
        dont_apply_to <- group_ids[!unlist(self$apply_arg)]
        dont_apply_to_str <- private$args_to_string(dont_apply_to, rm_capitalized_l = TRUE)
        cat(indentation_str, "  Don't apply to these groups:  ", dont_apply_to_str, "\n", sep = "")
      }

      # Return object invisibly to allow method chaining
      invisible(self)
    }
  ),
  private = list(
    # We use existing groupings in the given data frame
    ungroup_input = FALSE,
    apply_to_group = function(data, group_id) {
      if (dplyr::is_grouped_df(data)) {
        stop("`data` was grouped. Pass the group subset instead.")
      }

      # Check if we should apply the transformation to this group
      if (!is.null(self$apply_arg) &&
          !isTRUE(self$apply_arg[[group_id]])){
        # If the transformation should not be applied to this group
        # It becomes an identity function
        return(data)
      }

      # Prepare arguments and apply function
      args <- self$get_group_args(group_id)
      args <- c(list(data = data), args)
      do.call(self$fn, args, envir = parent.frame())
    },
    check_initialize_args = function(fn, args, var_args, name) {
      super$check_initialize_args(fn = fn, args = args, name = name, group_cols = NULL)
      assert_collection <- checkmate::makeAssertCollection()
      checkmate::assert_list(var_args, names = "unique", add = assert_collection)
      checkmate::reportAssertions(assert_collection)
      if (length(intersect(names(args), names(var_args))) > 0) {
        assert_collection$push("`args` and `var_args` contained the same names.")
      }
      if (sum(abs(diff(lengths(var_args)))) != 0) {
        assert_collection$push("all lists in `var_args` must have the same length (one arg value per group).")
      }
      checkmate::reportAssertions(assert_collection)
    }
  )
)


