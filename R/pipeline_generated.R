

#   __________________ #< 374d2f927b10366fe88a1e973e84371d ># __________________
#   Generated pipeline                                                      ####


# Adding .apply=function(){sample(c(TRUE, FALSE), 1)} to generators
# means transformations would only include a transformation 50% of the time

# Applies one transformation at a time
# With different generated argument values per group

#' @title Chain multiple transformations and generate argument values per group
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Build a pipeline of transformations to be applied sequentially.
#'
#'  Generate argument values for selected arguments with a given set of generators.
#'  E.g. randomly generate argument values for each group in a \code{data.frame}.
#'
#'  Groupings are reset between each transformation. See `group_cols`.
#'
#'  \strong{Standard workflow}: Instantiate pipeline -> Add transformations -> Apply to data
#'
#'  To apply the same arguments to all groups, see
#'  \code{\link[rearrr:Pipeline]{Pipeline}}.
#'
#'  To apply different but specified argument values to a fixed set of groups,
#'  see \code{\link[rearrr:FixedGroupsPipeline]{FixedGroupsPipeline}}.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family pipelines
#' @examples
#' # Attach package
#' library(rearrr)
#'
#' # Create a data frame
#' df <- data.frame(
#'   "Index" = 1:12,
#'   "A" = c(1:4, 9:12, 15:18),
#'   "G" = rep(1:3, each = 4)
#' )
#'
#' # Create new pipeline
#' pipe <- GeneratedPipeline$new()
#'
#' # Add 2D rotation transformation
#' # Note that we specify the grouping via `group_cols`
#' pipe$add_transformation(
#'   fn = rotate_2d,
#'   args = list(
#'     x_col = "Index",
#'     y_col = "A",
#'     suffix = "",
#'     overwrite = TRUE
#'   ),
#'   generators = list(degrees = function(){sample.int(360, 1)},
#'                     origin = function(){rnorm(2)}),
#'   name = "Rotate",
#'   group_cols = "G"
#' )
#'
#' # Add the `cluster_group` transformation
#' # Note that this function requires the entire input data
#' # to properly scale the groups. We therefore specify `group_cols`
#' # as part of `args`. This works as `cluster_groups()` accepts that
#' # argument.
#' # Also note the `.apply` generator which generates a TRUE/FALSE scalar
#' # for whether the transformation should be applied to the current group
#' pipe$add_transformation(
#'   fn = cluster_groups,
#'   args = list(
#'     cols = c("Index", "A"),
#'     suffix = "",
#'     overwrite = TRUE,
#'     group_cols = "G"
#'   ),
#'   generators = list(
#'     multiplier = function() {
#'       0.1 * runif(1) * 3 ^ sample.int(5, 1)
#'     },
#'     .apply = function(){sample(c(TRUE, FALSE), 1)}
#'   ),
#'   name = "Cluster"
#' )
#'
#' # Check pipeline object
#' pipe
#'
#' # Apply pipeline to data.frame
#' # Enable `verbose` to print progress
#' pipe$apply(df, verbose = TRUE)
#'
GeneratedPipeline <- R6::R6Class(
  "GeneratedPipeline",
  inherit = Pipeline,
  public = list(

    #' @field transformations \code{list} of transformations to apply.
    transformations = list(),

    #' @field names Names of the transformations.
    names = character(),

    #' @description
    #'  Add a transformation to the pipeline.
    #' @param fn Function that performs the transformation.
    #' @param args Named \code{list} with arguments for the \code{`fn`} function.
    #' @param generators Named \code{list} of functions for generating argument values
    #'  for a single call of \code{`fn`}.
    #'
    #'  It is possible to include an \emph{apply generator} for deciding whether
    #'  the transformation should be applied to the current group or not.
    #'  This is done by adding a function with the name \code{`.apply`} to the \code{`generators`} list.
    #'  E.g. \code{".apply" = function(){sample(c(TRUE, FALSE), 1)}}.
    #' @param name Name of the transformation step. Must be unique.
    #' @param group_cols Names of the columns to group the input
    #'  data by before applying the transformation.
    #'
    #'   Note that the transformation function is applied separately to each group (subset).
    #'   If the \code{`fn`} function requires access to the entire \code{data.frame}, the
    #'   grouping columns should be specified as part of \code{`args`} and
    #'   handled by the \code{`fn`} function.
    #' @return The pipeline. To allow chaining of methods.
    #' @examples
    #' # `generators` is a list of functions for generating
    #' # argument values for a chosen set of arguments
    #' # `.apply` can be used to disable the transformation
    #' generators = list(degrees = function(){sample.int(360, 1)},
    #'                   origin = function(){rnorm(2)},
    #'                   .apply = function(){sample(c(TRUE, FALSE), 1)})
    add_transformation = function(fn, args, generators, name, group_cols = NULL) {
      if (name %in% self$names) {
        stop(paste0("the `name`, ", name, ", already exists. Names must be unique."))
      }
      # Add transformation to pipeline
      self$names <- append(self$names, name)
      transformation <- GeneratedTransformation$new(
        fn = fn,
        args = args,
        generators = generators,
        name = name,
        group_cols = group_cols
      )
      self$transformations <- c(self$transformations,
                                setNames(list(transformation), name))

      # Return object invisibly to allow method chaining
      invisible(self)
    },

    #' @description
    #'  Print an overview of the pipeline.
    #' @param ... further arguments passed to or from other methods.
    #' @return The pipeline. To allow chaining of methods.
    print = function(...) {
      cat("GeneratedPipeline: \n")
      for (name in self$names){
        print(self$transformations[[name]], indent = 2, show_class = FALSE)
      }
      # Return object invisibly to allow method chaining
      invisible(self)
    }
  )
)


##  .................. #< 66acc1f3fe1ee1b09d407746e25f326f ># ..................
##  Generated transformation                                                ####


# A transformation to be applied with different
# generated argument values to each group

#' @title GeneratedTransformation
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Container for the type of transformation used in
#'  \code{\link[rearrr:GeneratedPipeline]{GeneratedPipeline}}.
#'
#'  \strong{Note}: For internal use.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family transformation classes
GeneratedTransformation <- R6::R6Class(
  "GeneratedTransformation",
  inherit = Transformation,
  public = list(

    #' @field name Name of transformation.
    name = NULL,

    #' @field fn Transformation function.
    fn = NULL,

    #' @field args \code{list} of constant arguments for \code{`fn`}.
    args = NULL,

    #' @field generators \code{list} of generator functions
    #'  for generating argument values.
    generators = NULL,

    #' @field apply_generator Generator function for deciding
    #'  whether to apply the transformation to the current group.
    apply_generator = NULL,

    #' @description
    #'  Initialize transformation.
    #' @param fn Transformation function.
    #' @param args \code{list} of constant arguments for \code{`fn`}.
    #' @param generators Named \code{list} of functions for generating argument values
    #'  for a single call of \code{`fn`}.
    #'
    #'  It is possible to include an \emph{apply generator} for deciding whether
    #'  the transformation should be applied to the current group or not.
    #'  This is done by adding a function with the name \code{`.apply`} to the \code{`generators`} list.
    #'  E.g. \code{".apply" = function(){sample(c(TRUE, FALSE), 1)}}.
    #' @param name Name of transformation.
    #' @param group_cols Names of columns to group \code{data.frame}
    #'  by before applying \code{`fn`}.
    #'
    #'  When \code{`NULL`}, the \code{data.frame} is not grouped.
    initialize = function(fn, args, generators, name = NULL, group_cols = NULL) {
      # Check arguments
      private$check_initialize_args(
        fn = fn,
        args = args,
        generators = generators,
        name = name,
        group_cols = group_cols
      )

      # Assign to object
      self$fn <- fn
      self$args <- args
      self$name <- name

      # In this type of Transformation,
      # we need to specify potential group columns
      self$group_cols <- group_cols

      # Extract .apply generator for whether to apply the transformation
      if (".apply" %in% names(generators)){
        self$apply_generator <- generators[[".apply"]]
        generators <- generators[names(generators) != ".apply"]
      }

      # Assign generators
      self$generators <- generators
    },

    #' @description
    #'  Get arguments for a group.
    #' @return \code{list} of arguments (both constant and generated).
    get_group_args = function() {
      # Prepare arguments for a group
      generated_args <- self$generate_args()
      c(self$args, generated_args)
    },

    #' @description
    #'  Generate arguments for a group with the \code{`generators`}.
    #' @return \code{list} of generated arguments.
    #'
    #'  Does not include the constant arguments.
    generate_args = function() {
      # Call each generator to generate arg values for a single group
      lapply(self$generators, do.call, args = list())
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
        cat(indentation_str, "GeneratedTransformation: \n")
        indentation_str <- paste0(rep(" ", indent + 2), collapse = "")
      }

      # Print name of transformation
      cat(indentation_str, name, "\n", sep = "")

      # Print arguments
      args_str <- private$args_to_string(self$args)
      cat(indentation_str, "  Constant arguments:  ", args_str, "\n", sep = "")

      # Print generator functions
      generators_str <- private$args_to_string(self$generators, max_len = 30,
                                               rm_function_start = TRUE)
      cat(indentation_str, "  Generators:  ", generators_str, "\n", sep = "")

      # Print the apply generator function
      if (!is.null(self$apply_generator)){
        apply_generator_str <- private$args_to_string(self$apply_generator, max_len = 30,
                                                      rm_function_start = TRUE)
        cat(indentation_str, "  Apply generator:  ", apply_generator_str, "\n", sep = "")
      }

      # Print group columns
      if (!is.null(self$group_cols)){
        group_cols_str <- private$args_to_string(self$group_cols)
        cat(indentation_str, "  Grouping columns:  ", group_cols_str, "\n", sep = "")
      }

      # Return object invisibly to allow method chaining
      invisible(self)
    }
  ),
  private = list(
    # We don't use existing groupings in the given data frame
    ungroup_input = TRUE,
    apply_to_group = function(data, group_id) {
      if (dplyr::is_grouped_df(data)) {
        stop("`data` was grouped. Pass the group subset instead.")
      }

      # Whether to apply transformation to this group
      if (!is.null(self$apply_generator)){
        do_apply <- self$apply_generator()
        if (!checkmate::test_flag(do_apply)) {
          stop("The apply generator must return `TRUE` or `FALSE`.")
        }
        if (!isTRUE(do_apply)){
          # Do not apply transformation to data
          # Becomes identity function
          return(data)
        }
      }

      # Get arguments for group and apply function
      args <- self$get_group_args()
      args <- c(list(data = data), args)
      do.call(self$fn, args, envir = parent.frame())
    },
    check_initialize_args = function(fn, args, generators, name, group_cols) {
      super$check_initialize_args(fn = fn, args = args, name = name,
                                  group_cols = group_cols)
      assert_collection <- checkmate::makeAssertCollection()
      checkmate::assert_list(generators, names = "unique", types = "function", add = assert_collection)
      checkmate::reportAssertions(assert_collection)
      if (length(intersect(names(args), names(generators))) > 0) {
        assert_collection$push("`args` and `generators` contained the same names.")
      }
      checkmate::reportAssertions(assert_collection)
    }
  )
)

