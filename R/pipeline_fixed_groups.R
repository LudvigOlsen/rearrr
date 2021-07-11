

#   __________________ #< b374fd49354d1eddc7d25f2e43608f7c ># __________________
#   Apply transformation differently per group                              ####

# Describe how this pipeline requires grouping the data frame beforehand
# Also that the transformations are applied to groups separately
# why a function that requires group_cols (e.g. cluster_groups) will get an ungrouped data frame
# and one should set group_cols in the args

# Idea for example:
# agp <- FixedGroupsPipeline$new(3)
# agp$add_transformation(fn=rotate_2d, args=list(x_col="Index", y_col="A", origin=c(0,0),
#                                                suffix="", overwrite=TRUE),
#                        var_args = list(degrees=list(45, 90, 120)), name="rotate")
# agp$add_transformation(fn=cluster_groups, args=list(cols=c("Index","A"),
#                                                     suffix="", overwrite=TRUE),
#                        var_args = list(multiplier=list(0.05, 0.1, 0.2)), name="cluster")
# # agp$transformations
# agp$apply(dplyr::group_by(df, G), verbose=T)


# Reminder: Side-effect R6 methods should always return self invisibly!


# A transformation to be applied with
# different argument values to each group
FixedGroupsTransformation <- R6::R6Class(
  "FixedGroupsTransformation",
  inherit = Transformation,
  public = list(
    var_args = NULL,
    num_groups = NULL,
    apply_arg = NULL,
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
      if (".apply" %in% names(var_args)){
        self$apply_arg <- var_args[[".apply"]]
        if (checkmate::test_logical(self$apply_arg, any.missing = FALSE)){
          stop("The apply argument must be a logical (a `TRUE`/`FALSE` per group).")
        }
        var_args <- var_args[names(var_args) != ".apply"]
      }
      self$var_args <- var_args
      self$num_groups <- length(var_args[[1]])
    },
    get_group_args = function(group_id) {
      checkmate::assert_integerish(
        group_id,
        lower = 1,
        upper = self$num_groups,
        any.missing = FALSE,
        len = 1
      )
      var_args <- sapply(self$var_args, `[[`, group_id)
      var_args <- setNames(var_args, names(self$var_args))
      c(self$args, var_args)
    },
    apply = function(data) {
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

      if (isTRUE(show_class)){
        cat(indentation_str, "FixedGroupsTransformation: \n")
        indentation_str <- paste0(rep(" ", indent + 2), collapse = "")
      }

      cat(indentation_str, name, "\n", sep = "")
      args_str <- private$args_to_string(self$args)
      cat(indentation_str, "  Constant arguments:  ", args_str, "\n", sep = "")
      var_args_str <- private$args_to_string(self$var_args)
      cat(indentation_str, "  Varying arguments:   ", var_args_str, "\n", sep = "")
      if (!is.null(self$apply_arg)){
        group_ids <- seq_len(self$num_groups)
        dont_apply_to <- group_ids[!unlist(self$apply_arg)]
        dont_apply_to_str <- private$args_to_string(dont_apply_to, rm_capitalized_l = TRUE)
        cat(indentation_str, "  Don't apply to these groups:  ", dont_apply_to_str, "\n", sep = "")
      }
      invisible(self)
    }
  ),
  private = list(
    ungroup_input = FALSE,
    apply_to_group = function(data, group_id) {
      if (dplyr::is_grouped_df(data)) {
        stop("`data` was grouped. Pass the group subset instead.")
      }
      if (!is.null(self$apply_arg) &&
          !isTRUE(self$apply_arg[[group_id]])){
        # If the transformation should not be applied to this group
        # It becomes an identity function
        return(data)
      }
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

# Applies one transformation at a time
# With different arguments per group
FixedGroupsPipeline <- R6::R6Class(
  "FixedGroupsPipeline",
  inherit = Pipeline,
  public = list(
    num_groups = NULL,
    initialize = function(num_groups) {
      self$num_groups <- num_groups
    },
    add_transformation = function(fn, args, var_args, name) {
      if (name %in% self$names) {
        stop(paste0("the `name`, ", name, ", already exists. Names must be unique."))
      }
      self$names <- append(self$names, name)
      transformation <- FixedGroupsTransformation$new(
        fn = fn,
        args = args,
        var_args = var_args,
        name = name
      )
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
    },
    apply = function(data, verbose = FALSE) {
      if (dplyr::n_groups(data) != self$num_groups){
        stop(paste0("`data` did not have exactly ", self$num_groups, " groups as expected."))
      }
      super$apply(data = data, verbose = verbose)
    },
    print = function(...) {
      cat("FixedGroupsPipeline: \n")
      cat("  No. expected groups: ", self$num_groups, "\n", sep = "")
      for (name in self$names){
        print(self$transformations[[name]], indent = 2, show_class = FALSE)
      }
      invisible(self)
    }
  ),
  private = list(
    warn_grouped_input = FALSE
  )
)
