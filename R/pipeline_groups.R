

#   __________________ #< b374fd49354d1eddc7d25f2e43608f7c ># __________________
#   Apply transformation differently per group                              ####

# TODO Perhaps add option to exclude a transformation from a group
# E.g. via a special arg in var_args or similar (.apply)

# Idea for example:
# agp <- ByGroupPipeline$new(3)
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
ByGroupTransformation <- R6::R6Class(
  "ByGroupTransformation",
  inherit = Transformation,
  public = list(
    var_args = NULL,
    num_groups = NULL,
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
      self$var_args <- var_args
      self$num_groups <- length(var_args[[1]])
    },
    get_group_args = function(group_id) {
      checkmate::assert_integerish(
        group_id,
        lower = 0,
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
      super$apply(data = data) # TODO check super uses overridden apply_to_group
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
        cat(indentation_str, "ByGroupTransformation: \n")
        indentation_str <- paste0(rep(" ", indent + 2), collapse = "")
      }

      cat(indentation_str, name, "\n", sep = "")
      args_str <- private$args_to_string(self$args)
      cat(indentation_str, "  Constant Arguments:  ", args_str, "\n", sep = "")
      var_args_str <- private$args_to_string(self$var_args)
      cat(indentation_str, "  Varying Arguments:   ", var_args_str, "\n", sep = "")
      invisible(self)
    }
  ),
  private = list(
    apply_to_group = function(data, group_id) {
      if (dplyr::is_grouped_df(data)) {
        stop("`data` was grouped. Pass the group subset instead.")
      }
      args <- self$get_group_args(group_id)
      args <- c(list(data = data), args)
      do.call(self$fn, args, envir = parent.frame())
    },
    check_initialize_args = function(fn, args, var_args, name) {
      super$check_initialize_args(fn = fn, args = args, name = name)
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
ByGroupPipeline <- R6::R6Class(
  "ByGroupPipeline",
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
      transformation <- ByGroupTransformation$new(
        fn = fn,
        args = args,
        var_args = var_args,
        name = name
      )
      if (transformation$num_groups != self$num_groups) {
        stop(
          "the transformation must have the same number of groups (see var_args) as the GroupedPipe."
        )
      }
      self$transformations <- c(self$transformations,
                                setNames(list(transformation), name))
    },
    print = function(...) {
      cat("ByGroupPipeline: \n")
      cat("  No. expected groups: ", self$num_groups, "\n", sep = "")
      for (name in self$names){
        print(self$transformations[[name]], indent = 2, show_class = FALSE)
      }
      invisible(self)
    }
  )
)
