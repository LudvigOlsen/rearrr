
# Adding .apply=function(){sample(c(TRUE, FALSE), 1)} to generators
# means transformations would only include a transformation 50% of the time

# A transformation to be applied with different
# generated argument values to each group
GeneratedTransformation <- R6::R6Class(
  "GeneratedTransformation",
  inherit = Transformation,
  public = list(
    generators = NULL,
    apply_generator = NULL,
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
      self$generators <- generators
    },
    get_group_args = function() {
      generated_args <- self$generate_args()
      c(self$args, generated_args)
    },
    generate_args = function() {
      lapply(self$generators, do.call, args = list())
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
        cat(indentation_str, "GeneratedTransformation: \n")
        indentation_str <- paste0(rep(" ", indent + 2), collapse = "")
      }

      cat(indentation_str, name, "\n", sep = "")
      args_str <- private$args_to_string(self$args)
      cat(indentation_str, "  Constant arguments:  ", args_str, "\n", sep = "")
      generators_str <- private$args_to_string(self$generators, max_len = 30,
                                               rm_function_start = TRUE)
      cat(indentation_str, "  Generators:  ", generators_str, "\n", sep = "")
      if (!is.null(self$apply_generator)){
        apply_generator_str <- private$args_to_string(self$apply_generator, max_len = 30,
                                                      rm_function_start = TRUE)
        cat(indentation_str, "  Apply generator:  ", apply_generator_str, "\n", sep = "")
      }
      if (!is.null(self$group_cols)){
        group_cols_str <- private$args_to_string(self$group_cols)
        cat(indentation_str, "  Grouping columns:  ", group_cols_str, "\n", sep = "")
      }
      invisible(self)
    }
  ),
  private = list(
    ungroup_input = TRUE,
    apply_to_group = function(data, group_id) {
      if (dplyr::is_grouped_df(data)) {
        stop("`data` was grouped. Pass the group subset instead.")
      }
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

# Applies one transformation at a time
# With different generated argument values per group
GeneratedPipeline <- R6::R6Class(
  "GeneratedPipeline",
  inherit = Pipeline,
  public = list(
    add_transformation = function(fn, args, generators, name, group_cols = NULL) {
      if (name %in% self$names) {
        stop(paste0("the `name`, ", name, ", already exists. Names must be unique."))
      }

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
    },
    print = function(...) {
      cat("GeneratedPipeline: \n")
      for (name in self$names){
        print(self$transformations[[name]], indent = 2, show_class = FALSE)
      }
      invisible(self)
    }
  )
)
