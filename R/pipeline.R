
# TODO Add way to save pipeline to disk?

# NOTE: Any R6 method called for its side effects should return invisible(self).

# A transformation to be applied with
# different argument values to each group
Transformation <- R6::R6Class(
  "Transformation",
  public = list(
    name = NULL,
    fn = NULL,
    args = NULL,
    initialize = function(fn, args, name = NULL) {
      # Check arguments
      private$check_initialize_args(fn = fn, args = args, name = name)

      # Assign to object
      self$fn <- fn
      self$args <- args
      self$name <- name
    },
    apply = function(data) {
      run_by_group(
        data = data,
        fn = private$apply_to_group,
        restore_grouping = TRUE
      )
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
        cat(indentation_str, "Transformation: \n")
        indentation_str <- paste0(rep(" ", indent + 2), collapse = "")
      }

      cat(indentation_str, name, "\n", sep = "")
      args_str <- private$args_to_string(self$args)
      cat(indentation_str, "  Arguments:  ", args_str, "\n", sep = "")
      invisible(self)
    }
  ),
  private = list(
    apply_to_group = function(data, group_id) {
      if (dplyr::is_grouped_df(data)) {
        stop("`data` was grouped. Pass the group subset instead.")
      }
      args <- c(list(data = data), self$args)
      do.call(self$fn, args, envir = parent.frame())
    },
    check_initialize_args = function(fn, args, name){
      assert_collection <- checkmate::makeAssertCollection()
      checkmate::assert_function(fn, add = assert_collection)
      checkmate::assert_list(args, names = "unique", add = assert_collection)
      checkmate::assert_string(name, null.ok = TRUE, add = assert_collection)
      checkmate::reportAssertions(assert_collection)
    },
    args_to_string = function(args){
      arg_names <- names(args)
      arg_vals_strings <- lapply(args, clean_arg_str)
      paste0(arg_names, "=", arg_vals_strings, collapse = ", ")
    }
  )
)

# Applies one transformation at a time
# With the same arguments for all groups
Pipeline <- R6::R6Class(
  "Pipeline",
  public = list(
    transformations = list(),
    names = character(),
    add_transformation = function(fn, args, name){
      if (name %in% self$names){
        stop(paste0("the `name`, ", name, ", already exists. Names must be unique."))
      }

      self$names <- append(self$names, name)
      transformation <- Transformation$new(fn = fn, args = args, name = name)
      self$transformations <- c(self$transformations, setNames(list(transformation), name))
    },
    apply = function(data, verbose = FALSE) {
      if (isTRUE(verbose)){
        cat(paste0("\n", paste0(rep("-", 54), collapse = "")))
        cat("\nApplying transformations.")
        start_time <- proc.time()
      }

      # TODO benchmark if another looping function with env assignment
      # is faster with e.g. 5-10 transformations
      for (name in self$names){
        if (isTRUE(verbose)){
          cat(paste0("\nStarting: ", name))
          transf_start_time <- proc.time()
        }

        # Apply transformation
        data <- self$transformations[[name]]$apply(data = data)

        if (isTRUE(verbose)){
          transf_end_time <- proc.time()
          cat(paste0(
            "\nEnded: ", name,
            " | Took ", format_running_time_(transf_start_time, transf_end_time), "."
          ))
        }
      }
      if (isTRUE(verbose)){
        end_time <- proc.time()
        cat(paste0(
          "\nFinished applying transformations. Total time: ",
          format_running_time_(start_time, end_time), "."
        ))
        cat(paste0("\n", paste0(rep("-", 54), collapse = ""), "\n"))
      }

      data
    },
    print = function(...) {
      cat("Pipeline: \n")
      for (name in self$names){
        print(self$transformations[[name]], indent = 2, show_class = FALSE)
      }
      invisible(self)
    }
))

format_running_time_ <- function(t_start, t_end, digits = 4, suffix = "s") {
  t_total <- t_end[["elapsed"]] - t_start[["elapsed"]]
  t_total <- round(t_total, digits = digits)
  paste0(t_total, suffix)
}

# Collapse list of strings once
collapse_strings <- function(strings) {
  if (length(strings) > 1) {
    strings <- paste0(strings, collapse = "")
  }
  strings
}


# This is applied to each argument value separately
clean_arg_str <- function(string, max_len = 20) {
  # Deparse the strings and make sure split strings are collapsed
  string <- collapse_strings(deparse(string))
  # Reduce multiple consecutive whitespaces to a single whitespace
  string <- gsub("[[:blank:]]+", " ", string)
  # Remove trailing whitespaces
  string <- trimws(string)
  # Shorten long strings
  if (!is.null(max_len) && nchar(string) > max_len + 3) {
    string <- substr(string, 1, max_len)
    string <- paste0(string, "...")
  }
  string
}
