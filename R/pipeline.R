

#   __________________ #< 48020236dfd0dbd89a97a74647216c72 ># __________________
#   Basic pipeline                                                          ####


# NOTE: Any R6 method called for its side effects should return invisible(self).

# Applies one transformation at a time
# With the same arguments for all groups

#' @title Chain multiple transformations
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Build a pipeline of transformations to be applied sequentially.
#'
#'  Uses the same arguments for all groups in \code{`data`}.
#'
#'  Groupings are reset between each transformation. See `group_cols`.
#'
#'  \strong{Standard workflow}: Instantiate pipeline -> Add transformations -> Apply to data
#'
#'  To apply different argument values to each group, see
#'  \code{\link[rearrr:GeneratedPipeline]{GeneratedPipeline}} for generating
#'  argument values for an arbitrary number of groups and
#'  \code{\link[rearrr:FixedGroupsPipeline]{FixedGroupsPipeline}} for specifying
#'  specific values for a fixed set of groups.
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
#' pipe <- Pipeline$new()
#'
#' # Add 2D rotation transformation
#' # Note that we specify the grouping via `group_cols`
#' pipe$add_transformation(
#'   fn = rotate_2d,
#'   args = list(
#'     x_col = "Index",
#'     y_col = "A",
#'     origin = c(0, 0),
#'     degrees = 45,
#'     suffix = "",
#'     overwrite = TRUE
#'   ),
#'   name = "Rotate",
#'   group_cols = "G"
#' )
#'
#' # Add the `cluster_group` transformation
#' # Note that this function requires the entire input data
#' # to properly scale the groups. We therefore specify `group_cols`
#' # as part of `args`. This works as `cluster_groups()` accepts that
#' # argument.
#' pipe$add_transformation(
#'   fn = cluster_groups,
#'   args = list(
#'     cols = c("Index", "A"),
#'     suffix = "",
#'     overwrite = TRUE,
#'     multiplier = 0.05,
#'     group_cols = "G"
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
Pipeline <- R6::R6Class(
  "Pipeline",
  public = list(

    #' @field transformations \code{list} of transformations to apply.
    transformations = list(),

    #' @field names Names of the transformations.
    names = character(),

    #' @description
    #'  Add a transformation to the pipeline.
    #' @param fn Function that performs the transformation.
    #' @param args Named \code{list} with arguments for the \code{`fn`} function.
    #' @param name Name of the transformation step. Must be unique.
    #' @param group_cols Names of the columns to group the input
    #'  data by before applying the transformation.
    #'
    #'   Note that the transformation function is applied separately to each group (subset).
    #'   If the \code{`fn`} function requires access to the entire \code{data.frame}, the
    #'   grouping columns should be specified as part of \code{`args`} and
    #'   handled by the \code{`fn`} function.
    #' @return The pipeline. To allow chaining of methods.
    add_transformation = function(fn, args, name, group_cols = NULL){
      if (name %in% self$names){
        stop(paste0("the `name`, ", name, ", already exists. Names must be unique."))
      }
      # Add transformation to pipeline
      self$names <- append(self$names, name)
      transformation <- Transformation$new(fn = fn, args = args,
                                           name = name, group_cols = group_cols)
      self$transformations <- c(self$transformations, setNames(list(transformation), name))

      # Return object invisibly to allow method chaining
      invisible(self)
    },

    #' @description
    #'  Apply the pipeline to a \code{data.frame}.
    #' @param data \code{data.frame}.
    #'
    #'  A grouped \code{data.frame} will raise a warning and the grouping will be ignored.
    #'  Use the \code{`group_cols`} argument in the \code{`add_transformation`} method to
    #'  specify how \code{`data`} should be grouped for each transformation.
    #' @param verbose Whether to print the progress.
    #' @return Transformed version of \code{`data`}.
    apply = function(data, verbose = FALSE) {
      # Warn if `data` is grouped and that grouping would be ignored
      if (isTRUE(private$warn_grouped_input) && dplyr::is_grouped_df(data)){
        warning(
          "Ignoring groups in `data`. Only the `group_cols` grouping specifications are used."
        )
      }
      # Print start of process
      if (isTRUE(verbose)){
        cat(paste0("\n", paste0(rep("-", 54), collapse = "")))
        cat("\nApplying transformations.")
        start_time <- proc.time()
      }

      # Apply each of the transformations sequentially
      # TODO benchmark if another looping function with env assignment
      # is faster with e.g. 5-10 transformations
      for (name in self$names){
        # Print start of transformation
        if (isTRUE(verbose)){
          cat(paste0("\nStarting: ", name))
          transf_start_time <- proc.time()
        }

        # Apply transformation
        data <- self$transformations[[name]]$apply(data = data)

        # Print running time of the transformation
        if (isTRUE(verbose)){
          transf_end_time <- proc.time()
          cat(paste0(
            "\nEnded: ", name,
            " | Took ", format_running_time_(transf_start_time, transf_end_time), "."
          ))
        }
      }
      # Print end of finish with running time etc.
      if (isTRUE(verbose)){
        end_time <- proc.time()
        cat(paste0(
          "\nFinished applying transformations. Total time: ",
          format_running_time_(start_time, end_time), "."
        ))
        cat(paste0("\n", paste0(rep("-", 54), collapse = ""), "\n"))
      }

      # Return transformed data frame
      data
    },

    #' @description
    #'  Print an overview of the pipeline.
    #' @param ... further arguments passed to or from other methods.
    #' @return The pipeline. To allow chaining of methods.
    print = function(...) {
      cat("Pipeline: \n")
      for (name in self$names){
        print(self$transformations[[name]], indent = 2, show_class = FALSE)
      }
      # Return object invisibly to allow method chaining
      invisible(self)
    }
  ),
  private = list(
    # Groupings in the input data will be ignored
    # We instead rely on `group_cols` specifications
    warn_grouped_input = TRUE
  )
)


##  .................. #< 79fc422a3b7326a8c27f242052fbd9c1 ># ..................
##  Transformation class                                                    ####


# A transformation to be applied with
# different argument values to each group

#' @title Transformation
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Container for the type of transformation used in
#'  \code{\link[rearrr:Pipeline]{Pipeline}}.
#'
#'  \strong{Note}: For internal use.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family transformation classes
Transformation <- R6::R6Class(
  "Transformation",
  public = list(

    #' @field name Name of transformation.
    name = NULL,

    #' @field fn Transformation function.
    fn = NULL,

    #' @field args \code{list} of arguments for \code{`fn`}.
    args = NULL,

    #' @field group_cols Names of columns to group \code{data.frame}
    #'  by before applying \code{`fn`}.
    #'
    #'  When \code{`NULL`}, the \code{data.frame} is not grouped.
    group_cols = NULL,

    #' @description
    #'  Initialize transformation.
    #' @param fn Transformation function.
    #' @param args \code{list} of arguments for \code{`fn`}.
    #' @param name Name of transformation.
    #' @param group_cols Names of columns to group \code{data.frame}
    #'  by before applying \code{`fn`}.
    #'
    #'  When \code{`NULL`}, the \code{data.frame} is not grouped.
    initialize = function(fn, args, name = NULL, group_cols = NULL) {
      # Check arguments
      private$check_initialize_args(fn = fn, args = args, name = name,
                                    group_cols = group_cols)

      # Assign to object
      self$fn <- fn
      self$args <- args
      self$name <- name

      # In this type of Transformation,
      # we need to specify potential group columns
      self$group_cols <- group_cols
    },

    #' @description
    #'  Apply the transformation to a \code{data.frame}.
    #' @param data \code{data.frame}.
    #'
    #'  A grouped \code{data.frame} will first be ungrouped. If \code{`group_cols`} is specified,
    #'  it will then be grouped by those columns.
    #' @return Transformed version of \code{`data`}.
    apply = function(data) {
      # Ungroup data frame if specified
      if (isTRUE(private$ungroup_input)){
        data <- dplyr::ungroup(data)
      }
      # Group data frame if given some group column names
      if (!is.null(self$group_cols)){
        data <- dplyr::group_by(data, !!!rlang::syms(self$group_cols))
      }
      # Apply transformation to each group
      run_by_group(
        data = data,
        fn = private$apply_to_group,
        restore_grouping = dplyr::is_grouped_df(data)
      )
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
        cat(indentation_str, "Transformation: \n")
        indentation_str <- paste0(rep(" ", indent + 2), collapse = "")
      }

      # Print name of transformation
      cat(indentation_str, name, "\n", sep = "")

      # Print arguments
      args_str <- private$args_to_string(self$args)
      cat(indentation_str, "  Arguments:  ", args_str, "\n", sep = "")

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

      # Prepare arguments and apply function
      args <- c(list(data = data), self$args)
      do.call(self$fn, args, envir = parent.frame())
    },
    check_initialize_args = function(fn, args, name, group_cols){
      assert_collection <- checkmate::makeAssertCollection()
      checkmate::assert_function(fn, add = assert_collection)
      checkmate::assert_list(args, names = "unique", add = assert_collection)
      checkmate::assert_string(name, null.ok = TRUE, add = assert_collection)
      checkmate::assert_character(group_cols, null.ok = TRUE, min.chars = 1,
                                  any.missing = FALSE, add = assert_collection)
      checkmate::reportAssertions(assert_collection)
    },
    # Convert list of arguments to single string for print methods
    args_to_string = function(args, max_len = 20, rm_function_start = FALSE, rm_capitalized_l = FALSE) {
      arg_names <- names(args)
      arg_vals_strings <- lapply(
        args,
        clean_arg_str,
        max_len = max_len,
        rm_function_start = rm_function_start,
        rm_capitalized_l = rm_capitalized_l
      )
      if (length(arg_names) == 0){
        string <- paste0(arg_vals_strings, collapse = ", ")
      } else {
        string <- paste0(arg_names, "=", arg_vals_strings, collapse = ", ")
      }
      string
    }
  )
)


##  .................. #< b86e070947c65180cf4882eb350888c6 ># ..................
##  Utilities                                                               ####


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
clean_arg_str <- function(string, max_len = 20, rm_function_start = FALSE, rm_capitalized_l = FALSE) {
  # Deparse the strings and make sure split strings are collapsed
  string <- collapse_strings(deparse(string))
  # Reduce multiple consecutive whitespaces to a single whitespace
  string <- gsub("[[:blank:]]+", " ", string)
  # Remove trailing whitespaces
  string <- trimws(string)
  # Compress function string
  string <- gsub("function[[:space:]]?\\([[:space:]]?", "function(", string)
  string <- gsub("[[:space:]]?\\) \\{[[:space:]]*", "){", string)
  string <- gsub("\\{[[:space:]]*", "{", string)
  string <- gsub("[[:space:]]*\\}", "}", string)
  if (isTRUE(rm_function_start)){
    string <- gsub("function\\(\\)", "", string)
  }
  if (isTRUE(rm_capitalized_l)){
    string <- gsub("L", "", string, fixed = TRUE)
  }
  # Shorten long strings
  if (!is.null(max_len) && nchar(string) > max_len + 3) {
    string <- substr(string, 1, max_len)
    string <- paste0(string, "...")
  }
  string
}
