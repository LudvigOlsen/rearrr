

#   __________________ #< f7c7492857e04f8c2ccd8ed8f5fd56dd ># __________________
#   Roll elements                                                           ####


#' @title Roll elements
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Rolls positions of elements.
#'
#'  Example:
#'
#'  Rolling \code{c(1, 2, 3, 4, 5)} with \code{`n = 2`} becomes:
#'
#'  \code{c(3, 4, 5, 1, 2)}
#'
#'  \code{roll_elements_vec()} takes and returns a \code{vector}.
#'
#'  Should not be confused with \code{\link[rearrr:roll_values]{roll_values()}},
#'  which changes the \emph{values} of the elements and wraps to a given range.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @param data \code{vector} or \code{data.frame} to roll elements of. When a \code{data.frame} is
#'  grouped, the rolling is applied group-wise.
#' @param cols Names of columns in \code{`data`} to roll.
#'  If \code{NULL}, the \emph{index} is rolled and used to reorder \code{`data`}.
#'
#'  \strong{N.B.} only used when \code{`data`} is a \code{data.frame}.
#' @param n Number of positions to roll. A positive number rolls \emph{left/up}.
#'  A negative number rolls \emph{right/down}.
#' @param n_fn Function to find \code{`n`}. Useful when \code{`data`} is a
#'  grouped \code{data.frame} and \code{`n`} should depend on the rows in the group.
#'
#'  \strong{Input}: Each specified \code{vector}/\code{column} in \code{`data`}
#'  is passed to the function as a separate argument.
#'
#'  \strong{Output}: It should return either a \code{vector}
#'  with one \code{integer-like scalar} \emph{per column}
#'  or a single \code{integer-like scalar} to use for all columns.
#'
#'  Can be created with \code{\link[rearrr:create_n_fn]{create_n_fn()}}.
#'  See also \code{\link[rearrr:median_index]{median_index()}} and
#'  \code{\link[rearrr:median_index]{quantile_index()}}.
#' @param n_col_name Name of new column with the applied \code{`n`} values.
#'  If \code{NULL}, no column is added.
#' @param overwrite Whether to allow overwriting of columns with the
#'  same name as \code{`n_col_name`}. (Logical)
#' @param ... Extra arguments for \code{`n_fn`}.
#' @export
#' @return Rolled \code{`data`}.
#' @family roll functions
#' @family rearrange functions
#' @examples
#' # Attach packages
#' library(rearrr)
#' library(dplyr)
#'
#' # Roll vector left
#' roll_elements(1:10, n = 2)
#'
#' # Roll vector right and return the vector
#' roll_elements_vec(1:10, n = -2)
#'
#' # Roll vector left by median index (rounded to 6)
#' roll_elements(3:12, n_fn = median_index)
#'
#' # Roll vector right by median value (rounded to 8)
#' roll_elements(3:12, n_fn = create_n_fn(median, negate = TRUE))
#'
#' # Pass extra arguments (here 'prob') to 'n_fn' via '...'
#' roll_elements(
#'   1:10,
#'   n_fn = quantile_index,
#'   prob = 0.2
#' )
#'
#' #
#' # Roll data.frame
#' #
#'
#' # Set seed
#' set.seed(1)
#'
#' # Create a data frame
#' df <- data.frame(
#'   "x" = 1:20,
#'   "y" = runif(20) * 10,
#'   "g" = rep(1:4, each = 5)
#' )
#'
#' # Roll rows left/up
#' roll_elements(df, n = 2)
#'
#' # Roll rows right/down
#' roll_elements(df, n = -2)
#'
#' # Roll 'x' column right/down
#' roll_elements(df, cols = "x", n = -2)
#'
#' # Roll rows right by median index in each group
#' # Specify 'negate' for the 'median_index' function
#' roll_elements(
#'   df %>% dplyr::group_by(g),
#'   n_fn = median_index,
#'   negate = TRUE
#' )
roll_elements <- function(data,
                          cols = NULL,
                          n = NULL,
                          n_fn = NULL,
                          n_col_name = ".n",
                          overwrite = FALSE,
                          ...) {
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_numeric(n,
    any.missing = FALSE,
    finite = TRUE,
    null.ok = TRUE,
    add = assert_collection
  )
  checkmate::assert_function(n_fn, null.ok = TRUE, add = assert_collection)
  checkmate::assert_string(n_col_name, null.ok = TRUE, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  if ((is.null(n) && is.null(n_fn)) ||
    (!is.null(n) && !is.null(n_fn))) {
    assert_collection$push("exactly one of {'n', 'n_fn'} must be specified.")
  }
  checkmate::reportAssertions(assert_collection)
  check_unique_colnames_(cols, n_col_name)
  check_overwrite_(data = data, nm = n_col_name, overwrite = overwrite)
  # End of argument checks ####

  # If no rolling, just return data
  if (!is.null(n) && all(n == 0)) {
    return(data)
  }

  inverse_direction <- FALSE
  uses_tmp_index <- FALSE
  if (is.data.frame(data) && is.null(cols)) {
    # Roll rows
    tmp_index_col <- create_tmp_var(data)
    data[[tmp_index_col]] <- seq_len(nrow(data))
    cols <- tmp_index_col
    uses_tmp_index <- TRUE

    # For this to work, we have to inverse
    # the rolling direction
    inverse_direction <- TRUE
  }

  out <- rearranger_(
    data = data,
    rearrange_fn = roll_elements_rearranger_method_,
    check_fn = NULL,
    cols = cols,
    overwrite = overwrite,
    n = n,
    n_fn = n_fn,
    n_fn_args = rlang::exprs(...),
    n_col_name = n_col_name,
    inverse_direction = inverse_direction
  )

  if (isTRUE(uses_tmp_index)) {
    if (!is.null(n_col_name)) {
      out[[n_col_name]] <- purrr::map(out[[n_col_name]],
        .f = setNames,
        nm = ".index"
      )
    }
    out <- out[order(out[[tmp_index_col]]), , drop = FALSE]
    out[[tmp_index_col]] <- NULL
  }

  out
}

#' @rdname roll_elements
#' @export
roll_elements_vec <- function(data,
                              n = NULL,
                              n_fn = NULL,
                              ...) {
  checkmate::assert(
    checkmate::check_vector(data, strict = TRUE),
    checkmate::check_factor(data)
  )
  roll_elements(
    data = data,
    n = n,
    n_fn = n_fn,
    n_col_name = NULL,
    overwrite = TRUE,
    ...
  )
}

roll_elements_rearranger_method_ <- function(data,
                                             grp_id,
                                             cols,
                                             overwrite,
                                             n,
                                             n_fn,
                                             n_fn_args,
                                             n_col_name,
                                             inverse_direction,
                                             ...) {
  # Initial check of n
  if (!is.null(n) && all(n == 0)) {
    return(data)
  }

  # Number of dimensions
  # Each column is a dimension
  num_dims <- length(cols)

  # Convert columns to list of vectors
  dim_vectors <- as.list(data[, cols, drop = FALSE])

  # Find n
  n <- apply_coordinate_fn_(
    dim_vectors = dim_vectors,
    coordinates = n,
    fn = n_fn,
    num_dims = num_dims,
    coordinate_name = "n",
    fn_name = "n_fn",
    dim_var_name = "cols",
    grp_id = grp_id,
    allow_len_one = TRUE,
    extra_args = n_fn_args
  )

  # Check n again
  if (all(n == 0)) {
    return(data)
  }

  if (isTRUE(inverse_direction)) {
    n <- -1 * n
  }

  # Roll dimensions
  dim_vectors <-
    purrr::map2(.x = dim_vectors, .y = n, .f = ~ {
      c(tail(x = .x, n = -.y), head(x = .x, n = .y))
    })

  # Add dim_vectors as columns with no suffix
  data <- add_dimensions_(
    data = data,
    new_vectors = setNames(dim_vectors, cols),
    suffix = "",
    overwrite = TRUE
  )

  if (!is.null(n_col_name)) {
    if (isTRUE(inverse_direction)) {
      n <- -1 * n
    }
    # Add 'n' column
    data <- add_info_col_(
      data = data,
      nm = n_col_name,
      content = list_coordinates_(n, cols),
      overwrite = overwrite
    )
  }

  data
}
