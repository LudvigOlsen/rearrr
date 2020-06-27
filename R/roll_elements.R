




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
#'  which changes the \code{value} of the elements and wraps to a given range.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @param data \code{vector} or \code{data.frame} to roll elements of. When a \code{data.frame} is
#'  grouped, the rolling is applied group-wise.
#' @param cols Names of columns to roll in \code{`data`}. If \code{NULL}, all columns are rolled.
#'
#'  \strong{N.B.} only used when \code{`data`} is a \code{data.frame}.
#' @param n Number of positions to roll. A positive number rolls \code{`x`} \emph{left}.
#'  A negative number rolls \code{`x`} \emph{right}.
#' @param n_fn Function to find \code{`n`}. Useful when \code{`x`} is a grouped \code{data.frame}, where
#'  we want \code{`n`} to depend on the rows in the group. The entire subset is passed to the function,
#'  and it must return an integer-like scalar.
#'
#'  E.g. \code{function(x){round(median(x$v))}} would get the median of the \code{v} variable in the subset.
#' @param ... Extra arguments for \code{`n_fn`}.
#' @export
#' @return Rolled \code{`data`}.
#' @family roll functions
#' @family rearrange functions
#' @examples
#' \donttest{
#' # Attach packages
#' library(rearrr)
#' library(dplyr)
#'
#' # Roll vector left
#' roll_elements(1:10, n = 2)
#'
#' # Roll vector right
#' roll_elements(1:10, n = -2)
#'
#' # Roll vector left by median (rounded to 6)
#' roll_elements(1:10, n_fn = function(x){round(median(x))})
#'
#' # Pass extra arguments to 'n_fn' via '...'
#' roll_elements(
#'   1:10,
#'   n_fn = function(x, y){round(median(x)) + y},
#'   y = 2
#' )
#'
#' #
#' # Roll data.frame
#' #
#'
#' # Set seed
#  set.seed(1)
#'
#' # Create a data frame
#' df <- data.frame(
#'   "x" = 1:20,
#'   "y" = runif(20)*10,
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
#' # Roll rows by median in each group
#' roll_elements(
#'   df %>% dplyr::group_by(g),
#'   n_fn = function(x, y){round(median(x$y)) - 2},
#'   y = 2
#' )
#'
#' }
roll_elements <- function(data,
                          cols = NULL,
                          n = NULL,
                          n_fn = NULL,
                          n_col_name = ".n",
                          ...) {
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_number(n,
                           finite = TRUE,
                           null.ok = TRUE,
                           add = assert_collection)
  checkmate::assert_function(n_fn, null.ok = TRUE, add = assert_collection)
  checkmate::assert_string(n_col_name, null.ok = TRUE, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  if ((is.null(n) && is.null(n_fn)) ||
      (!is.null(n) && !is.null(n_fn))) {
    assert_collection$push("exactly one of {'n', 'n_fn'} must be specified.")
  }
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # If no rolling, just return data
  if (!is.null(n) && n == 0) {
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

  out <- rearranger(
    data = data,
    rearrange_fn = roll_elements_rearranger_method,
    check_fn = NULL,
    cols = cols,
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
                                      nm = ".index")
    }
    out <- out[order(out[[tmp_index_col]]), , drop = FALSE]
    out[[tmp_index_col]] <- NULL
  }

  out
}

#' @rdname roll_elements
roll_elements_vec <- function(data,
                              n = NULL,
                              n_fn = NULL,
                              ...) {
  checkmate::assert(checkmate::check_vector(data, strict = TRUE),
                    checkmate::check_factor(data))
  roll_elements(
    data = data,
    n = n,
    n_fn = n_fn,
    n_col_name = NULL,
    ...
  )
}

roll_elements_rearranger_method <- function(data,
                                            cols,
                                            n,
                                            n_fn,
                                            n_fn_args,
                                            n_col_name,
                                            inverse_direction) {
  # Initial check of n
  if (!is.null(n) && n == 0) {
    return(data)
  }

  # Number of dimensions
  # Each column is a dimension
  num_dims <- length(cols)

  # Convert columns to list of vectors
  dim_vectors <- as.list(data[, cols, drop = FALSE])

  # Find n
  n <- apply_coordinate_fn(
    dim_vectors = dim_vectors,
    coordinates = n,
    fn = n_fn,
    num_dims = num_dims,
    coordinate_name = "n",
    fn_name = "n_fn",
    dim_var_name = "cols",
    allow_len_one = TRUE,
    extra_args = n_fn_args
  )

  # Check n again
  if (n == 0) {
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

  # Add dim_vectors as columns with the suffix
  data <- add_dimensions(
    data = data,
    new_vectors = setNames(dim_vectors, cols),
    suffix = ""
  )

  if (!is.null(n_col_name)) {
    if (isTRUE(inverse_direction)) {
      n <- -1 * n
    }
    data[[n_col_name]] <- list_coordinates(n, cols)
  }

  data

}
