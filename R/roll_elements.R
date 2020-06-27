


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
#' @return Rolled \code{`x`}.
#' @family roll functions
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
                          ...) {


  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_vector(data, strict = FALSE, add = assert_collection)
  checkmate::assert_character(
    cols,
    null.ok = TRUE,
    min.chars = 1,
    unique = TRUE,
    any.missing = FALSE,
    min.len = 1,
    add = assert_collection
  )
  checkmate::assert_number(n,
                           finite = TRUE,
                           null.ok = TRUE,
                           add = assert_collection)
  checkmate::assert_function(n_fn, null.ok = TRUE, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  if ((is.null(n) && is.null(n_fn)) ||
      (!is.null(n) && !is.null(n_fn))) {
    assert_collection$push("exactly one of {'n', 'n_fn'} must be specified.")
  }
  if (!is.data.frame(data) && !is.null(cols)) {
    assert_collection$push("when 'data' is not a data.frame, 'cols' should be NULL.")
  }
  checkmate::reportAssertions(assert_collection)
  if (!is.null(cols) && length(setdiff(cols, colnames(data))) > 0) {
    # TODO Make helper for this where n cols are shown and with ... when more were there
    assert_collection$push(paste0(
      "these names in 'cols' where not columns in 'data': ",
      head(setdiff(cols, colnames(data)), 3)
    ))
  }
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # If no rolling, just return data
  if (!is.null(n) && n == 0) {
    return(data)
  }

  if (is.data.frame(data)) {
    roll_elements_df(
      data = data,
      cols = cols,
      n = n,
      n_fn = n_fn,
      ...
    )
  } else if (is.vector(data) || is.factor(data)) {
    roll_elements_vector(data = data,
                         n = n,
                         n_fn = n_fn,
                         ...)
  } else {
    stop("'data' has unsupported type.")
  }
}

roll_elements_vector <- function(data, n, n_fn, ...) {
  n <- apply_n_fn(data = data,
                  n = n,
                  n_fn = n_fn,
                  ...)
  if (n == 0) {
    return(data)
  }
  c(tail(x = data, n = -n), head(x = data, n = n))
}

roll_elements_df <- function(data, cols, n, n_fn, ...) {
  if (!is.null(n) && n == 0) {
    return(data)
  }

  # Roll each subset of data
  run_by_group(
    data = data,
    fn = function(data) {
      # Apply n_fn
      n <- apply_n_fn(data = data,
                      n = n,
                      n_fn = n_fn,
                      ...)

      if (n == 0) {
        return(data)
      }

      # Roll indices
      inds <- roll_elements_vector(seq_len(nrow(data)), n = n, n_fn = NULL)

      # Get rolled values/rows
      if (!is.null(cols)) {
        data[, cols] <- data[inds, cols, drop = FALSE]
        data
      } else {
        data[inds, , drop = FALSE]
      }
    }
  )
}

apply_n_fn <- function(data, n, n_fn, ...) {
  if (!is.null(n_fn)) {
    n <- n_fn(data, ...)
    if (!checkmate::test_integerish(n, len = 1)) {
      stop(paste0(
        "output of 'n_fn' must be an integer-like scalar but was: ",
        paste0(deparse(n), collapse = "")
      ))
    }
  }
  n
}
