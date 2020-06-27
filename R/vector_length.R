

#   __________________ #< 0dd0b136804bbb4f4767bd7daa2fcba1 ># __________________
#   Vector length                                                           ####


#' @title Calculate vector length(s)
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Calculates vector lengths/magnitudes \emph{row-wise} or \emph{column-wise} with
#'  \deqn{sqrt(sum(x^2))}
#'  Where \eqn{x} is the \code{vector} to get the length/magnitude of.
#'
#'  Should not be confused with \code{\link[base:length]{length()}}, which counts the elements.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @param cols Names of columns in \code{`data`} to measure vector length of.
#' @param by_row Whether to measure length of row vectors instead of column vectors. (Logical)
#'
#'  Note: Disable when \code{`data`} is a \code{vector}.
#' @param len_col_name Name of new column with the row vector lengths when
#'  \code{`data`} is a \code{data.frame} and \code{`by_row`} is \code{TRUE}.
#' @export
#' @inheritParams multi_mutator
#' @family measuring functions
#' @return Vector length(s).
#'
#'  When \code{`data`} is a \code{vector}: \code{scalar}
#'
#'  When \code{`data`} is a \code{data.frame} and \code{`by_row`} is \code{TRUE}:
#'  \code{`data`} with an extra column with row vector lengths.
#'
#'  When \code{`data`} is a \code{data.frame} and \code{`by_row`} is \code{FALSE}:
#'  A \code{data.frame} with the summarized column vector lengths.
#' @examples
#' \donttest{
#' # Attach packages
#' library(rearrr)
#' library(dplyr)
#'
#' # Set seed
#' set.seed(1)
#'
#' # Create a data frame
#' df <- data.frame(
#'   "x" = runif(20),
#'   "y" = runif(20),
#'   "g" = rep(1:4, each=5)
#' )
#'
#' # Measure row-wise
#' vector_length(df, cols = c("x", "y"), by_row = TRUE)
#'
#' # Measure column-wise
#' vector_length(df, cols = c("x", "y"), by_row = FALSE)
#'
#' # By groups in 'g'
#' df %>%
#'   dplyr::group_by(g) %>%
#'   vector_length(cols = c("x", "y"), by_row = FALSE)
#'
#' # Measure vector length of a vector
#' vector_length(c(1:10))
#' }
vector_length <- function(data,
                          cols = NULL,
                          by_row = is.data.frame(data),
                          len_col_name = ".vec_len") {
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_flag(by_row, add = assert_collection)
  checkmate::assert_string(len_col_name, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  if (isTRUE(by_row) && is.null(len_col_name)) {
    assert_collection$push("when 'by_row' is 'TRUE', 'len_col_name' must be specified.")
  }
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Mutate with each multiplier
  multi_mutator(
    data = data,
    mutate_fn = vector_length_mutator_method,
    check_fn = NULL,
    cols = cols,
    suffix = "",
    force_df = FALSE,
    keep_original = TRUE,
    by_row = by_row,
    len_col_name = len_col_name
  )

}

vector_length_mutator_method <- function(data,
                                         cols,
                                         by_row,
                                         len_col_name,
                                         ...) {

  dim_vectors <- as.list(data[, cols, drop = FALSE])

  if (isTRUE(by_row)) {
    vec_lengths <- vec_lengths_rowwise(dim_vectors)
  } else {
    # Column summaries
    vec_lengths <- vec_lengths_colwise(dim_vectors)
    return(setNames(vec_lengths, cols))
  }

  # Add lengths to 'data'
  data[[len_col_name]] <- vec_lengths

  data

}


vec_lengths_rowwise <- function(dim_vectors) {
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_list(dim_vectors,
                         any.missing = FALSE,
                         types = "numeric",
                         add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  if (!all(length(dim_vectors[[1]]) == lengths(dim_vectors))) {
    assert_collection$push("all 'dim_vectors' must have the same length.")
  }
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  purrr::transpose(dim_vectors) %>%
    purrr::simplify_all() %>%
    purrr::map_dbl(vec_length) %>%
    purrr::simplify()
}

vec_lengths_colwise <- function(dim_vectors) {
  checkmate::assert_list(dim_vectors, any.missing = FALSE, types = "numeric")
  if (!all(length(dim_vectors[[1]]) == lengths(dim_vectors))) {
    stop("All 'dim_vectors' must have the same length.")
  }
  dim_vectors %>%
    purrr::map(vec_length)
}

vec_length <- function(x) {
  sqrt(sum(x ^ 2))
}