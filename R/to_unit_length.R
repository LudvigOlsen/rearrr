

#   __________________ #< 0dd0b136804bbb4f4767bd7daa2fcba1 ># __________________
#   To unit length                                                          ####


#' @title Scale to unit length
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Scales the vectors to unit length \emph{row-wise} or \emph{column-wise}.
#'
#'  The \code{*_vec()} version take and return a \code{vector}.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @param cols Names of columns in \code{`data`} to scale.
#' @param by_row Whether to scale row \code{vector}s instead of column \code{vector}s. (Logical)
#'
#'  Note: Disable when \code{`data`} is a \code{vector}.
#' @export
#' @inheritParams multi_mutator_
#' @family scaling functions
#' @return Scaled \code{vector} or \code{data.frame} (\code{tibble}) with the scaled columns.
#' @examples
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
#'   "g" = rep(1:4, each = 5)
#' )
#'
#' # Scale row-wise
#' to_unit_length(df, cols = c("x", "y"), by_row = TRUE)
#'
#' # Scale column-wise
#' to_unit_length(df, cols = c("x", "y"), by_row = FALSE)
#'
#' # Overwrite columns
#' to_unit_length(df, cols = c("x", "y"), suffix = "", overwrite = TRUE)
#'
#' # By groups in 'g'
#' df %>%
#'   dplyr::group_by(g) %>%
#'   to_unit_length(cols = c("x", "y"), by_row = FALSE)
#'
#' # Scale a vector
#' to_unit_length_vec(c(1:10))
#' to_unit_length(c(1:10), suffix = "", overwrite = TRUE)
#' vector_length(to_unit_length_vec(c(1:10)))
to_unit_length <- function(data,
                           cols = NULL,
                           by_row = is.data.frame(data),
                           suffix = ifelse(isTRUE(by_row), "_row_unit", "_col_unit"),
                           overwrite = FALSE) {
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_flag(by_row, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Mutate with each multiplier
  multi_mutator_(
    data = data,
    mutate_fn = to_unit_length_mutator_method_,
    check_fn = NULL,
    cols = cols,
    suffix = suffix,
    overwrite = overwrite,
    force_df = FALSE,
    keep_original = TRUE,
    by_row = by_row
  )
}

#' @rdname to_unit_length
#' @export
to_unit_length_vec <- function(data){
  checkmate::assert_numeric(data)
  to_unit_length(
    data = data,
    suffix = "",
    overwrite = TRUE
  )
}

to_unit_length_mutator_method_ <- function(data,
                                           grp_id,
                                           cols,
                                           overwrite,
                                           by_row,
                                           suffix,
                                           ...) {

  dim_vectors <- as.list(data[, cols, drop = FALSE])

  if (isTRUE(by_row)) {
    unit_dim_vectors <- to_unit_lengths_rowwise_(dim_vectors)
  } else {
    unit_dim_vectors <- to_unit_lengths_colwise_(dim_vectors)
  }

  # Add dim_vectors as columns with the suffix
  data <- add_dimensions_(
    data = data,
    new_vectors = setNames(unit_dim_vectors, cols),
    suffix = suffix,
    overwrite = overwrite
  )

  data
}

# Normalize vector to Unit length
to_unit_vector_ <- function(x) {
  checkmate::assert_numeric(x, any.missing = FALSE)
  if (sum(x^2) == 0) {
    return(x)
  }
  x / sqrt(sum(x^2))
}

# Normalize dimensions
# One vector per dimension
to_unit_lengths_rowwise_ <- function(dim_vectors) {
  checkmate::assert_list(dim_vectors, any.missing = FALSE, types = "numeric")
  if (!all(length(dim_vectors[[1]]) == lengths(dim_vectors))) {
    stop("All 'dim_vectors' must have the same length.")
  }
  purrr::transpose(dim_vectors) %>%
    purrr::simplify_all() %>%
    purrr::map(to_unit_vector_) %>%
    purrr::transpose() %>%
    purrr::simplify_all()
}

to_unit_lengths_colwise_ <- function(dim_vectors) {
  checkmate::assert_list(dim_vectors, any.missing = FALSE, types = "numeric")
  if (!all(length(dim_vectors[[1]]) == lengths(dim_vectors))) {
    stop("All 'dim_vectors' must have the same length.")
  }
  dim_vectors %>%
    purrr::map(to_unit_vector_)
}
