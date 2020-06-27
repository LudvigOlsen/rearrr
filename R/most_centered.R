

#   __________________ #< c3a51e3301fd0446cdd35cb99e5740a3 ># __________________
#   most_centered                                                           ####

#' @title Find the coordinates for data point closest to the centroid
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Returns the coordinates for the
#'  data point with the shortest distance to the centroid.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @param ... Numeric vectors.
#' @param na.rm Whether to ignore missing values when calculating centroid coordinates. (Logical)
#' @export
#' @return \code{vector} with the coordinates for the data point closest to the centroid.
#' @examples
#' \donttest{
#' # Attach packages
#' library(rearrr)
#'
#' # Set seed
#' set.seed(1)
#'
#' # Create three vectors
#' x <- runif(10)
#' y <- runif(10)
#' z <- runif(10)
#'
#' # Find coordinates of data point
#' # closest to the centroid
#' most_centered(x, y, z)
#'
#' # Compare to centroid coordinates
#' centroid(x, y, z)
#' }
most_centered <- function(..., na.rm = FALSE) {
  # Convert inputs to list
  dim_vectors <- list(...)

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_list(
    dim_vectors,
    types = "numeric",
    min.len = 1,
    .var.name = "list(...)",
    add = assert_collection
  )
  checkmate::assert_flag(na.rm, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  if (length(unique(lengths(dim_vectors))) != 1) {
    assert_collection$push("all vectors in '...' must have the same length.")
  }
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Calculate distances to the centroid
  distances <- calculate_distances(dim_vectors = dim_vectors,
                                   to = centroid(..., na.rm = na.rm))

  # Return coordinates of the data point with the shortest distance
  purrr::transpose(dim_vectors) %>%
    purrr::simplify_all() %>%
    .[[which.min(distances)]]
}
