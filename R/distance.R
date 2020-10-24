

#   __________________ #< 60cfc78f594e5611a6eaaf34a2b212ae ># __________________
#   Calculate distances                                                     ####


#' @title Calculate the distance to an origin
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Calculates the distance to the specified origin with:
#'  \deqn{d(P1, P2) = sqrt( (x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2 + ... )}
#'
#'  The origin can be supplied as coordinates or as a function that returns coordinates. The
#'  latter can be useful when supplying a grouped \code{data.frame} and finding the distance to e.g. the centroid
#'  of each group.
#'
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @param cols Names of columns in \code{`data`} to measure distance in.
#'  Each column is considered a dimension.
#' @param origin Coordinates of the origin to calculate distances to.
#'  A scalar to use in all dimensions
#'  or a \code{vector} with one scalar per dimension.
#'
#'  \strong{N.B.} Ignored when \code{`origin_fn`} is not \code{NULL}.
#' @param distance_col_name Name of new column with the distances.
#' @param origin_col_name Name of new column with the origin coordinates. If \code{NULL}, no column is added.
#' @export
#' @return \code{data.frame} (\code{tibble}) with the additional columns (distances and origin coordinates).
#' @inheritParams multi_mutator_
#' @family measuring functions
#' @family distance functions
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
#' # Calculate distances in the two dimensions (x and y)
#' # With the origin at x=0.5, y=0.5
#' distance(
#'   data = df,
#'   cols = c("x", "y"),
#'   origin = c(0.5, 0.5)
#' )
#'
#' # Calculate distances to the centroid for each group in 'g'
#' distance(
#'   data = dplyr::group_by(df, g),
#'   cols = c("x", "y"),
#'   origin_fn = centroid
#' )
distance <- function(data,
                     cols = NULL,
                     origin = NULL,
                     origin_fn = NULL,
                     distance_col_name = ".distance",
                     origin_col_name = ".origin",
                     overwrite = FALSE) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_string(distance_col_name, add = assert_collection)
  checkmate::assert_string(origin_col_name, null.ok = TRUE, add = assert_collection)
  checkmate::assert_numeric(origin,
    min.len = 1,
    any.missing = FALSE,
    null.ok = TRUE,
    add = assert_collection
  )
  checkmate::assert_function(origin_fn, null.ok = TRUE, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  check_unique_colnames_(cols, distance_col_name, origin_col_name)
  check_overwrite_(data = data,
                   nm = distance_col_name,
                   overwrite = overwrite)
  check_overwrite_(data = data,
                   nm = origin_col_name,
                   overwrite = overwrite)
  # End of argument checks ####

  # Mutate with each multiplier
  multi_mutator_(
    data = data,
    mutate_fn = calculate_distances_mutator_method_,
    check_fn = NULL,
    cols = cols,
    overwrite = overwrite,
    force_df = TRUE,
    keep_original = TRUE,
    origin = origin,
    origin_fn = origin_fn,
    distance_col_name = distance_col_name,
    origin_col_name = origin_col_name
  )
}


calculate_distances_mutator_method_ <- function(data,
                                                grp_id,
                                                cols,
                                                overwrite,
                                                origin,
                                                origin_fn,
                                                distance_col_name,
                                                origin_col_name,
                                                ...) {

  # Number of dimensions
  # Each column is a dimension
  num_dims <- length(cols)

  # Convert columns to list of vectors
  dim_vectors <- as.list(data[, cols, drop = FALSE])

  # Find origin if specified
  origin <- apply_coordinate_fn_(
    dim_vectors = dim_vectors,
    coordinates = origin,
    fn = origin_fn,
    num_dims = length(cols),
    coordinate_name = "origin",
    fn_name = "origin_fn",
    dim_var_name = "cols",
    grp_id = grp_id,
    allow_len_one = TRUE
  )

  # Calculate distances
  # formula: sqrt( (x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2 )
  distances <-
    calculate_distances_(dim_vectors = dim_vectors, to = origin)

  # Add info columns
  if (!is.null(origin_col_name)) {
    data[[origin_col_name]] <- list_coordinates_(origin, cols)
  }
  data[[distance_col_name]] <- distances

  data
}


# Helper used in multiple places
calculate_distances_ <- function(dim_vectors, to) {

  # Distance formula:
  # d(P1, P2) = sqrt( (x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2 )

  # Calculate (x2-x1)^2, (y2-y1)^2, etc.
  distance_terms <-
    purrr::map2(.x = dim_vectors, .y = to, .f = ~ {
      (.x - .y)^2
    })

  # Calculate sqrt(sum(x,y,z))
  # of the squared differences dim-wise
  distances <- distance_terms %>%
    purrr::transpose() %>%
    purrr::simplify_all() %>%
    purrr::map_dbl(~ {
      sqrt(sum(.x))
    })

  distances
}
