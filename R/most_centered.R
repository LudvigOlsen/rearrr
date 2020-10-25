

#   __________________ #< c3a51e3301fd0446cdd35cb99e5740a3 ># __________________
#   most_centered                                                           ####

#' @title Find the coordinates for the data point closest to the centroid
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Returns the coordinates for the data point with the
#'  shortest distance to the \code{\link[rearrr:centroid]{centroid}}.
#'
#'  To get a logical vector (\code{TRUE}/\code{FALSE}) indicating
#'  whether a data point is the most centered,
#'  use \code{\link[rearrr:is_most_centered]{is_most_centered()}}.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @inheritParams apply_coord_fn_
#' @param na.rm Whether to ignore missing values. At least one data point must be complete. (Logical)
#' @family coordinate functions
#' @export
#' @return The coordinates for the data point closest to the centroid.
#'  Either as a \code{vector} or a \code{data.frame}.
#' @examples
#' # Attach packages
#' library(rearrr)
#' library(dplyr)
#'
#' # Set seed
#' set.seed(1)
#'
#' # Create three vectors
#' x <- runif(10)
#' y <- runif(10)
#' z <- runif(10)
#'
#' # Find coordinates of the data point
#' # closest to the centroid
#' most_centered(x, y, z)
#'
#' # Compare to centroid coordinates
#' centroid(x, y, z)
#'
#' #
#' # For data.frames
#' #
#'
#' # Create data frame
#' df <- data.frame(
#'   "x" = x,
#'   "y" = y,
#'   "z" = z,
#'   "g" = rep(1:2, each = 5)
#' )
#'
#' # Find coordinates of the data point
#' # closest to the centroid
#' most_centered(df, cols = c("x", "y", "z"))
#'
#' # When 'df' is grouped
#' df %>%
#'   dplyr::group_by(g) %>%
#'   most_centered(cols = c("x", "y", "z"))
#'
#' # Filter to only include most centered data points
#' df %>%
#'   dplyr::group_by(g) %>%
#'   dplyr::filter(is_most_centered(x, y, z))
most_centered <- function(..., cols = NULL, na.rm = FALSE) {
  most_centered_coord_fn <- function(...) {
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

    # Remove NAs or complain about them
    any_nas <- any(purrr::map_lgl(dim_vectors, .f = ~{any(is.na(.x))}))
    if (isTRUE(any_nas)){
      if (isTRUE(na.rm)){
        dim_vectors <- dim_vectors %>%
          purrr::transpose() %>%
          purrr::simplify_all() %>%
          purrr::discard(.p = ~{any(is.na(.x))}) %>%
          purrr::transpose() %>%
          purrr::simplify_all()

        if (length(dim_vectors) == 0){
          assert_collection$push("After removing missing values ('NA's), there were no data points left.")
        }

      } else {
        assert_collection$push("'...' contained missing values ('NA's).")
      }
    }
    checkmate::reportAssertions(assert_collection)
    # End of argument checks ####

    # Calculate distances to the centroid
    distances <- calculate_distances_(
      dim_vectors = dim_vectors,
      to = centroid(..., na.rm = na.rm)
    )

    if (any(is.na(distances))){
      stop("Calculated distances had missing values ('NA's).")
    }

    # Return coordinates of the data point with the shortest distance
    purrr::transpose(dim_vectors) %>%
      purrr::simplify_all() %>%
      .[[which.min(distances)]]
  }

  # Apply coordinate function
  apply_coord_fn_(
    ...,
    cols = cols,
    coord_fn = most_centered_coord_fn,
    fn_name = "most_centered_fn",
    coordinate_name = "most_centered"
  )
}


#' @title Find which data point is closest to the centroid
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Finds the data point with the
#'  shortest distance to the centroid.
#'
#'  To get the coordinates of the most centered data point,
#'  use \code{\link[rearrr:most_centered]{most_centered()}} instead.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @param ... \code{Numeric vector}s.
#' @inheritParams most_centered
#' @family coordinate functions
#' @export
#' @return Logical vector (\code{TRUE}/\code{FALSE}) indicating if a data point is the most centered.
#' @examples
#' \donttest{
#' # Attach packages
#' library(rearrr)
#' library(dplyr)
#'
#' # Set seed
#' set.seed(1)
#'
#' # Create three vectors
#' x <- runif(10)
#' y <- runif(10)
#' z <- runif(10)
#'
#' # Find the data point closest to the centroid
#' is_most_centered(x, y, z)
#'
#' # Compare to coordinates for the most centered
#' most_centered(x, y, z)
#'
#' #
#' # For data.frames
#' #
#'
#' # Create data frame
#' df <- data.frame(
#'   "x" = x,
#'   "y" = y,
#'   "z" = z,
#'   "g" = rep(1:2, each = 5)
#' )
#'
#' # Filter the data points
#' # closest to the centroid
#' df %>%
#'   dplyr::filter(is_most_centered(x, y, z))
#'
#' # When 'df' is grouped
#' df %>%
#'   dplyr::group_by(g) %>%
#'   dplyr::filter(is_most_centered(x, y, z))
#'
#' # Add as column
#' df %>%
#'   dplyr::group_by(g) %>%
#'   dplyr::mutate(mc = is_most_centered(x, y, z))
#' }
is_most_centered <- function(..., na.rm = FALSE){
  checkmate::assert_list(list(...), types = "numeric", any.missing = na.rm)
  dp_most_centered <- most_centered(..., na.rm = na.rm)
  list(...) %>%
    purrr::transpose() %>%
    purrr::simplify_all() %>%
    purrr::map(.f = ~{
      all(.x == dp_most_centered)
    }) %>%
    purrr::simplify()
}
