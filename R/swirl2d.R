

#   __________________ #< 9d281822e7dafc7afc466a38db278b1e ># __________________
#   Swirl 2d                                                                ####


#' @title Swirl the values around an origin in 2 dimensions
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  The values are swirled counterclockwise around a specified origin.
#'  The swirling is done by rotating around the origin with the degrees based
#'  on the distances to the origin as so: \deqn{degrees = scale_fn(distances) / (2 * radius) * 360}
#'
#'  The origin can be supplied as coordinates or as a function that returns coordinates. The
#'  latter can be useful when supplying a grouped \code{data.frame} and swirling around e.g. the centroid
#'  of each group.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @param radius Radius of the most-inner swirl on the x-axis in the \emph{simplest} case.
#'  A negative number changes the direction to clockwise rotation.
#'  Can be a \code{vector} with multiple radiuses.
#'
#'  Note: With a custom \code{`scaling_fn`}, this might not be the actual swirl radius anymore. Think of
#'  it more as a width setting where a larger number leads to fewer full rotations.
#' @param x_col Name of x column in \code{`data`}. If \code{NULL} and \code{`data`} is a \code{vector},
#'  the index of \code{`data`} is used. If \code{`data`} is a \code{data.frame}, it must be specified.
#' @param y_col Name of y column in \code{`data`}. If \code{`data`} is a \code{data.frame}, it must be specified.
#' @param origin Coordinates of the origin to swirl around. Must be a \code{vector} with 2 elements (i.e. origin_x, origin_y).
#'  Ignored when \code{`origin_fn`} is not \code{NULL}.
#' @param origin_fn Function for finding the origin coordinates to swirl the values around.
#'  Each column will be passed as a \code{vector} (i.e. a \code{vector} with x-values and
#'  a \code{vector} with y-values).
#'  It should return a \code{vector} with one constant per dimension (i.e. origin_x, origin_y).
#'
#'  Can be created with \code{\link[rearrr:create_origin_fn]{create_origin_fn()}} if you want to apply
#'  the same function to each dimension.
#'
#'  E.g. the \code{\link[rearrr:centroid]{centroid()}} function, which is created with:
#'
#'  \code{create_origin_fn(mean)}
#'
#'  Which returns the following function:
#'
#'  \code{function(...)\{}
#'
#'  \verb{  }\code{list(...) \%>\%}
#'
#'  \verb{    }\code{purrr::map(mean) \%>\%}
#'
#'  \verb{    }\code{unlist(recursive = TRUE,}
#'
#'  \verb{           }\code{use.names = FALSE)}
#'
#'  \code{\}}
#' @param scale_fn Function for scaling the distances before calculating the degrees.
#'  Should take a \code{numeric vector} (the distances) as its only \emph{required} input and
#'  return a \code{numeric vector} (the scaled distances) of the same length. E.g.:
#'
#'  \code{function(d)\{}
#'
#'  \verb{  }\code{d ^ 1.5}
#'
#'  \code{\}}
#' @param origin_col_name Name of new column with the origin coordinates. If \code{NULL}, no column is added.
#' @param degrees_col_name Name of new column with the degrees. If \code{NULL}, no column is added.
#' @param radius_col_name Name of new column with the radius. If \code{NULL}, no column is added.
#' @export
#' @return \code{data.frame} (\code{tibble}) with three new columns containing the swirled x- and y-values and the degrees.
#' @family mutate functions
#' @family rotation functions
#' @inheritParams multi_mutator
#' @examples
#' \donttest{
#' # Attach packages
#' library(rearrr)
#' library(dplyr)
#' library(ggplot2)
#'
#' # Set seed
#' set.seed(4)
#'
#' # Create a data frame
#' df <- data.frame(
#'   "x" = 1:50,
#'   "y" = 1,
#'   "r1" = runif(50),
#'   "r2" = runif(50) * 35,
#'   "g" = rep(1:5, each=10)
#' )
#'
#' # Rotate values
#' swirl_2d(df, radius = 45, x_col = "x", y_col = "y")
#'
#' # Swirl around the centroid
#' df_swirled <- swirl_2d(
#'   data = df,
#'   radius = c(95, 96, 97, 98, 99, 100),
#'   x_col = "x",
#'   y_col = "y",
#'   origin_fn = centroid,
#'   scale_fn = function(x) {
#'     x ^ 1.6
#'   }
#' )
#'
#' df_swirled
#'
#' # Plot swirls
#' df_swirled %>%
#'   ggplot(aes(x = x_swirled, y = y_swirled, color = factor(.radius))) +
#'   geom_point() +
#'   theme_minimal() +
#'   labs(x = "x", y = "y", color = ".radius")
#'
#' #
#' # Swirl random data
#' # The trick lies in finding the right radius
#' #
#'
#' # Swirl the random columns
#' df_swirled <- swirl_2d(
#'   data = df,
#'   radius = 5,
#'   x_col = "r1",
#'   y_col = "r2",
#'   origin_fn = centroid
#' )
#'
#' # Plot swirls
#' df_swirled %>%
#'   ggplot(aes(x = r1_swirled, y = r2_swirled)) +
#'   geom_point() +
#'   theme_minimal() +
#'   labs(x = "r1", y = "r2")
#'
#' }
swirl_2d <- function(data,
                    radius,
                    x_col = NULL,
                    y_col = NULL,
                    suffix = "_swirled",
                    origin = c(0, 0),
                    origin_fn = NULL,
                    scale_fn = identity,
                    keep_original = TRUE,
                    degrees_col_name = ".degrees",
                    radius_col_name = ".radius",
                    origin_col_name = ".origin") {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_numeric(
    radius,
    any.missing = FALSE,
    min.len = 1,
    add = assert_collection
  )
  checkmate::assert_string(x_col, null.ok = TRUE, add = assert_collection)
  checkmate::assert_string(y_col, null.ok = TRUE, add = assert_collection)
  checkmate::assert_string(suffix, add = assert_collection)
  checkmate::assert_string(degrees_col_name, null.ok = TRUE, add = assert_collection)
  checkmate::assert_string(radius_col_name, null.ok = TRUE, add = assert_collection)
  checkmate::assert_string(origin_col_name, null.ok = TRUE, add = assert_collection)
  checkmate::assert_numeric(origin,
                            len = 2,
                            any.missing = FALSE,
                            add = assert_collection)
  checkmate::assert_function(origin_fn, null.ok = TRUE, add = assert_collection)
  checkmate::assert_function(scale_fn, nargs = 1, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  if (is.data.frame(data) && is.null(y_col)) {
    assert_collection$push("when 'data' is a data.frame, 'y_col' must be specified.")
  }
  if (is.data.frame(data) && is.null(x_col)) {
    assert_collection$push("when 'data' is a data.frame, 'x_col' must be specified.")
  }
  if (length(c(x_col, y_col)) == 2 && x_col == y_col){
    assert_collection$push("'x_col' and 'y_col' cannot be the same column.")
  }
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Mutate for each degree
  purrr::map_dfr(
    .x = radius,
    .f = function(radi) {
      out <- multi_mutator(
        data = data,
        mutate_fn = swirl_2d_mutator_method,
        check_fn = NULL,
        force_df = TRUE,
        min_dims = 2,
        keep_original = keep_original,
        cols = c(x_col, y_col),
        radius = radi,
        scale_fn = scale_fn,
        suffix = suffix,
        origin = origin,
        origin_fn = origin_fn,
        degrees_col_name = degrees_col_name,
        origin_col_name = origin_col_name
      )
      if (!is.null(radius_col_name)) {
        out[[radius_col_name]] <- radi
      }

      out
    }
  )

}

swirl_2d_mutator_method <- function(data,
                                   cols,
                                   radius,
                                   scale_fn,
                                   suffix,
                                   origin,
                                   origin_fn,
                                   degrees_col_name,
                                   origin_col_name){

  # Extract columns
  x_col <- cols[[1]]
  y_col <- cols[[2]]

  # Convert columns to list of vectors
  dim_vectors <- as.list(data[, cols, drop=FALSE])

  # Find origin if specified
  origin <- apply_coordinate_fn(
    dim_vectors = dim_vectors,
    coordinates = origin,
    fn = origin_fn,
    num_dims = length(cols),
    coordinate_name = "origin",
    fn_name = "origin_fn",
    dim_var_name = "cols",
    allow_len_one = FALSE
  )

  # Calculate distances to origin
  distances <- calculate_distances(dim_vectors = dim_vectors, to = origin)

  # Scale distances
  scaled_distances <- scale_fn(distances)

  # Convert distances to degrees
  degrees <- calculate_swirl_degrees(distances = scaled_distances, radius = radius)

  # Add degrees column
  deg_tmp_var <- create_tmp_var(data = data, tmp_var = ".__degrees__", disallowed = degrees_col_name)
  data[[deg_tmp_var]] <- degrees

  # Call rotate_2d for each unique distance
  data <- purrr::map_dfr(.x = split(data, f = distances), .f = ~ {
    rotate_2d(
      data = .x,
      x_col = x_col,
      y_col = y_col,
      degrees = .x[[deg_tmp_var]][[1]],
      origin = origin,
      suffix = suffix,
      origin_col_name = NULL
    )
  })

  # Add info columns
  if (!is.null(origin_col_name)) {
    data[[origin_col_name]] <- list_coordinates(origin, names = cols)
  }
  if (!is.null(degrees_col_name)) {
    data[[degrees_col_name]] <- data[[deg_tmp_var]]
  }

  # Remove temporary column
  data[[deg_tmp_var]] <- NULL

  data
}
