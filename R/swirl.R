

#   __________________ #< 9d281822e7dafc7afc466a38db278b1e ># __________________
#   Swirl 2d                                                                ####


#' @title Rotate the values around an origin in 2 dimensions
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  The values are rotated counterclockwise around a specified origin.
#'
#'  The origin can be supplied as coordinates or as a function that returns coordinates. The
#'  latter can be useful when supplying a grouped data frame and rotating around e.g. the centroid
#'  of each group.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @param degrees Degrees to rotate values counterclockwise. In \code{[-360, 360]}.
#'  Can be a \code{vector} with multiple degrees.
#' @param x_col Name of x column in \code{`data`}. If \code{NULL} and \code{`data`} is a \code{vector},
#'  the index of \code{`data`} is used. If \code{`data`} is a \code{data.frame}, it must be specified.
#' @param y_col Name of y column in \code{`data`}. If \code{`data`} is a \code{data.frame}, it must be specified.
#' @param origin Coordinates of the origin to rotate around. Must be a \code{vector} with 2 elements (orig_x, orig_y).
#'  Ignored when \code{`origin_fn`} is not \code{NULL}.
#' @param origin_fn Function for finding the origin coordinates to rotate the values around.
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
#' @param degree_col_name Name of new column with the degrees. If \code{NULL}, no column is added.
#' @export
#' @return \code{data.frame} (\code{tibble}) with three new columns containing the rotated x- and y-values and the degrees.
#' @details
#'  Applies the following rotation matrix:
#'
#'  | [ \eqn{cos \theta} |, \eqn{ -sin \theta} | ] |
#'  | :--- | :--- | :--- |
#'  | [ \eqn{sin \theta} |, \eqn{ cos \theta}  | ] |
#'
#'  That is:
#'
#'  \eqn{x' = x cos \theta - y sin \theta}
#'
#'  \eqn{y' = x sin \theta + y cos \theta}
#'
#'  Where \eqn{\theta} is the angle in radians.
#'
#'  As specified at [Wikipedia/Rotation_matrix](https://en.wikipedia.org/wiki/Rotation_matrix).
#' @family mutate functions
#' @inheritParams multi_mutator
#' @examples
#' \donttest{
#' # Attach packages
#' library(rearrr)
#' library(dplyr)
#' library(ggplot2)
#'
#' # Set seed
#' set.seed(1)
#'
#' # Create a data frame
#' df <- data.frame(
#'   "x" = 1:50,
#'   "y" = 1,
#'   "g" = rep(1:5, each=10)
#' )
#'
#' # Rotate values
#' swirl2d(df, radius = 45, x_col = "x", y_col = "y")
#'
#' # Swirl around the centroid
#' df_swirled <- swirl2d(
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
#'   ggplot(aes(x=x_rotated, y=y_rotated, color = factor(.radius))) +
#'   geom_point() +
#'   theme_minimal() +
#'   labs(x = "x", y = "y", color = ".radius")
#'
#' # Rotate around group centroids
#' df_grouped <- df %>%
#'   dplyr::group_by(g) %>%
#'   swirl2d(
#'     radius = c(95, 96, 97, 98, 99, 100),
#'     x_col = "x",
#'     y_col = "y",
#'     origin_fn = centroid,
#'     scale_fn = function(x) {
#'       x ^ 1.6
#'     })
#'
#' df_grouped
#'
#' # Plot group swirls
#' df_grouped %>%
#'   ggplot(aes(x = x_rotated, y = y_rotated, color = factor(.radius))) +
#'   geom_point() +
#'   theme_minimal() +
#'   labs(x = "x", y = "y", color = ".radius")
#'
#' }
swirl2d <- function(data,
                    radius,
                    x_col = NULL,
                    y_col = NULL,
                    suffix = "_swirled",
                    origin = c(0, 0),
                    origin_fn = NULL,
                    scale_fn = identity,
                    keep_original = TRUE,
                    degree_col_name = ".degrees",
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
  checkmate::assert_string(degree_col_name, null.ok = TRUE, add = assert_collection)
  checkmate::assert_string(radius_col_name, null.ok = TRUE, add = assert_collection)
  checkmate::assert_string(origin_col_name, null.ok = TRUE, add = assert_collection)
  checkmate::assert_numeric(origin,
                            len = 2,
                            any.missing = FALSE,
                            add = assert_collection)
  checkmate::assert_function(origin_fn, null.ok = TRUE, add = assert_collection)
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
        mutate_fn = swirl2d_mutator_method,
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
        degree_col_name = degree_col_name,
        origin_col_name = origin_col_name
      )
      if (!is.null(radius_col_name)) {
        out[[radius_col_name]] <- radi
      }

      out
    }
  )

}

swirl2d_mutator_method <- function(data,
                                   cols,
                                   radius,
                                   scale_fn,
                                   suffix,
                                   origin,
                                   origin_fn,
                                   degree_col_name,
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

  # Convert distances to degrees
  degrees <- (scale_fn(distances)/radius * 360) %% 360

  # Add degrees column (TODO Get name from user)
  deg_tmp_var <- create_tmp_var(data=data, tmp_var = ".degrees")
  data[[deg_tmp_var]] <- degrees

  # Call rotate2d for each unique distance
  data <- purrr::map_dfr(.x = split(data, f = distances), .f = ~{
    rotate2d(data = .x, x_col = x_col, y_col = y_col, degrees = .x[[deg_tmp_var]][[1]], origin = origin)
  })

  # Add info columns
  if (!is.null(origin_col_name)) {
    data[[origin_col_name]] <- list_coordinates(origin, names = cols)
  }
  if (!is.null(degree_col_name)) {
    data[[degree_col_name]] <- data[[deg_tmp_var]]
  }
  data[[deg_tmp_var]] <- NULL

  data
}
