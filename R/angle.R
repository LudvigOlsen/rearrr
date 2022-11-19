

#   __________________ #< 1bae15c31fc98d40b9bd4b7b5434d7b3 ># __________________
#   Angle                                                                   ####


#' @title Calculate the angle to an origin
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Calculates the angle between each data point \eqn{(x2, y2)} and the origin \eqn{(x1, y1)} with:
#'  \deqn{atan2(y2 - y1, x2 - x1)}
#'
#'  And converts to degrees \code{[0-360)}, measured counterclockwise from the \code{\{x > x1, y = y1\}} line.
#'
#'  \ifelse{html}{\out{<img src='figures/angle_wheel.jpg' width="120" alt='Angles wheel'>}}{}
#'
#'  The origin can be supplied as coordinates or as a function that returns coordinates. The
#'  latter can be useful when supplying a grouped \code{data.frame} and finding the angle to e.g. the centroid
#'  of each group.
#'
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @param x_col Name of x column in \code{`data`}. If \code{NULL} and \code{`data`} is a \code{vector},
#'  the index of \code{`data`} is used. If \code{`data`} is a \code{data.frame}, it must be specified.
#' @param y_col Name of y column in \code{`data`}. If \code{`data`} is a \code{data.frame}, it must be specified.
#' @param origin Coordinates of the origin to calculate angle to.
#'  A scalar to use in all dimensions
#'  or a \code{vector} with one scalar per dimension.
#'
#'  \strong{N.B.} Ignored when \code{`origin_fn`} is not \code{NULL}.
#' @param degrees_col_name Name of new column with the degrees.
#' @param origin_col_name Name of new column with the origin coordinates. If \code{NULL}, no column is added.
#' @export
#' @return \code{data.frame} (\code{tibble}) with the additional columns (degrees and origin coordinates).
#' @inheritParams multi_mutator_
#' @family measuring functions
#' @examples
#' # Attach packages
#' library(rearrr)
#' library(dplyr)
#' has_ggplot <- require(ggplot2)  # Attach if installed
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
#' # Calculate angles in the two dimensions (x and y)
#' # With the origin at x=0.5, y=0.5
#' df_angles <- angle(
#'   data = df,
#'   x_col = "x",
#'   y_col = "y",
#'   origin = c(0.5, 0.5)
#' )
#' df_angles
#'
#' # Plot points with degrees
#' # Degrees are measured counterclockwise around the
#' # positive side of the x-axis
#' if (has_ggplot){
#'   df_angles %>%
#'     ggplot(aes(x = x, y = y, color = .degrees)) +
#'     geom_segment(aes(x = 0.5, xend = 1, y = 0.5, yend = 0.5), color = "magenta") +
#'     geom_point() +
#'     theme_minimal()
#' }
#'
#' # Calculate angles to the centroid for each group in 'g'
#' angle(
#'   data = dplyr::group_by(df, g),
#'   x_col = "x",
#'   y_col = "y",
#'   origin_fn = centroid
#' )
angle <- function(data,
                  x_col = NULL,
                  y_col = NULL,
                  origin = NULL,
                  origin_fn = NULL,
                  degrees_col_name = ".degrees",
                  origin_col_name = ".origin",
                  overwrite = FALSE) {
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_string(degrees_col_name, add = assert_collection)
  checkmate::assert_string(x_col, null.ok = TRUE, add = assert_collection)
  checkmate::assert_string(y_col, null.ok = TRUE, add = assert_collection)
  checkmate::assert_string(origin_col_name, null.ok = TRUE, add = assert_collection)
  checkmate::assert_numeric(origin,
    min.len = 1,
    any.missing = FALSE,
    null.ok = TRUE,
    add = assert_collection
  )
  checkmate::assert_function(origin_fn, null.ok = TRUE, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # Check if we will need to overwrite columns
  check_unique_colnames_(x_col, y_col, origin_col_name, degrees_col_name)
  check_overwrite_(data = data, nm = degrees_col_name, overwrite = overwrite)
  check_overwrite_(data = data, nm = origin_col_name, overwrite = overwrite)
  # End of argument checks ####

  # Mutate with each multiplier
  multi_mutator_(
    data = data,
    mutate_fn = angle_mutator_method_,
    check_fn = NULL,
    min_dims = 2,
    cols = c(x_col, y_col),
    force_df = TRUE,
    overwrite = overwrite,
    keep_original = TRUE,
    origin = origin,
    origin_fn = origin_fn,
    degrees_col_name = degrees_col_name,
    origin_col_name = origin_col_name
  )
}


angle_mutator_method_ <- function(data,
                                  grp_id,
                                  cols,
                                  overwrite,
                                  origin,
                                  origin_fn,
                                  degrees_col_name,
                                  origin_col_name,
                                  ...) {

  # Convert columns to list of vectors
  dim_vectors <- as.list(data[, cols, drop = FALSE])

  # Find origin if specified
  origin <- apply_coordinate_fn_(
    dim_vectors = dim_vectors,
    coordinates = origin,
    fn = origin_fn,
    num_dims = 2,
    coordinate_name = "origin",
    fn_name = "origin_fn",
    dim_var_name = "c(x_col, y_col)",
    grp_id = grp_id,
    allow_len_one = TRUE
  )

  data[[degrees_col_name]] <- atan2(
    data[[cols[[2]]]] - origin[[2]],
    data[[cols[[1]]]] - origin[[1]]
  )
  data[[degrees_col_name]] <-
    radians_to_degrees(data[[degrees_col_name]])
  data[[degrees_col_name]] <-
    ifelse(sign(data[[degrees_col_name]]) < 0,
      360 + data[[degrees_col_name]],
      data[[degrees_col_name]]
    )

  # Add origin coordinates
  data <- add_info_col_(
    data = data,
    nm = origin_col_name,
    content = list_coordinates_(origin, cols),
    overwrite = overwrite
  )

  data
}
