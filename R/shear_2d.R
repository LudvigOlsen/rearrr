

#   __________________ #< 7e8f15723958ac192ae9187ae855656b ># __________________
#   shear 2d                                                                ####


#' @title Shear the values around an origin in 2 dimensions
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Shear a set of 2d points around an origin.
#'  The shearing formulas (excluding the origin movements) is:
#'  \deqn{x' = x + x_shear * y}
#'  \deqn{y' = y + y_shear * x}
#'
#'  The data points in \code{`data`} are moved prior to the shearing, to bring
#'  the origin to \code{0} in all dimensions. After the shearing, the
#'  inverse move is applied to bring the origin back to its original position.
#'
#'  The origin can be supplied as coordinates or as a function that returns coordinates. The
#'  latter can be useful when supplying a grouped \code{data.frame} and shearing around e.g. the centroid
#'  of each group.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @param x_shear Shear factor for the x dimension (\code{numeric}). Decides the amount of shearing.
#'  Can be a \code{vector} with multiple shear factors.
#' @param y_shear Shear factor for the y dimension (\code{numeric}). Decides the amount of shearing.
#'  Can be a \code{vector} with multiple shear factors.
#' @param x_col Name of x column in \code{`data`}.
#' @param y_col Name of y column in \code{`data`}.
#' @param origin Coordinates of the origin to shear around.
#'  \code{Vector} with 2 elements (origin_x, origin_y).
#'  Ignored when \code{`origin_fn`} is not \code{NULL}.
#' @param shear_col_name Name of new column with the shearing factors.
#'  If \code{NULL}, no column is added.
#'
#'  Also adds a string version with the same name + \code{"_str"},
#'  making it easier to group by the shearing factors
#'  when plotting multiple shearings.
#' @param origin_col_name Name of new column with the origin coordinates.
#'  If \code{NULL}, no column is added.
#' @family mutate functions
#' @family shearing functions
#' @export
#' @return \code{data.frame} (\code{tibble}) with sheared columns,
#'  the shearing factors and the origin coordinates.
#' @inheritParams multi_mutator_
#' @examples
#' # Attach packages
#' library(rearrr)
#' library(dplyr)
#' library(ggplot2)
#'
#' # Create a data frame
#' df <- data.frame(
#'   "x" = rep(1:6, each = 2),
#'   "y" = rep(c(1, 4), 6),
#'   "g" = rep(1:2, each = 6)
#' )
#'
#' # Shear the x variable with regards to y
#' # around the centroid
#' df_sheared <- shear_2d(
#'   data = df,
#'   x_shear = 2.5,
#'   x_col = "x",
#'   y_col = "y",
#'   origin_fn = centroid
#' )
#'
#' # Plot sheared data
#' # Black: original points
#' # Red: sheared points
#' df_sheared %>%
#'   ggplot(aes(x = x, y = y)) +
#'   geom_point() +
#'   geom_point(aes(x = x_sheared, y = y_sheared, color = "red")) +
#'   theme_minimal()
#'
#' # Shear in both dimensions
#' df_sheared <- shear_2d(
#'   data = df,
#'   x_shear = 2.5,
#'   y_shear = 2.5,
#'   x_col = "x",
#'   y_col = "y",
#'   origin_fn = centroid
#' )
#'
#' # Plot sheared data
#' # Black: original points
#' # Red: sheared points
#' df_sheared %>%
#'   ggplot(aes(x = x, y = y)) +
#'   geom_point() +
#'   geom_point(aes(x = x_sheared,y = y_sheared, color = "red")) +
#'   theme_minimal()
#'
#' # Shear grouped data frame
#' # Affects the calculated origin
#' df_sheared <- shear_2d(
#'   data = dplyr::group_by(df, g),
#'   x_shear = 2.5,
#'   x_col = "x",
#'   y_col = "y",
#'   origin_fn = centroid
#' )
#'
#' # Plot sheared data
#' # Black: original points
#' # Red: sheared points
#' df_sheared %>%
#'   ggplot(aes(x = x, y = y)) +
#'   geom_point() +
#'   geom_point(aes(x = x_sheared, y = y_sheared, color = "red")) +
#'   theme_minimal()
#'
#' # Shear a vector with multiple shearing factors
#' shear_2d(
#'   data = c(1:10),
#'   x_shear = c(1, 1.5, 2, 2.5),
#'   origin = c(0, 0)
#' )
shear_2d <- function(data,
                     x_shear,
                     y_shear = 0,
                     x_col = NULL,
                     y_col = NULL,
                     suffix = "_sheared",
                     origin = NULL,
                     origin_fn = NULL,
                     keep_original = TRUE,
                     shear_col_name = ".shear",
                     origin_col_name = ".origin",
                     overwrite = FALSE) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_numeric(
    x_shear,
    any.missing = FALSE,
    min.len = 1,
    add = assert_collection
  )
  checkmate::assert_numeric(
    y_shear,
    any.missing = FALSE,
    min.len = 1,
    add = assert_collection
  )
  checkmate::assert_string(x_col, null.ok = TRUE, add = assert_collection)
  checkmate::assert_string(y_col, null.ok = TRUE, add = assert_collection)
  checkmate::assert_string(suffix, add = assert_collection)
  checkmate::assert_string(shear_col_name, null.ok = TRUE, add = assert_collection)
  checkmate::assert_string(origin_col_name, null.ok = TRUE, add = assert_collection)
  checkmate::assert_numeric(origin,
                            len = 2,
                            any.missing = FALSE,
                            null.ok = TRUE,
                            add = assert_collection
  )
  checkmate::assert_function(origin_fn, null.ok = TRUE, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  if (is.data.frame(data) && is.null(y_col)) {
    assert_collection$push("when 'data' is a data.frame, 'y_col' must be specified.")
  }
  if (is.data.frame(data) && is.null(x_col)) {
    assert_collection$push("when 'data' is a data.frame, 'x_col' must be specified.")
  }
  if (length(c(x_col, y_col)) == 2 && x_col == y_col) {
    assert_collection$push("'x_col' and 'y_col' cannot be the same column.")
  }
  if ((length(x_shear) > 1 && length(y_shear) > 1) &&
      length(x_shear) != length(y_shear)){
    assert_collection$push("when both 'x_shear' and 'y_shear' have length > 1, they must have the same length.")
  }
  checkmate::reportAssertions(assert_collection)
  # Check if we will need to overwrite columns
  check_unique_colnames_(x_col, y_col, shear_col_name, origin_col_name)
  check_overwrite_(data = data, nm = shear_col_name, overwrite = overwrite)
  check_overwrite_(data = data, nm = origin_col_name, overwrite = overwrite)
  # End of argument checks ####

  # Mutate for each degree
  purrr::map2_dfr(
    .x = x_shear,
    .y = y_shear,
    .f = function(shear_x, shear_y) {
      multi_mutator_(
        data = data,
        mutate_fn = shear_2d_mutator_method_,
        check_fn = NULL,
        force_df = TRUE,
        min_dims = 2,
        keep_original = keep_original,
        cols = c(x_col, y_col),
        x_shear = shear_x,
        y_shear = shear_y,
        suffix = suffix,
        overwrite = overwrite,
        origin = origin,
        origin_fn = origin_fn,
        origin_col_name = origin_col_name,
        shear_col_name = shear_col_name
      )
    }
  )
}


shear_2d_mutator_method_ <- function(data,
                                     grp_id,
                                     cols,
                                     overwrite,
                                     x_shear,
                                     y_shear,
                                     suffix,
                                     origin,
                                     origin_fn,
                                     origin_col_name,
                                     shear_col_name,
                                     ...) {

  # Creating shearing matrix
  shearing_matrix <- create_shearing_matrix_2d_(x_shear, y_shear)

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
    allow_len_one = FALSE
  )

  # Apply shearing matrix
  # Handles moving of the origin
  dim_vectors <- apply_transformation_matrix_dim_vectors_(
    dim_vectors = dim_vectors,
    mat = shearing_matrix,
    cols = cols,
    origin = origin
  )

  # Add sheared columns to data
  data <- add_dimensions_(
    data = data,
    new_vectors = setNames(
      dim_vectors,
      cols),
    suffix = suffix,
    overwrite = overwrite
  )

  # Add info columns
  data <- add_info_col_(
    data = data,
    nm = origin_col_name,
    content = list_coordinates_(origin, names = cols),
    check_overwrite = FALSE # Already checked
  )

  if (!is.null(shear_col_name)){
    data[[shear_col_name]] <- list_coordinates_(c(x_shear, y_shear), cols)
    data <- paste_coordinates_column_(data, shear_col_name)
  }


  data
}
