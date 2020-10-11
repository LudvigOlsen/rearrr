

#   __________________ #< cbb313c6f0b4b461ad626879ebe2ac6a ># __________________
#   Shear 3d                                                                ####

# TODO Add:
# @family mutate functions
# @family shearing functions

#' @title Shear values around an origin in 3 dimensions
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  TODO
#'
#'  The origin can be supplied as coordinates or as a function that returns coordinates. The
#'  latter can be useful when supplying a grouped \code{data.frame} and shearing around e.g. the centroid
#'  of each group.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @param x_shear,y_shear,z_shear TODO
#' @param x_col,y_col,z_col Name of x/y/z column in \code{`data`}. All must be specified.
#' @param origin Coordinates of the origin to rotate around. Must be a \code{vector} with 3 elements (i.e. origin_x, origin_y, origin_z).
#'  Ignored when \code{`origin_fn`} is not \code{NULL}.
#' @param shear_col_name Name of new column with the shearing amounts. If \code{NULL}, no column is added.
#'
#'  Also adds a string version with the same name + \code{"_str"}, making it easier to group by the shearing amounts
#'  when plotting multiple shears.
#' @param origin_col_name Name of new column with the origin coordinates. If \code{NULL}, no column is added.
#' @keywords internal
#' @return \code{data.frame} (\code{tibble}) with five new columns containing
#'  the sheared x-, y- and z-values and the shearing amounts and origin coordinates.
#' @details
#'  TODO
#' @inheritParams multi_mutator_
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
#' df_square <- square(runif(100)) %>%
#'   rename(x = .square_x,
#'          y = Value) %>%
#'   mutate(z = 1)
#'
#' # Shear the x variable with regards to y
#' # around the centroid
#' df_sheared <- shear_3d(
#'   data = df_square,
#'   x_col = "x",
#'   y_col = "y",
#'   z_col = "z",
#'   x_shear = 2,
#'   z_shear = 4,
#'   origin_fn = centroid
#' )
#'
#' # Plot sheared data
#' # Black: original points
#' # Red: sheared points
#' # Size: sheared z value
#' df_sheared %>%
#'   ggplot(aes(x = x, y = y)) +
#'   geom_point() +
#'   geom_point(aes(x = x_sheared, y = y_sheared, color = "red")) +
#'   theme_minimal()
#'
#' \dontrun{
#' # Plot 3d with plotly
#' plotly::plot_ly(
#'   x = df_sheared$x_sheared,
#'   y = df_sheared$y_sheared,
#'   z = df_sheared$z_sheared,
#'   type = "scatter3d",
#'   mode = "markers",
#'   color = df_sheared$.shear_str
#' )
#' }
#'
#'
#' }
shear_3d <- function(data,
                     x_col,
                     y_col,
                     z_col,
                     x_shear = NULL,
                     y_shear = NULL,
                     z_shear = NULL,
                     suffix = "_sheared",
                     origin = NULL,
                     origin_fn = NULL,
                     keep_original = TRUE,
                     shear_col_name = ".shear",
                     origin_col_name = ".origin",
                     overwrite = FALSE) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(data, min.cols = 3, add = assert_collection)
  checkmate::assert_numeric(
    x_shear,
    any.missing = FALSE,
    null.ok = TRUE,
    len = 1,
    add = assert_collection
  )
  checkmate::assert_numeric(
    y_shear,
    any.missing = FALSE,
    null.ok = TRUE,
    len = 1,
    add = assert_collection
  )
  checkmate::assert_numeric(
    z_shear,
    any.missing = FALSE,
    null.ok = TRUE,
    len = 1,
    add = assert_collection
  )
  checkmate::assert_string(x_col, add = assert_collection)
  checkmate::assert_string(y_col, add = assert_collection)
  checkmate::assert_string(z_col, add = assert_collection)
  checkmate::assert_string(suffix, add = assert_collection)
  checkmate::assert_string(shear_col_name, null.ok = TRUE, add = assert_collection)
  checkmate::assert_string(origin_col_name, null.ok = TRUE, add = assert_collection)
  checkmate::assert_numeric(origin,
                            len = 3,
                            any.missing = FALSE,
                            null.ok = TRUE,
                            add = assert_collection
  )
  checkmate::assert_function(origin_fn, null.ok = TRUE, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  checkmate::assert_character(
    c(x_col, y_col, z_col),
    min.chars = 1,
    any.missing = FALSE,
    len = 3,
    unique = TRUE,
    add = assert_collection
  )
  if (length(c(x_shear, y_shear, z_shear)) != 2){
    assert_collection$push(
      "Exactly 2 of {x_shear, y_shear, z_shear} must be non-null."
    )
  }
  checkmate::reportAssertions(assert_collection)
  # Check if we will need to overwrite columns
  check_unique_colnames_(x_col, y_col, z_col, shear_col_name, origin_col_name)
  check_overwrite_(data = data, nm = shear_col_name, overwrite = overwrite)
  check_overwrite_(data = data, nm = origin_col_name, overwrite = overwrite)
  # End of argument checks ####

  multi_mutator_(
    data = data,
    mutate_fn = shear_3d_mutator_method_,
    check_fn = NULL,
    force_df = TRUE,
    overwrite = overwrite,
    min_dims = 3,
    keep_original = keep_original,
    cols = c(x_col, y_col, z_col),
    x_shear = x_shear,
    y_shear = y_shear,
    z_shear = z_shear,
    suffix = suffix,
    origin = origin,
    origin_fn = origin_fn,
    origin_col_name = origin_col_name,
    shear_col_name = shear_col_name
  )
}


shear_3d_mutator_method_ <- function(data,
                                     grp_id,
                                     cols,
                                     overwrite,
                                     x_shear,
                                     y_shear,
                                     z_shear,
                                     suffix,
                                     origin,
                                     origin_fn,
                                     origin_col_name,
                                     shear_col_name,
                                     ...) {

  # Create rotation matrix
  shearing_matrix <- create_shearing_matrix_3d_(
    x_shear = x_shear,
    y_shear = y_shear,
    z_shear = z_shear
  )

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
  if (!is.null(shear_col_name)) {
    shear_amounts <- list(x_shear, y_shear, z_shear)
    shear_amounts <- replace(shear_amounts, shear_amounts == "NULL", NA_real_)
    data[[shear_col_name]] <- list_coordinates_(shear_amounts, names = cols)
    data <- paste_coordinates_column_(data, shear_col_name, na.rm = TRUE)
  }
  if (!is.null(origin_col_name)) {
    data[[origin_col_name]] <- list_coordinates_(origin, names = cols)
  }

  data
}

