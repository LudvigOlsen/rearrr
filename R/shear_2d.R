

#   __________________ #< 7e8f15723958ac192ae9187ae855656b ># __________________
#   shear 2d                                                                ####

# TODO Add:
# @family mutate functions
# @family shearing functions

#' @title Shear the values around an origin in 2 dimensions
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  TODO
#'
#'  The origin can be supplied as coordinates or as a function that returns coordinates. The
#'  latter can be useful when supplying a grouped \code{data.frame} and shearing around e.g. the centroid
#'  of each group.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @param x_shear TODO (Also, can this work with data == vector?
#'  Or do we need to specify that it HAS to be a data.frame?)
#' @param x_col Name of x column in \code{`data`}.
#' @param y_col Name of y column in \code{`data`}.
#' @param origin Coordinates of the origin to shear around. Must be a \code{vector} with 2 elements (orig_x, orig_y).
#'  Ignored when \code{`origin_fn`} is not \code{NULL}.
#' @param shear_col_name Name of new column with the shearing amounts. If \code{NULL}, no column is added.
#'
#'  Also adds a string version with the same name + \code{"_str"}, making it easier to group by the shearing amounts
#'  when plotting multiple shears.
#' @param origin_col_name Name of new column with the origin coordinates. If \code{NULL}, no column is added.
#' @keywords internal
#' @return \code{data.frame} (\code{tibble}) TODO
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
#' # Create a data frame
#' df <- data.frame(
#'   "Index" = 1:12,
#'   "A" = c(
#'     1, 2, 3, 4, 9, 10, 11,
#'     12, 15, 16, 17, 18
#'   ),
#'   "G" = c(
#'     1, 1, 1, 1, 2, 2,
#'     2, 2, 3, 3, 3, 3
#'   )
#' )
#'
#' # TODO Add examples ;)
#' }
shear_2d <- function(data,
                     x_shear,
                     x_col = NULL,
                     y_col = NULL,
                     suffix = "_sheared",
                     origin = NULL,
                     origin_fn = NULL,
                     keep_original = TRUE,
                     shear_col_name = ".x_shear",
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
  checkmate::reportAssertions(assert_collection)
  # Check if we will need to overwrite columns
  check_unique_colnames_(x_col, y_col, shear_col_name, origin_col_name)
  check_overwrite_(data = data, nm = shear_col_name, overwrite = overwrite)
  check_overwrite_(data = data, nm = origin_col_name, overwrite = overwrite)
  # End of argument checks ####

  # Mutate for each degree
  purrr::map_dfr(
    .x = x_shear,
    .f = function(shear) {
      out <- multi_mutator_(
        data = data,
        mutate_fn = shear_2d_mutator_method_,
        check_fn = NULL,
        force_df = TRUE,
        min_dims = 2,
        keep_original = keep_original,
        cols = c(x_col, y_col),
        x_shear = shear,
        suffix = suffix,
        overwrite = overwrite,
        origin = origin,
        origin_fn = origin_fn,
        origin_col_name = origin_col_name
      )
      if (!is.null(shear_col_name)) {
        out[[shear_col_name]] <- setNames(x_shear, x_col) # TODO works?
        out <- paste_coordinates_column_(out, shear_col_name)
      }

      out
    }
  )
}


shear_2d_mutator_method_ <- function(data,
                                     grp_id,
                                     cols,
                                     overwrite,
                                     x_shear,
                                     suffix,
                                     origin,
                                     origin_fn,
                                     origin_col_name,
                                     ...) {

  # Extract columns
  x_col <- cols[[1]]
  y_col <- cols[[2]]

  # Extract x and y values
  x <- data[[x_col]]
  y <- data[[y_col]]

  # Find origin if specified
  origin <- apply_coordinate_fn_(
    dim_vectors = list(x, y),
    coordinates = origin,
    fn = origin_fn,
    num_dims = length(cols),
    coordinate_name = "origin",
    fn_name = "origin_fn",
    dim_var_name = "cols",
    grp_id = grp_id,
    allow_len_one = FALSE
  )

  # Move origin
  x <- x - origin[[1]]

  # Shear in x-axis
  x <- x + x_shear * (y - origin[[2]])

  # Move origin
  x <- x + origin[[1]]

  # Add sheared columns to data
  data <- add_info_col_(
    data = data,
    nm = paste0(x_col, suffix),
    content = x,
    overwrite = overwrite
  )
  data <- add_info_col_(
    data = data,
    nm = paste0(y_col, suffix),
    content = y,
    overwrite = overwrite
  )

  # Add info columns
  data <- add_info_col_(
    data = data,
    nm = origin_col_name,
    content = list_coordinates_(origin, names = cols),
    check_overwrite = FALSE # Already checked
  )

  data
}
