

#   __________________ #< 7e8f15723958ac192ae9187ae855656b ># __________________
#   shear 2d                                                                ####


#' @title Shear the values around an origin in 2 dimensions
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  The values are rotated counterclockwise around a specified origin.
#'
#'  The origin can be supplied as coordinates or as a function that returns coordinates. The
#'  latter can be useful when supplying a grouped \code{data.frame} and rotating around e.g. the centroid
#'  of each group.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @param degrees Degrees to rotate values counterclockwise. In \code{[-360, 360]}.
#'  Can be a \code{vector} with multiple degrees.
#' @param x_col Name of x column in \code{`data`}. If \code{NULL} and \code{`data`} is a \code{vector},
#'  the index of \code{`data`} is used. If \code{`data`} is a \code{data.frame}, it must be specified.
#' @param y_col Name of y column in \code{`data`}. If \code{`data`} is a \code{data.frame}, it must be specified.
#' @param origin Coordinates of the origin to rotate around. Must be a \code{vector} with 2 elements (orig_x, orig_y).
#'  Ignored when \code{`origin_fn`} is not \code{NULL}.
#' @param degrees_col_name Name of new column with the degrees. If \code{NULL}, no column is added.
#' @param origin_col_name Name of new column with the origin coordinates. If \code{NULL}, no column is added.
#' @export
#' @return \code{data.frame} (\code{tibble}) with seven new columns containing
#'  the rotated x-,y- and z-values and the degrees, radiuses and origin coordinates.
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
#' @family rotation functions
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
#' # Rotate values around (0, 0)
#' rotate_2d(df, degrees = 45, x_col = "Index", y_col = "A", origin = c(0, 0))
#'
#' # Rotate A around the centroid
#' df_rotated <- df %>%
#'   rotate_2d(
#'     x_col = "Index",
#'     y_col = "A",
#'     degrees = c(0, 120, 240),
#'     origin_fn = centroid
#'   )
#' df_rotated
#'
#' # Plot A and A rotated around overall centroid
#' ggplot(df_rotated, aes(x = Index_rotated, y = A_rotated, color = factor(.degrees))) +
#'   geom_hline(yintercept = mean(df$A), size = 0.2, alpha = .4, linetype = "dashed") +
#'   geom_vline(xintercept = mean(df$Index), size = 0.2, alpha = .4, linetype = "dashed") +
#'   geom_line(alpha = .4) +
#'   geom_point() +
#'   theme_minimal() +
#'   labs(x = "Index", y = "Value", color = "Degrees")
#'
#' # Rotate around group centroids
#' df_grouped <- df %>%
#'   dplyr::group_by(G) %>%
#'   rotate_2d(
#'     x_col = "Index",
#'     y_col = "A",
#'     degrees = c(0, 120, 240),
#'     origin_fn = centroid
#'   )
#' df_grouped
#'
#' # Plot A and A rotated around group centroids
#' ggplot(df_grouped, aes(x = Index_rotated, y = A_rotated, color = factor(.degrees))) +
#'   geom_point() +
#'   theme_minimal() +
#'   labs(x = "Index", y = "Value", color = "Degrees")
#'
#' df_square <- square(runif(100), x_col_name = "x") %>%
#'   rotate_2d(
#'     degrees = 45,
#'     x_col="x",
#'     y_col = "Value",
#'     origin = c(0, 0.5),
#'     suffix = "",
#'     overwrite = T) %>%
#'   shear_2d(
#'     x_shear = 0.5,
#'     x_col = "x",
#'     y_col="Value",
#'     origin = c(0,0.5),
#'     overwrite = T)
#'
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
        out[[shear_col_name]] <- setNames(x_shear, "x") # TODO works?
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

  # Add rotated columns to data
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
