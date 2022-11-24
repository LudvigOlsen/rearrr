

#   __________________ #< cd1a61becee10db96fdb9c8566818046 ># __________________
#   Rotate 2d                                                                ####


#' @title Rotate the values around an origin in 2 dimensions
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
#' @param origin Coordinates of the origin to rotate around.
#'  A \code{vector} with 2 elements (i.e. origin_x, origin_y).
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
#'   "Index" = 1:12,
#'   "A" = c(1:4, 9:12, 15:18),
#'   "G" = rep(1:3, each = 4)
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
#' if (has_ggplot){
#'   ggplot(df_rotated, aes(x = Index_rotated, y = A_rotated, color = factor(.degrees))) +
#'     geom_hline(yintercept = mean(df$A), size = 0.2, alpha = .4, linetype = "dashed") +
#'     geom_vline(xintercept = mean(df$Index), size = 0.2, alpha = .4, linetype = "dashed") +
#'     geom_line(alpha = .4) +
#'     geom_point() +
#'     theme_minimal() +
#'     labs(x = "Index", y = "Value", color = "Degrees")
#' }
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
#' if (has_ggplot){
#'   ggplot(df_grouped, aes(x = Index_rotated, y = A_rotated, color = factor(.degrees))) +
#'     geom_point() +
#'     theme_minimal() +
#'     labs(x = "Index", y = "Value", color = "Degrees")
#' }
rotate_2d <- function(data,
                      degrees,
                      x_col = NULL,
                      y_col = NULL,
                      suffix = "_rotated",
                      origin = NULL,
                      origin_fn = NULL,
                      keep_original = TRUE,
                      degrees_col_name = ".degrees",
                      origin_col_name = ".origin",
                      overwrite = FALSE) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_numeric(
    degrees,
    lower = -360,
    upper = 360,
    any.missing = FALSE,
    min.len = 1,
    add = assert_collection
  )
  checkmate::assert_string(x_col, null.ok = TRUE, add = assert_collection)
  checkmate::assert_string(y_col, null.ok = TRUE, add = assert_collection)
  checkmate::assert_string(suffix, add = assert_collection)
  checkmate::assert_string(degrees_col_name, null.ok = TRUE, add = assert_collection)
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
  check_unique_colnames_(x_col, y_col, degrees_col_name, origin_col_name)
  check_overwrite_(data = data, nm = degrees_col_name, overwrite = overwrite)
  check_overwrite_(data = data, nm = origin_col_name, overwrite = overwrite)
  # End of argument checks ####

  # Mutate for each degree
  purrr::map_dfr(
    .x = degrees,
    .f = function(degree) {
      out <- multi_mutator_(
        data = data,
        mutate_fn = rotate_2d_mutator_method_,
        check_fn = NULL,
        force_df = TRUE,
        min_dims = 2,
        keep_original = keep_original,
        cols = c(x_col, y_col),
        degrees = degree,
        suffix = suffix,
        overwrite = overwrite,
        origin = origin,
        origin_fn = origin_fn,
        origin_col_name = origin_col_name
      )
      if (!is.null(degrees_col_name)) {
        out[[degrees_col_name]] <- degree
      }

      out
    }
  )
}


rotate_2d_mutator_method_ <- function(data,
                                      grp_id,
                                      cols,
                                      overwrite,
                                      degrees,
                                      suffix,
                                      origin,
                                      origin_fn,
                                      origin_col_name,
                                      ...) {
  # Extract columns
  x_col <- cols[[1]]
  y_col <- cols[[2]]

  # Create rotation matrix based on the degrees
  rotation_matrix <- create_rotation_matrix_2d_(deg = degrees)

  # Extract x and y values
  if (is.null(x_col)) {
    x_col <- "Index"
    x <- seq_len(nrow(data))
    cols <- c(x_col, y_col)
  } else {
    x <- data[[x_col]]
  }
  y <- data[[y_col]]
  dim_vectors <- list(x, y)

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

  # Apply rotation matrix
  # Handles moving of the origin
  dim_vectors <- apply_transformation_matrix_dim_vectors_(
    dim_vectors = dim_vectors,
    mat = rotation_matrix,
    cols = cols,
    origin = origin
  )

  # Add rotated columns to data
  # Add dim_vectors as columns with the suffix
  data <- add_dimensions_(
    data = data,
    new_vectors = setNames(dim_vectors, cols),
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

  data
}
