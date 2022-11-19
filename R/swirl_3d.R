

#   __________________ #< 75c51fb80e1d6de1d19a0a8257294c4a ># __________________
#   Swirl 3d                                                                ####


#' @title Swirl the values around an origin in 3 dimensions
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  The values are swirled counterclockwise around a specified origin.
#'  The swirling is done by rotating around the origin, basing the degrees
#'  for each rotation-axis on the distances to the origin as so:
#'  \deqn{x_degrees = scale_fn(distances) / (2 * x_radius) * 360}
#'
#'  The origin can be supplied as coordinates or as a function that returns coordinates. The
#'  latter can be useful when supplying a grouped \code{data.frame} and swirling around e.g. the centroid
#'  of each group.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @param x_radius,y_radius,z_radius Radiuses of the
#'  most-inner swirls around each axis (in the \emph{simplest} case).
#'  Can be \code{vector}s with multiple radiuses.
#'
#'  E.g. the \code{`x_radius`} specifies the radius when rotating \emph{around} the x-axis,
#'  not the radius \emph{on} the x-axis.
#'
#'  Note: With a custom \code{`scaling_fn`}, these might not be the actual swirl radiuses anymore. Think of
#'  them more as width settings where a larger number leads to fewer full rotations.
#' @param x_col,y_col,z_col Name of x/y/z column in \code{`data`}. All must be specified.
#' @param origin Coordinates of the origin to swirl around.
#'  \code{Vector} with 3 elements (i.e. origin_x, origin_y, origin_z).
#'  Ignored when \code{`origin_fn`} is not \code{NULL}.
#' @param scale_fn Function for scaling the distances before calculating the degrees.
#'
#'  \strong{Input}: A \code{numeric vector} (the distances).
#'
#'  \strong{Output}: A \code{numeric vector} (the scaled distances) of the same length.
#'
#'  E.g.:
#'
#'  \code{function(d)\{}
#'
#'  \verb{  }\code{d ^ 1.5}
#'
#'  \code{\}}
#' @param degrees_col_name Name of new column with the degrees. If \code{NULL}, no column is added.
#'
#'  Also adds a string version with the same name + \code{"_str"}, making it easier to group by the degrees
#'  when plotting multiple rotations.
#' @param origin_col_name Name of new column with the origin coordinates. If \code{NULL}, no column is added.
#' @param radius_col_name Name of new column with the radiuses. If \code{NULL}, no column is added.
#' @export
#' @return \code{data.frame} (\code{tibble}) with new columns containing
#'  the swirled x- and y-values, the degrees, the radiuses, and the origin coordinates.
#' @family mutate functions
#' @family rotation functions
#' @family distance functions
#' @inheritParams multi_mutator_
#' @examples
#' # Attach packages
#' library(rearrr)
#' library(dplyr)
#' has_ggplot <- require(ggplot2)  # Attach if installed
#'
#' # Set seed
#' set.seed(4)
#'
#' # Create a data frame
#' df <- data.frame(
#'   "x" = 1:50,
#'   "y" = 1:50,
#'   "z" = 1:50,
#'   "r1" = runif(50),
#'   "r2" = runif(50) * 35,
#'   "o" = 1,
#'   "g" = rep(1:5, each = 10)
#' )
#'
#' # Swirl values around (0, 0, 0)
#' swirl_3d(
#'   data = df,
#'   x_radius = 45,
#'   x_col = "x",
#'   y_col = "y",
#'   z_col = "z",
#'   origin = c(0, 0, 0)
#' )
#'
#' # Swirl around the centroid
#' df_swirled <- swirl_3d(
#'   data = df,
#'   x_col = "x",
#'   y_col = "y",
#'   z_col = "z",
#'   x_radius = c(100, 0, 0),
#'   y_radius = c(0, 100, 0),
#'   z_radius = c(0, 0, 100),
#'   origin_fn = centroid
#' )
#'
#' df_swirled
#'
#' # Plot swirls
#' if (has_ggplot){
#'   ggplot(df_swirled, aes(x = x_swirled, y = y_swirled, color = .radius_str, alpha = z_swirled)) +
#'     geom_vline(xintercept = mean(df$x), size = 0.2, alpha = .4, linetype = "dashed") +
#'     geom_hline(yintercept = mean(df$y), size = 0.2, alpha = .4, linetype = "dashed") +
#'     geom_path(alpha = .4) +
#'     geom_point() +
#'     theme_minimal() +
#'     labs(x = "x", y = "y", color = "radius", alpha = "z (opacity)")
#' }
#'
#' \dontrun{
#' # Plot 3d with plotly
#' plotly::plot_ly(
#'   x = df_swirled$x_swirled,
#'   y = df_swirled$y_swirled,
#'   z = df_swirled$z_swirled,
#'   type = "scatter3d",
#'   mode = "markers",
#'   color = df_swirled$.radius_str
#' )
#' }
#'
#' # Swirl around the centroid
#' df_swirled <- swirl_3d(
#'   data = df,
#'   x_col = "x",
#'   y_col = "y",
#'   z_col = "z",
#'   x_radius = c(50, 0, 0),
#'   y_radius = c(0, 50, 0),
#'   z_radius = c(0, 0, 50),
#'   origin_fn = centroid
#' )
#'
#' df_swirled
#'
#' # Plot swirls
#' if (has_ggplot){
#'   ggplot(df_swirled, aes(x = x_swirled, y = y_swirled, color = .radius_str, alpha = z_swirled)) +
#'     geom_vline(xintercept = mean(df$x), size = 0.2, alpha = .4, linetype = "dashed") +
#'     geom_hline(yintercept = mean(df$y), size = 0.2, alpha = .4, linetype = "dashed") +
#'     geom_path(alpha = .4) +
#'     geom_point() +
#'     theme_minimal() +
#'     labs(x = "x", y = "y", color = "radius", alpha = "z (opacity)")
#' }
#'
#' \dontrun{
#' # Plot 3d with plotly
#' plotly::plot_ly(
#'   x = df_swirled$x_swirled,
#'   y = df_swirled$y_swirled,
#'   z = df_swirled$z_swirled,
#'   type = "scatter3d",
#'   mode = "markers",
#'   color = df_swirled$.radius_str
#' )
#' }
#' \donttest{
#'
#' df_swirled <- swirl_3d(
#'   data = df,
#'   x_col = "x",
#'   y_col = "y",
#'   z_col = "z",
#'   x_radius = c(25, 50, 25, 25),
#'   y_radius = c(50, 75, 100, 25),
#'   z_radius = c(75, 25, 25, 25),
#'   origin_fn = centroid,
#'   scale_fn = function(x) {
#'     x^0.81
#'   }
#' )
#'
#' # Plot swirls
#' if (has_ggplot){
#'   ggplot(df_swirled, aes(x = x_swirled, y = y_swirled, color = .radius_str, alpha = z_swirled)) +
#'     geom_vline(xintercept = mean(df$x), size = 0.2, alpha = .4, linetype = "dashed") +
#'     geom_hline(yintercept = mean(df$y), size = 0.2, alpha = .4, linetype = "dashed") +
#'     geom_path(alpha = .4) +
#'     geom_point() +
#'     theme_minimal() +
#'     labs(x = "x", y = "y", color = "radius", alpha = "z (opacity)")
#' }
#' }
#'
#' \dontrun{
#' # Plot 3d with plotly
#' plotly::plot_ly(
#'   x = df_swirled$x_swirled,
#'   y = df_swirled$y_swirled,
#'   z = df_swirled$z_swirled,
#'   type = "scatter3d",
#'   mode = "markers",
#'   color = df_swirled$.radius_str
#' )
#' }
#' \donttest{
#'
#' #
#' # Swirl random data
#' # The trick lies in finding the right radiuses
#' #
#'
#' # Swirl the random columns
#' df_swirled <- swirl_3d(
#'   data = df,
#'   x_col = "r1",
#'   y_col = "r2",
#'   z_col = "o",
#'   x_radius = c(0, 0, 0, 0),
#'   y_radius = c(0, 30, 60, 90),
#'   z_radius = c(10, 10, 10, 10),
#'   origin_fn = centroid
#' )
#'
#' # Not let's rotate it every 10 degrees
#' df_rotated <- df_swirled %>%
#'   rotate_3d(
#'     x_col = "r1_swirled",
#'     y_col = "r2_swirled",
#'     z_col = "o_swirled",
#'     x_deg = rep(0, 36),
#'     y_deg = rep(0, 36),
#'     z_deg = (1:36) * 10,
#'     suffix = "",
#'     origin = df_swirled$.origin[[1]],
#'     overwrite = TRUE
#'   )
#'
#' # Plot rotated swirls
#' if (has_ggplot){
#'   ggplot(
#'     df_rotated,
#'     aes(
#'       x = r1_swirled,
#'       y = r2_swirled,
#'       color = .degrees_str,
#'       alpha = o_swirled
#'     )
#'   ) +
#'     geom_vline(xintercept = mean(df$r1), size = 0.2, alpha = .4, linetype = "dashed") +
#'     geom_hline(yintercept = mean(df$r2), size = 0.2, alpha = .4, linetype = "dashed") +
#'     geom_point(show.legend = FALSE) +
#'     theme_minimal() +
#'     labs(x = "r1", y = "r2", color = "radius", alpha = "o (opacity)")
#' }
#' }
swirl_3d <- function(data,
                     x_col,
                     y_col,
                     z_col,
                     x_radius = 0,
                     y_radius = 0,
                     z_radius = 0,
                     suffix = "_swirled",
                     origin = NULL,
                     origin_fn = NULL,
                     scale_fn = identity,
                     keep_original = TRUE,
                     degrees_col_name = ".degrees",
                     radius_col_name = ".radius",
                     origin_col_name = ".origin",
                     overwrite = FALSE) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(data, min.cols = 3, add = assert_collection)
  checkmate::assert_numeric(
    x_radius,
    any.missing = FALSE,
    min.len = 1,
    add = assert_collection
  )
  checkmate::assert_numeric(
    y_radius,
    any.missing = FALSE,
    min.len = 1,
    add = assert_collection
  )
  checkmate::assert_numeric(
    z_radius,
    any.missing = FALSE,
    min.len = 1,
    add = assert_collection
  )
  checkmate::assert_string(x_col, add = assert_collection)
  checkmate::assert_string(y_col, add = assert_collection)
  checkmate::assert_string(z_col, add = assert_collection)
  checkmate::assert_string(suffix, add = assert_collection)
  checkmate::assert_string(degrees_col_name, null.ok = TRUE, add = assert_collection)
  checkmate::assert_string(radius_col_name, null.ok = TRUE, add = assert_collection)
  checkmate::assert_string(origin_col_name, null.ok = TRUE, add = assert_collection)
  checkmate::assert_numeric(origin,
    len = 3,
    any.missing = FALSE,
    null.ok = TRUE,
    add = assert_collection
  )
  checkmate::assert_function(origin_fn, null.ok = TRUE, add = assert_collection)
  checkmate::assert_function(scale_fn, nargs = 1, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  checkmate::assert_character(
    c(x_col, y_col, z_col),
    min.chars = 1,
    any.missing = FALSE,
    len = 3,
    unique = TRUE,
    add = assert_collection
  )
  if (!all(length(x_radius) == c(length(y_radius), length(z_radius)))) {
    assert_collection$push(
      paste0(
        "'x_radius', 'y_radius', and 'z_radius' must all have the same length but had lengths: ",
        paste0(c(
          length(x_radius), length(y_radius), length(z_radius)
        ), collapse = ", "),
        "."
      )
    )
  }
  checkmate::reportAssertions(assert_collection)
  # Check if we will need to overwrite columns
  check_unique_colnames_(x_col, y_col, z_col, degrees_col_name, origin_col_name, radius_col_name)
  check_overwrite_(data = data, nm = degrees_col_name, overwrite = overwrite)
  check_overwrite_(data = data, nm = origin_col_name, overwrite = overwrite)
  check_overwrite_(data = data, nm = radius_col_name, overwrite = overwrite)
  # End of argument checks ####

  # Mutate for each degree
  output <- purrr::map_dfr(
    .x = purrr::transpose(list(x_radius, y_radius, z_radius)) %>%
      purrr::simplify_all(),
    .f = function(radiuses) {
      out <- multi_mutator_(
        data = data,
        mutate_fn = swirl_3d_mutator_method_,
        check_fn = NULL,
        force_df = TRUE,
        min_dims = 3,
        keep_original = keep_original,
        cols = c(x_col, y_col, z_col),
        overwrite = overwrite,
        x_radius = radiuses[[1]],
        y_radius = radiuses[[2]],
        z_radius = radiuses[[3]],
        scale_fn = scale_fn,
        suffix = suffix,
        origin = origin,
        origin_fn = origin_fn,
        degrees_col_name = degrees_col_name,
        origin_col_name = origin_col_name
      )

      if (!is.null(radius_col_name)) {
        out[[radius_col_name]] <- list_coordinates_(
          radiuses,
          names = c(x_col, y_col, z_col)
        )
      }


      out
    }
  )

  if (!is.null(radius_col_name)) {
    output <- paste_coordinates_column_(output, radius_col_name)
  }

  output
}

swirl_3d_mutator_method_ <- function(data,
                                     grp_id,
                                     cols,
                                     overwrite,
                                     x_radius,
                                     y_radius,
                                     z_radius,
                                     scale_fn,
                                     suffix,
                                     origin,
                                     origin_fn,
                                     degrees_col_name,
                                     origin_col_name,
                                     ...) {

  # Extract columns
  x_col <- cols[[1]]
  y_col <- cols[[2]]
  z_col <- cols[[3]]

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

  # Calculate distances to origin
  distances <- calculate_distances_(dim_vectors = dim_vectors, to = origin)

  # Scale distances
  scaled_distances <- scale_fn(distances)

  if (length(scaled_distances) != length(distances)) {
    stop("the output of 'scale_fn' must have the same length as the input.")
  }

  # Convert distances to degrees
  x_degrees <- calculate_swirl_degrees_(distances = scaled_distances, radius = x_radius)
  y_degrees <- calculate_swirl_degrees_(distances = scaled_distances, radius = y_radius)
  z_degrees <- calculate_swirl_degrees_(distances = scaled_distances, radius = z_radius)

  # Add degrees column
  deg_tmp_var <- create_tmp_var(data = data, tmp_var = ".__degrees__", disallowed = degrees_col_name)
  data[[deg_tmp_var]] <- list(x_degrees, y_degrees, z_degrees) %>%
    purrr::transpose() %>%
    purrr::simplify_all() %>%
    purrr::map(~ {
      setNames(.x, cols)
    })

  # Call rotate_3d for each unique distance
  data <- purrr::map_dfr(.x = split(data, f = scaled_distances), .f = ~ {
    tmp_degrees <- .x[[deg_tmp_var]][[1]]
    rotate_3d(
      data = .x,
      x_col = x_col,
      y_col = y_col,
      z_col = z_col,
      x_deg = tmp_degrees[[1]],
      y_deg = tmp_degrees[[2]],
      z_deg = tmp_degrees[[3]],
      origin = origin,
      suffix = suffix,
      origin_col_name = NULL,
      degrees_col_name = NULL,
      overwrite = overwrite
    )
  })

  # Add info columns
  if (!is.null(origin_col_name)) {
    data[[origin_col_name]] <- list_coordinates_(origin, names = cols)
  }
  if (!is.null(degrees_col_name)) {
    data[[degrees_col_name]] <- data[[deg_tmp_var]]
  }

  # Remove temporary column
  data[[deg_tmp_var]] <- NULL

  data
}
