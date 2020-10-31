

#   __________________ #< 693e49c6d0dbc6d737ec22cc3cb480fb ># __________________
#   Circularize                                                             ####


#' @title Create x-coordinates so the points form a circle
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Create the x-coordinates for a \code{vector} of y-coordinates such that
#'  they form a circle.
#'
#'  This will likely look most like a circle when the y-coordinates are somewhat equally distributed,
#'  e.g. a uniform distribution.
#'
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @param degrees_col_name Name of new column with the angles in degrees. If \code{NULL}, no column is added.
#'
#'  Angling is counterclockwise around \code{(0, 0)} and starts at \code{(max(x), 0)}.
#' @param origin_col_name Name of new column with the origin coordinates (center of circle). If \code{NULL}, no column is added.
#' @inheritParams hexagonalize
#' @export
#' @return \code{data.frame} (\code{tibble}) with the added x-coordinates and the angle in degrees.
#' @family forming functions
#' @inheritParams multi_mutator_
#' @examples
#' # Attach packages
#' library(rearrr)
#' library(dplyr)
#' library(purrr)
#' library(ggplot2)
#'
#' # Set seed
#' set.seed(1)
#'
#' # Create a data frame
#' df <- data.frame(
#'   "y" = runif(200),
#'   "g" = factor(rep(1:5, each = 40))
#' )
#'
#' # Circularize 'y'
#' df_circ <- circularize(df, y_col = "y")
#' df_circ
#'
#' # Plot circle
#' df_circ %>%
#'   ggplot(aes(x = .circle_x, y = y, color = .degrees)) +
#'   geom_point() +
#'   theme_minimal()
#'
#' #
#' # Grouped circularization
#' #
#'
#' # Circularize 'y' for each group
#' # First cluster the groups a bit to move the
#' # circles away from each other
#' df_circ <- df %>%
#'   cluster_groups(
#'     cols = "y",
#'     group_cols = "g",
#'     suffix = "",
#'     overwrite = TRUE
#'   ) %>%
#'   dplyr::group_by(g) %>%
#'   circularize(
#'     y_col = "y",
#'     overwrite = TRUE
#'   )
#'
#' # Plot circles
#' df_circ %>%
#'   ggplot(aes(x = .circle_x, y = y, color = g)) +
#'   geom_point() +
#'   theme_minimal()
#'
#' #
#' # Specifying minimum value
#' #
#'
#' # Specify minimum value manually
#' df_circ <- circularize(df, y_col = "y", .min = -2)
#' df_circ
#'
#' # Plot circle
#' df_circ %>%
#'   ggplot(aes(x = .circle_x, y = y, color = .degrees)) +
#'   geom_point() +
#'   theme_minimal()
#'
#' #
#' # Multiple circles by contraction
#' #
#'
#' # Start by circularizing 'y'
#' df_circ <- circularize(df, y_col = "y")
#'
#' # Contract '.circle_x' and 'y' towards the centroid
#' # To contract with multiple multipliers at once,
#' # we wrap the call in purrr::map_dfr
#' df_expanded <- purrr::map_dfr(
#'   .x = 1:10 / 10,
#'   .f = function(mult) {
#'     expand_distances(
#'       data = df_circ,
#'       cols = c(".circle_x", "y"),
#'       multiplier = mult,
#'       origin_fn = centroid,
#'       overwrite = TRUE
#'     )
#'   }
#' )
#' df_expanded
#'
#' df_expanded %>%
#'   ggplot(aes(
#'     x = .circle_x_expanded, y = y_expanded,
#'     color = .degrees, alpha = .multiplier
#'   )) +
#'   geom_point() +
#'   theme_minimal()
circularize <- function(data,
                        y_col = NULL,
                        .min = NULL,
                        .max = NULL,
                        offset_x = 0,
                        keep_original = TRUE,
                        x_col_name = ".circle_x",
                        degrees_col_name = ".degrees",
                        origin_col_name = ".origin",
                        overwrite = FALSE) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_string(x_col_name, min.chars = 1, add = assert_collection)
  checkmate::assert_string(degrees_col_name, null.ok = TRUE, add = assert_collection)
  checkmate::assert_string(origin_col_name, null.ok = TRUE, add = assert_collection)
  checkmate::assert_number(.min, null.ok = TRUE, add = assert_collection)
  checkmate::assert_number(.max, null.ok = TRUE, add = assert_collection)
  checkmate::assert_number(offset_x, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  check_unique_colnames_(y_col, x_col_name, degrees_col_name, origin_col_name)
  check_overwrite_(data = data,
                  nm = x_col_name,
                  overwrite = overwrite)
  check_overwrite_(data = data,
                  nm = degrees_col_name,
                  overwrite = overwrite)
  check_overwrite_(data = data,
                  nm = origin_col_name,
                  overwrite = overwrite)
  # End of argument checks ####

  # Mutate with each multiplier
  multi_mutator_(
    data = data,
    mutate_fn = circularize_mutator_method_,
    check_fn = NULL,
    cols = y_col,
    suffix = "",
    overwrite = overwrite,
    force_df = TRUE,
    keep_original = keep_original,
    .min = .min,
    .max = .max,
    offset_x = offset_x,
    x_col_name = x_col_name,
    degrees_col_name = degrees_col_name,
    origin_col_name = origin_col_name
  )
}


circularize_mutator_method_ <- function(data,
                                        grp_id,
                                        cols,
                                        overwrite,
                                        .min,
                                        .max,
                                        offset_x,
                                        x_col_name,
                                        degrees_col_name,
                                        origin_col_name,
                                        ...) {

  col <- cols

  # Create tmp var names
  tmp_side_col <- create_tmp_var(data, tmp_var = ".side")
  tmp_index_col <- create_tmp_var(data)

  # Create temporary index for reordering later
  data[[tmp_index_col]] <- seq_len(nrow(data))

  # Order by column of interest
  data <- data[order(data[[col]]), , drop = FALSE]

  # Divide into sides (left/right)
  data[[tmp_side_col]] <-
    head(rep(c(1, 2), ceiling(nrow(data) / 2)), nrow(data))

  # Find minimum value
  if (is.null(.min)) {
    .min <- min(data[[col]])
  }

  # Find maximum value
  if (is.null(.max)) {
    .max <- max(data[[col]])
  }

  # Set range outliers no NA
  data_list <- split_range_outliers_(
    data = data,
    col = col,
    .min = .min,
    .max = .max
  )
  data <- data_list[["data"]]
  outliers <- data_list[["outliers"]]

  # Properties of circle
  diameter <- .max - .min
  radius <- diameter / 2
  origin <- .max - radius

  # y = r * sin(theta), x = r * cos(theta)
  # sin(theta) = y/r
  y_r <- (data[[col]] - origin) / radius
  # Truncate numbers slightly outside the scope of asin (i.e. -1, 1)
  y_r <- ifelse(is_between_(y_r, 1, 1 + 1e-10), 1, y_r)
  y_r <- ifelse(is_between_(y_r, -(1 + 1e-10), -1), -1, y_r)
  # Calculate angles in radians
  angle <- asin(y_r)

  # Add x coordinate column
  data[[x_col_name]] <- radius * cos(angle)

  # Negate x coordinates for left side
  data[[x_col_name]] <- ifelse(data[[tmp_side_col]] == 1,
    -data[[x_col_name]],
    data[[x_col_name]]
  )

  # Make range outliers NA
  outliers <- add_na_column_(data = outliers, col = x_col_name, overwrite = overwrite)

  if (!is.null(degrees_col_name)) {
    # Add degrees column
    data[[degrees_col_name]] <- radians_to_degrees(angle) - 90
    # Make it counterclockwise
    data[[degrees_col_name]] <- -1 * data[[degrees_col_name]]
    # Separate sides
    data[[degrees_col_name]] <- ifelse(data[[tmp_side_col]] == 2,
      360 - data[[degrees_col_name]],
      data[[degrees_col_name]]
    )

    # Shift values such that (max(x), 0) is 0/360 degrees
    data <- roll_values(
      data = data,
      cols = degrees_col_name,
      add = 90,
      .min = 0,
      .max = 360,
      suffix = "",
      range_col_name = NULL,
      overwrite = TRUE
    )

    # Add NA degrees column to outliers subset
    outliers <- add_na_column_(
      data = outliers,
      col = degrees_col_name,
      overwrite = overwrite)
  }

  if (!is.null(origin_col_name)) {
    # Add origin coordinates column
    data[[origin_col_name]] <- list_coordinates_(c(0, origin), c(x_col_name, col))

    # Add NA origin column to outliers subset
    outliers <- add_na_column_(
      data = outliers,
      col = origin_col_name,
      as_list = TRUE,
      overwrite = overwrite
    )
  }

  data <- dplyr::bind_rows(
    data, outliers
  )

  # Clean up
  data <- data[order(data[[tmp_index_col]]), , drop = FALSE]
  data[[tmp_index_col]] <- NULL
  data[[tmp_side_col]] <- NULL

  # Offset x
  data[[x_col_name]] <- data[[x_col_name]] + offset_x

  data
}
