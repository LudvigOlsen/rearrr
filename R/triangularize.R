

#   __________________ #< c57f9a818c5fbbd07c256952a1b6699a ># __________________
#   Triangle                                                                ####


#' @title Create x-coordinates so the points form a triangle
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Create the x-coordinates for a \code{vector} of y-coordinates such that
#'  they form a triangle.
#'
#'  The data points are stochastically distributed based on edge lengths, why it might be preferable to
#'  set a random seed.
#'
#'  This will likely look most like a triangle when the y-coordinates are somewhat equally distributed,
#'  e.g. a uniform distribution.
#'
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @inheritParams hexagonalize
#' @export
#' @return \code{data.frame} (\code{tibble}) with the added x-coordinates and an identifier
#'  for the edge the data point is a part of.
#' @family forming functions
#' @examples
#' # Attach packages
#' library(rearrr)
#' library(dplyr)
#' library(purrr)
#' has_ggplot <- require(ggplot2)  # Attach if installed
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
#' # Triangularize 'y'
#' df_tri <- triangularize(df, y_col = "y")
#' df_tri
#'
#' # Plot triangle
#' if (has_ggplot){
#'   df_tri %>%
#'     ggplot(aes(x = .triangle_x, y = y, color = .edge)) +
#'     geom_point() +
#'     theme_minimal()
#' }
#'
#' #
#' # Grouped squaring
#' #
#'
#' # Triangularize 'y' for each group
#' # First cluster the groups a bit to move the
#' # triangles away from each other
#' df_tri <- df %>%
#'   cluster_groups(
#'     cols = "y",
#'     group_cols = "g",
#'     suffix = "",
#'     overwrite = TRUE
#'   ) %>%
#'   dplyr::group_by(g) %>%
#'   triangularize(
#'     y_col = "y",
#'     overwrite = TRUE
#'   )
#'
#' # Plot triangles
#' if (has_ggplot){
#'   df_tri %>%
#'     ggplot(aes(x = .triangle_x, y = y, color = g)) +
#'     geom_point() +
#'     theme_minimal()
#' }
#'
#' #
#' # Specifying minimum value
#' #
#'
#' # Specify minimum value manually
#' df_tri <- triangularize(df, y_col = "y", .min = -2)
#' df_tri
#'
#' # Plot triangle
#' if (has_ggplot){
#'   df_tri %>%
#'     ggplot(aes(x = .triangle_x, y = y, color = .edge)) +
#'     geom_point() +
#'     theme_minimal()
#' }
#'
#' #
#' # Multiple triangles by contraction
#' #
#'
#' \donttest{
#' # Start by squaring 'y'
#' df_tri <- triangularize(df, y_col = "y")
#'
#' # Contract '.triangle_x' and 'y' towards the centroid
#' # To contract with multiple multipliers at once,
#' # we wrap the call in purrr::map_dfr
#' df_expanded <- purrr::map_dfr(
#'   .x = 1:10 / 10,
#'   .f = function(mult) {
#'     expand_distances(
#'       data = df_tri,
#'       cols = c(".triangle_x", "y"),
#'       multiplier = mult,
#'       origin_fn = centroid,
#'       overwrite = TRUE
#'     )
#'   }
#' )
#' df_expanded
#'
#' if (has_ggplot){
#'   df_expanded %>%
#'     ggplot(aes(
#'       x = .triangle_x_expanded, y = y_expanded,
#'       color = .edge, alpha = .multiplier
#'     )) +
#'     geom_point() +
#'     theme_minimal()
#' }
#' }
triangularize <- function(data,
                          y_col = NULL,
                          .min = NULL,
                          .max = NULL,
                          offset_x = 0,
                          keep_original = TRUE,
                          x_col_name = ".triangle_x",
                          edge_col_name = ".edge",
                          overwrite = FALSE) {


  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_string(x_col_name, min.chars = 1, add = assert_collection)
  checkmate::assert_string(edge_col_name, null.ok = TRUE, add = assert_collection)
  checkmate::assert_number(.min, null.ok = TRUE, add = assert_collection)
  checkmate::assert_number(.max, null.ok = TRUE, add = assert_collection)
  checkmate::assert_number(offset_x, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  check_unique_colnames_(y_col, x_col_name, edge_col_name)
  check_overwrite_(data = data,
                   nm = x_col_name,
                   overwrite = overwrite)
  check_overwrite_(data = data,
                   nm = edge_col_name,
                   overwrite = overwrite)
  # End of argument checks ####

  # Mutate with each multiplier
  multi_mutator_(
    data = data,
    mutate_fn = triangularize_mutator_method_,
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
    edge_col_name = edge_col_name
  )
}

triangularize_mutator_method_ <- function(data,
                                          grp_id,
                                          cols,
                                          overwrite,
                                          .min,
                                          .max,
                                          offset_x,
                                          x_col_name,
                                          edge_col_name,
                                          suffix = NULL,
                                          ...) {
  col <- cols

  # Create tmp var names
  tmp_side_col <- create_tmp_var(data, tmp_var = ".side")
  tmp_index_col <- create_tmp_var(data)

  # Create temporary index for reordering later
  data[[tmp_index_col]] <- seq_len(nrow(data))

  # Order by column of interest
  data <- data[order(data[[col]]), , drop = FALSE]

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

  # Properties of triangle
  height <- .max - .min
  # Pythagoras comes in handy!
  side_length <- sqrt(2 * (height / 2)^2)
  width <- height

  # Dividing into sides (left/right)
  # As the two short sides together require more points than the long left-side
  # we need to distribute the points depending on the ratio between lengths of left and right
  # But, we still want to make sure that it alternates all the time between the two sides
  # so we don't get large holes in the lines.
  # So we make sure there's at least one of both sides every 3 data points
  # and randomly distribute the last (3rd) data point based on the ratio

  # Calculate the distribution of the last one-third of sides
  n_right <- round((nrow(data) / (2 * side_length + height)) * 2 * side_length)
  n_left <- nrow(data) - n_right
  excess_right <- n_right - nrow(data) / 3
  excess_left <- n_left - nrow(data) / 3

  # Divide into sides (left/right)
  # We sample based on the side lengths
  data[[tmp_side_col]] <-
    head(purrr::simplify(purrr::map(
      .x = seq_len(ceiling(nrow(data) / 2)),
      .f = function(x) {
        sample(c(1, 2, sample(
          x = c(1, 2),
          size = 1,
          prob = c(excess_left, excess_right)
        )), replace = FALSE)
      }
    )), nrow(data))

  # Section cutoffs
  midline <- (.max - (height / 2))

  # Get data points per section (top, bottom)
  top <-
    data[data[[col]] >= midline, ,
      drop = FALSE
    ]
  bottom <-
    data[data[[col]] < midline, ,
      drop = FALSE
    ]

  ## Create x-coordinate

  # Top section
  top[[x_col_name]] <-
    min_max_scale(
      top[[col]],
      new_min = width / 2,
      new_max = 0,
      old_min = midline,
      old_max = .max
    )

  # Bottom section
  bottom[[x_col_name]] <-
    min_max_scale(
      bottom[[col]],
      new_min = 0,
      new_max = width / 2,
      old_min = .min,
      old_max = midline
    )

  outliers <- add_na_column_(data = outliers, col = x_col_name, overwrite = overwrite)

  # Edge numbers
  if (!is.null(edge_col_name)){
    top[[edge_col_name]] <- ifelse(top[[tmp_side_col]] == 1, 3, 1)
    bottom[[edge_col_name]] <- ifelse(bottom[[tmp_side_col]] == 1, 3, 2)
    outliers <- add_na_column_(data = outliers, col = edge_col_name, overwrite = overwrite)
  }

  # Combine datasets
  new_data <- dplyr::bind_rows(
    top, bottom, outliers
  )

  # Push to sides
  new_data[[x_col_name]] <- ifelse(new_data[[tmp_side_col]] == 1,
    0,
    new_data[[x_col_name]]
  )

  # Clean up
  new_data <- new_data[order(new_data[[tmp_index_col]]), , drop = FALSE]
  new_data[[tmp_index_col]] <- NULL
  new_data[[tmp_side_col]] <- NULL

  if (!is.null(edge_col_name)){
    new_data[[edge_col_name]] <- factor(new_data[[edge_col_name]])
  }

  # Offset x
  new_data[[x_col_name]] <- new_data[[x_col_name]] + offset_x

  new_data
}
