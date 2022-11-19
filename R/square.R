

#   __________________ #< 16dfb8d7e0b0483ba9b962677c43324b ># __________________
#   Square                                                                  ####


#' @title Create x-coordinates so the points form a square
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Create the x-coordinates for a \code{vector} of y-coordinates such that
#'  they form a rotated square.
#'
#'  This will likely look most like a square when the y-coordinates are somewhat equally distributed,
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
#' # Square 'y'
#' df_sq <- square(df, y_col = "y")
#' df_sq
#'
#' # Plot square
#' if (has_ggplot){
#'   df_sq %>%
#'     ggplot(aes(x = .square_x, y = y, color = .edge)) +
#'     geom_point() +
#'     theme_minimal()
#' }
#'
#' #
#' # Grouped squaring
#' #
#'
#' # Square 'y' for each group
#' # First cluster the groups a bit to move the
#' # squares away from each other
#' df_sq <- df %>%
#'   cluster_groups(
#'     cols = "y",
#'     group_cols = "g",
#'     suffix = "",
#'     overwrite = TRUE
#'   ) %>%
#'   dplyr::group_by(g) %>%
#'   square(
#'     y_col = "y",
#'     overwrite = TRUE
#'   )
#'
#' # Plot squares
#' if (has_ggplot){
#'   df_sq %>%
#'     ggplot(aes(x = .square_x, y = y, color = g)) +
#'     geom_point() +
#'     theme_minimal()
#' }
#'
#' #
#' # Specifying minimum value
#' #
#'
#' # Specify minimum value manually
#' df_sq <- square(df, y_col = "y", .min = -2)
#' df_sq
#'
#' # Plot square
#' if (has_ggplot){
#'   df_sq %>%
#'     ggplot(aes(x = .square_x, y = y, color = .edge)) +
#'     geom_point() +
#'     theme_minimal()
#' }
#'
#' #
#' # Multiple squares by contraction
#' #
#'
#' # Start by squaring 'y'
#' df_sq <- square(df, y_col = "y")
#'
#' # Contract '.square_x' and 'y' towards the centroid
#' # To contract with multiple multipliers at once,
#' # we wrap the call in purrr::map_dfr
#' df_expanded <- purrr::map_dfr(
#'   .x = c(1, 0.75, 0.5, 0.25, 0.125),
#'   .f = function(mult) {
#'     expand_distances(
#'       data = df_sq,
#'       cols = c(".square_x", "y"),
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
#'       x = .square_x_expanded, y = y_expanded,
#'       color = .edge, alpha = .multiplier
#'     )) +
#'     geom_point() +
#'     theme_minimal()
#' }
square <- function(data,
                   y_col = NULL,
                   .min = NULL,
                   .max = NULL,
                   offset_x = 0,
                   keep_original = TRUE,
                   x_col_name = ".square_x",
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
    mutate_fn = square_mutator_method_,
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

# Note: It's a rotated square (so diamond'ish)
# so height and width are the diagonals of the square
square_mutator_method_ <- function(data,
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

  # Properties of square
  height <- .max - .min
  width <- height

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
      old_max = .max,
      na.rm = TRUE
    )

  # Bottom section
  bottom[[x_col_name]] <-
    min_max_scale(
      bottom[[col]],
      new_min = 0,
      new_max = width / 2,
      old_min = .min,
      old_max = midline,
      na.rm = TRUE
    )

  outliers <- add_na_column_(data = outliers, col = x_col_name, overwrite = overwrite)

  # Edge numbers
  if (!is.null(edge_col_name)){
    top[[edge_col_name]] <- ifelse(top[[tmp_side_col]] == 1, 4, 1)
    bottom[[edge_col_name]] <- ifelse(bottom[[tmp_side_col]] == 1, 3, 2)
    outliers <- add_na_column_(data = outliers, col = edge_col_name, overwrite = overwrite)
  }

  # Combine datasets
  new_data <- dplyr::bind_rows(
    top, bottom, outliers
  )

  # Push to sides
  new_data[[x_col_name]] <- ifelse(new_data[[tmp_side_col]] == 1,
    -new_data[[x_col_name]],
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
