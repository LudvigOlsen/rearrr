

#   __________________ #< e5a2cbb7fe60438112c2447bf1135059 ># __________________
#   Hexagonalize                                                            ####


#' @title Create x-coordinates so the points form a hexagon
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Create the x-coordinates for a \code{vector} of y-coordinates such that
#'  they form a hexagon.
#'
#'  This will likely look most like a hexagon when the y-coordinates are somewhat equally distributed,
#'  e.g. a uniform distribution.
#'
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @param y_col Name of column in \code{`data`} with y-coordinates to create x-coordinates for.
#' @param .min Minimum y-coordinate. If \code{NULL}, it is inferred by the given y-coordinates.
#' @param .max Maximum y-coordinate. If \code{NULL}, it is inferred by the given y-coordinates.
#' @param offset_x Value to offset the x-coordinates by.
#' @param x_col_name Name of new column with the x-coordinates.
#' @param edge_col_name Name of new column with the edge identifiers. If \code{NULL}, no column is added.
#'
#'  Numbering is clockwise and starts at the upper-right edge.
#' @export
#' @return \code{data.frame} (\code{tibble}) with the added x-coordinates and an identifier
#'  for the edge the data point is a part of.
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
#' # Hexagonalize 'y'
#' df_hex <- hexagonalize(df, y_col = "y")
#' df_hex
#'
#' # Plot hexagon
#' df_hex %>%
#'   ggplot(aes(x = .hexagon_x, y = y, color = .edge)) +
#'   geom_point() +
#'   theme_minimal()
#'
#' #
#' # Grouped hexagonalization
#' #
#'
#' # Hexagonalize 'y' for each group
#' # First cluster the groups a bit to move the
#' # hexagons away from each other
#' df_hex <- df %>%
#'   cluster_groups(
#'     cols = "y",
#'     group_cols = "g",
#'     suffix = "",
#'     overwrite = TRUE
#'   ) %>%
#'   dplyr::group_by(g) %>%
#'   hexagonalize(
#'     y_col = "y",
#'     overwrite = TRUE
#'   )
#'
#' # Plot hexagons
#' df_hex %>%
#'   ggplot(aes(x = .hexagon_x, y = y, color = g)) +
#'   geom_point() +
#'   theme_minimal()
#'
#' #
#' # Specifying minimum value
#' #
#'
#' # Specify minimum value manually
#' df_hex <- hexagonalize(df, y_col = "y", .min = -2)
#' df_hex
#'
#' # Plot hexagon
#' df_hex %>%
#'   ggplot(aes(x = .hexagon_x, y = y, color = .edge)) +
#'   geom_point() +
#'   theme_minimal()
#'
#' #
#' # Multiple hexagons by contraction
#' #
#'
#' # Start by hexagonalizing 'y'
#' df_hex <- hexagonalize(df, y_col = "y")
#'
#' # Contract '.hexagon_x' and 'y' towards the centroid
#' # To contract with multiple multipliers at once,
#' # we wrap the call in purrr::map_dfr
#' df_expanded <- purrr::map_dfr(
#'   .x = c(1, 0.75, 0.5, 0.25, 0.125),
#'   .f = function(mult) {
#'     expand_distances(
#'       data = df_hex,
#'       cols = c(".hexagon_x", "y"),
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
#'     x = .hexagon_x_expanded, y = y_expanded,
#'     color = .edge, alpha = .multiplier
#'   )) +
#'   geom_point() +
#'   theme_minimal()
hexagonalize <- function(data,
                         y_col = NULL,
                         .min = NULL,
                         .max = NULL,
                         offset_x = 0,
                         keep_original = TRUE,
                         x_col_name = ".hexagon_x",
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
    mutate_fn = hexagonalize_mutator_method_,
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


hexagonalize_mutator_method_ <- function(data,
                                         grp_id,
                                         cols,
                                         overwrite,
                                         .min,
                                         .max,
                                         offset_x,
                                         x_col_name,
                                         edge_col_name,
                                         ...) {

  # Is a single column
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

  # Properties of hexagon
  height <- .max - .min
  side_length <- height / 2
  # Pythagoras comes in handy!
  width <- sqrt(side_length^2 - (side_length / 2)^2) * 2

  # Section cutoffs
  middle_upper <- (.max - (side_length / 2))
  middle_lower <- (.min + (side_length / 2))

  # Get data points per section (top, middle, bottom)
  top <-
    data[data[[col]] >= middle_upper, ,
      drop = FALSE
    ]
  bottom <-
    data[data[[col]] <= middle_lower, ,
      drop = FALSE
    ]
  middle <-
    data[is_between_(x = data[[col]], a = middle_lower, b = middle_upper), ,
      drop = FALSE
    ]

  ## Create x-coordinate

  # Top section
  top[[x_col_name]] <-
    min_max_scale(
      top[[col]],
      new_min = width / 2,
      new_max = 0,
      old_min = middle_upper,
      old_max = .max
    )

  # Middle section
  middle[[x_col_name]] <- width / 2

  # Bottom section
  bottom[[x_col_name]] <-
    min_max_scale(
      bottom[[col]],
      new_min = 0,
      new_max = width / 2,
      old_min = .min,
      old_max = middle_lower
    )

  outliers <- add_na_column_(data = outliers, col = x_col_name, overwrite = overwrite)

  # Edge numbers
  if (!is.null(edge_col_name)){
    top[[edge_col_name]] <- ifelse(top[[tmp_side_col]] == 1, 6, 1)
    middle[[edge_col_name]] <- ifelse(middle[[tmp_side_col]] == 1, 5, 2)
    bottom[[edge_col_name]] <- ifelse(bottom[[tmp_side_col]] == 1, 4, 3)
    outliers <- add_na_column_(data = outliers, col = edge_col_name, overwrite = overwrite)
  }


  # Combine datasets
  new_data <- dplyr::bind_rows(
    top, middle, bottom, outliers
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
