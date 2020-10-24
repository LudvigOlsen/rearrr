

#   __________________ #< 44cb44f704ab50637f9d41e676fa70d7 ># __________________
#   Cluster groups                                                          ####


#' @title Move data points into clusters
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Transform values such that the elements in each group move closer to their centroid.
#'
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @param data \code{data.frame}. If \code{`group_cols`} is \code{NULL}, it must be grouped with
#'  \code{\link[dplyr:group_by]{dplyr::group_by()}}.
#' @param cols Names of columns in \code{`data`} to mutate.
#'  Each column is considered a dimension to contract distances in.
#' @param group_cols Names of grouping columns in \code{`data`}. Must be distinct from the names in \code{`cols`}.
#'
#'  If \code{NULL} and \code{`data`} is grouped, those groups are used instead.
#' @param scale_min_fn,scale_max_fn Function to find the minimum/maximum value in
#'  the original data when rescaling the contracted data.
#'
#'  \strong{Input}: A \code{numeric vector}.
#'
#'  \strong{Output}: A \code{numeric scalar}.
#' @param keep_centroids Whether to ensure the clusters have their original centroids. (Logical)
#' @param multiplier Numeric constant to multiply the distance to the group centroid by. A smaller value
#'  makes the clusters more compact and vice versa.
#' @inheritParams multi_mutator_
#' @export
#' @return \code{data.frame} (\code{tibble}) with the clustered columns.
#' @details
#'  \itemize{
#'   \item{Contracts the distance from each data point to the centroid of its group.}
#'   \item{Performs MinMax scaling such that the scale of the data points is \emph{similar} to the original data.}
#'   \item{If enabled (not default), the centroids are moved to the original centroids.}
#'  }
#' @family mutate functions
#' @family clustering functions
#' @examples
#' # Attach packages
#' library(rearrr)
#' library(dplyr)
#' library(ggplot2)
#'
#' # Set seed
#' set.seed(2)
#'
#' # Create a data frame
#' df <- data.frame(
#'   "x" = runif(50),
#'   "y" = runif(50),
#'   "z" = runif(50),
#'   "g" = rep(c(1, 2, 3, 4, 5), each = 10)
#' )
#'
#' # Move the data points into clusters
#' cluster_groups(df,
#'   cols = c("x", "y"),
#'   group_col = "g"
#' )
#' cluster_groups(df,
#'   cols = c("x", "y"),
#'   group_col = "g",
#'   multiplier = 0.1
#' )
#' cluster_groups(df,
#'   cols = c("x"),
#'   group_col = "g",
#'   multiplier = 0.1
#' )
#'
#' #
#' # Plotting clusters
#' #
#'
#' # Cluster x and y for each group in g
#' df_clustered <- cluster_groups(
#'   data = df,
#'   cols = c("x", "y"),
#'   group_col = "g"
#' )
#'
#' # Plot the clusters over the original data points
#' # As we work with random data, the cluster might overlap
#' ggplot(
#'   df_clustered,
#'   aes(x = x_clustered, y = y_clustered, color = factor(g))
#' ) +
#'   # Original data
#'   geom_point(aes(x = x, y = y), alpha = 0.3, size = 0.8) +
#'   # Clustered data
#'   geom_point() +
#'   theme_minimal() +
#'   labs(x = "x", y = "y", color = "g")
#'
#' #
#' # Maintain original group centroids
#' #
#'
#' df_clustered <- cluster_groups(
#'   data = df,
#'   cols = c("x", "y"),
#'   group_col = "g",
#'   keep_centroids = TRUE
#' )
#'
#' # Plot the clusters over the original data points
#' # As we work with random data, the cluster might overlap
#' ggplot(
#'   df_clustered,
#'   aes(x = x_clustered, y = y_clustered, color = factor(g))
#' ) +
#'   # Original data
#'   geom_point(aes(x = x, y = y), alpha = 0.3, size = 0.8) +
#'   # Clustered data
#'   geom_point() +
#'   theme_minimal() +
#'   labs(x = "x", y = "y", color = "g")
#'
#' #
#' # Three dimensions
#' #
#'
#' # Cluster in 3d
#' df_clustered <- cluster_groups(
#'   data = df,
#'   cols = c("x", "y", "z"),
#'   group_col = "g"
#' )
#'
#' \dontrun{
#' # Plot 3d with plotly
#' plotly::plot_ly(
#'   x = df_clustered$x_clustered,
#'   y = df_clustered$y_clustered,
#'   z = df_clustered$z_clustered,
#'   type = "scatter3d",
#'   mode = "markers",
#'   color = df_clustered$g
#' )
#' }
cluster_groups <- function(data,
                           cols,
                           group_cols = NULL,
                           scale_min_fn = function(x) {
                             quantile(x, 0.025)
                           },
                           scale_max_fn = function(x) {
                             quantile(x, 0.975)
                           },
                           keep_centroids = FALSE,
                           multiplier = 0.05,
                           suffix = "_clustered",
                           keep_original = TRUE,
                           overwrite = FALSE) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(data, add = assert_collection)
  checkmate::assert_character(
    cols,
    min.len = 1,
    min.chars = 1,
    unique = TRUE,
    add = assert_collection
  )
  checkmate::assert_character(
    group_cols,
    min.len = 1,
    min.chars = 1,
    unique = TRUE,
    null.ok = TRUE,
    add = assert_collection
  )
  checkmate::assert_function(scale_min_fn, add = assert_collection)
  checkmate::assert_function(scale_max_fn, add = assert_collection)
  checkmate::assert_flag(keep_centroids, add = assert_collection)
  checkmate::assert_number(multiplier, add = assert_collection)
  checkmate::assert_flag(keep_original, add = assert_collection)
  checkmate::assert_string(suffix, add = assert_collection)
  checkmate::reportAssertions(assert_collection)

  checkmate::assert_names(colnames(data),
    must.include = c(cols, group_cols),
    add = assert_collection
  )
  checkmate::reportAssertions(assert_collection)

  # Ensure grouping
  if (!is.null(group_cols) &&
    length(intersect(group_cols, cols)) > 0) {
    assert_collection$push("'group_cols' cannot contain a column from 'cols'.")
    checkmate::reportAssertions(assert_collection)
  }
  if (!dplyr::is_grouped_df(data) && is.null(group_cols)) {
    assert_collection$push("when 'group_cols' is 'NULL', 'data' should be grouped.")
    checkmate::reportAssertions(assert_collection)
  }
  if (dplyr::is_grouped_df(data) && !is.null(group_cols)) {
    assert_collection$push("'data' is already grouped but 'group_cols' is not 'NULL'")
    checkmate::reportAssertions(assert_collection)
  }

  checkmate::reportAssertions(assert_collection)
  if (!isTRUE(overwrite)) {
    purrr::map(.x = cols, .f = ~ {
      check_overwrite_(data = data,
                       nm = paste0(.x, suffix),
                       overwrite = overwrite)
    })
  }
  # End of argument checks ####

  # Grouping
  if (!dplyr::is_grouped_df(data)) {
    data <- dplyr::group_by(data, !!!rlang::syms(group_cols))
  } else {
    group_cols <- colnames(dplyr::group_keys(data))
  }

  # Contract data
  expanded <- expand_distances(
    data = data,
    cols = cols,
    multiplier = multiplier,
    origin_fn = centroid,
    suffix = "",
    overwrite = TRUE,
    keep_original = keep_original,
    mult_col_name = NULL,
    origin_col_name = NULL
  )

  # MinMax scaling to restore original scale
  scaled <- plyr::llply(cols, function(cl) {
    min_max_scale(
      x = expanded[[cl]],
      new_min = scale_min_fn(data[[cl]]),
      new_max = scale_max_fn(data[[cl]])
    )
  }) %>%
    setNames(cols) %>%
    dplyr::bind_cols()

  # Collect columns
  clustered <- dplyr::bind_cols(
    scaled,
    expanded[, colnames(expanded) %ni% cols, drop = FALSE]
  )

  # Move centroids to original centroid coordinates
  if (isTRUE(keep_centroids)) {
    clustered <-
      transfer_centroids(
        to_data = clustered,
        from_data = data,
        cols = cols,
        group_cols = group_cols
      )
  }

  # Rename columns and select which columns to return
  if (suffix != "") {
    colnames(clustered) <-
      purrr::map_chr(.x = colnames(clustered), .f = ~ {
        ifelse(.x %in% cols, paste0(.x, suffix), .x)
      })
    if (isTRUE(keep_original)) {
      clustered <- dplyr::bind_cols(
        data[, cols, drop = FALSE],
        clustered
      )
    }
  } else if (!isTRUE(keep_original)) {
    exclude <- setdiff(colnames(data), colnames(clustered))
    if (length(exclude) > 0) {
      clustered <- clustered[, colnames(clustered) %ni% exclude, drop = FALSE]
    }
  }

  # Return clustered data
  clustered %>%
    dplyr::as_tibble()
}
