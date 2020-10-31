

#   __________________ #< 3fd0cb2200e0387b2809b1ae9d410a40 ># __________________
#   Create cluster data                                                     ####


#' @title Generate n-dimensional clusters
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Generates \code{data.frame} (\code{tibble}) with clustered groups.
#'
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @param num_rows Number of rows.
#' @param num_cols Number of columns (dimensions).
#' @param num_clusters Number of clusters.
#' @param compactness How compact the clusters should be. A larger value leads to more compact clusters (on average).
#'
#'  Technically, it is passed to the \code{`multiplier`} argument in
#'  \code{\link[rearrr:cluster_groups]{cluster_groups()}} as \eqn{`0.1 / compactness`}.
#' @param generator Function to generate the numeric values.
#'
#'  Must have the \emph{number of values to generate} as its first (and only required) argument,
#'  as that is the only argument we pass to it.
#' @param name_prefix Prefix string for naming columns.
#' @param cluster_col_name Name of cluster factor.
#' @export
#' @return \code{data.frame} (\code{tibble}) with the clustered columns and the cluster grouping factor.
#' @details
#'  \itemize{
#'   \item{Generates \code{data.frame} with random values using the \code{`generator`}.}
#'   \item{Divides the rows into groups (the clusters).}
#'   \item{Contracts the distance from each data point to the centroid of its group.}
#'   \item{Performs MinMax scaling such that the scale of the data points is similar to the generated data.}
#'  }
#' @family clustering functions
#' @examples
#' # Attach packages
#' library(rearrr)
#' library(dplyr)
#' library(ggplot2)
#' library(plotly)
#'
#' # Set seed
#' set.seed(10)
#'
#' # Generate clusters
#' generate_clusters(num_rows = 20, num_cols = 3, num_clusters = 3, compactness = 1.6)
#' generate_clusters(num_rows = 20, num_cols = 5, num_clusters = 6, compactness = 2.5)
#'
#' # Generate clusters and plot them
#' # Tip: Call this multiple times
#' # to see the behavior of `generate_clusters()`
#' generate_clusters(
#'   num_rows = 50, num_cols = 2,
#'   num_clusters = 5, compactness = 1.6
#' ) %>%
#'   ggplot(
#'     aes(x = D1, y = D2, color = .cluster)
#'   ) +
#'   geom_point() +
#'   theme_minimal() +
#'   labs(x = "D1", y = "D2", color = "Cluster")
#'
#' #
#' # Plot clusters in 3d view
#' #
#'
#' # Generate clusters
#' clusters <- generate_clusters(
#'   num_rows = 50, num_cols = 3,
#'   num_clusters = 5, compactness = 1.6
#' )
#'
#' \dontrun{
#' # Plot 3d with plotly
#' plotly::plot_ly(
#'   x = clusters$D1,
#'   y = clusters$D2,
#'   z = clusters$D3,
#'   type = "scatter3d",
#'   mode = "markers",
#'   color = clusters$.cluster
#' )
#' }
generate_clusters <- function(num_rows,
                              num_cols,
                              num_clusters,
                              compactness = 1.6,
                              generator = runif,
                              name_prefix = "D",
                              cluster_col_name = ".cluster") {
  # Generate data frame
  data <- matrix(
    data = generator(num_rows * num_cols),
    nrow = num_rows,
    ncol = num_cols
  ) %>%
    as.data.frame(stringsAsFactors = FALSE)

  # Name columns
  cols <- paste0(name_prefix, seq_len(num_cols))
  colnames(data) <- cols

  # Create groups
  data <- ndist_windows_(
    data = data,
    num_windows = num_clusters,
    factor_name = cluster_col_name
  )

  # Cluster the groups
  data <- cluster_groups(
    data = data,
    cols = cols,
    group_cols = cluster_col_name,
    scale_min_fn = min,
    scale_max_fn = max,
    keep_centroids = FALSE,
    multiplier = .1 / compactness,
    suffix = "",
    keep_original = FALSE,
    overwrite = TRUE
  )

  data
}
