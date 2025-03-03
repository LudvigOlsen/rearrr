

#   __________________ #< 45425d2ee7b52e512217af9ce04ee05b ># __________________
#   Transfer centroids                                                      ####


#' @title Transfer centroids from one data frame to another
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Given two \code{data.frames} with the same columns (and groupings),
#'  transfer the centroids from one to the other.
#'
#'  This is commonly used to restore the centroids after transforming the columns.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @param to_data \code{data.frame}.
#'
#'  Existing \code{`dplyr`} groups are ignored. Specify in \code{`group_cols`} instead.
#' @param from_data \code{data.frame} with the same columns (and groupings) as \code{`to_data`}.
#'
#'  Existing \code{`dplyr`} groups are ignored. Specify in \code{`group_cols`} instead.
#' @param cols Names of numeric columns to transfer centroids to.
#'  Must exist in both \code{`to_data`} and \code{`from_data`}.
#' @param group_cols Names of grouping columns.
#' @export
#' @family clustering functions
#' @return The \code{`to_data`} \code{data.frame} (\code{tibble}) with the
#'  centroids from the \code{`from_data`} \code{data.frame}.
#' @examples
#' # Attach packages
#' library(rearrr)
#' library(dplyr)
#'
#' # Set seed
#' set.seed(1)
#'
#' # Create a data frame
#' df <- data.frame(
#'   "x" = runif(20),
#'   "y" = runif(20),
#'   "g" = rep(1:4, each = 5)
#' )
#'
#' # Create another data frame with different x and y values
#' df2 <- df
#' df2$x <- runif(20)
#' df2$y <- runif(20)
#'
#' # Check centroids before transfer
#'
#' df %>%
#'   dplyr::group_by(g) %>%
#'   dplyr::summarize_all(mean)
#'
#' df2 %>%
#'   dplyr::group_by(g) %>%
#'   dplyr::summarize_all(mean)
#'
#' # Now let's transfer the centroids from df to df2
#'
#' df3 <- transfer_centroids(
#'   to_data = df2,
#'   from_data = df,
#'   cols = c("x", "y"),
#'   group_cols = "g"
#' )
#'
#' # Check that the transfer gave us the same centroids as df
#' df3 %>%
#'   dplyr::group_by(g) %>%
#'   dplyr::summarize_all(mean)
transfer_centroids <- function(to_data,
                               from_data,
                               cols,
                               group_cols = NULL) {
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(to_data,
    min.cols = 1,
    min.rows = 1,
    add = assert_collection
  )
  checkmate::assert_data_frame(from_data,
    min.cols = 1,
    min.rows = 1,
    add = assert_collection
  )
  checkmate::assert_character(cols,
    any.missing = FALSE,
    min.len = 1,
    add = assert_collection
  )
  checkmate::assert_character(
    group_cols,
    any.missing = FALSE,
    min.len = 1,
    null.ok = TRUE,
    add = assert_collection
  )
  checkmate::reportAssertions(assert_collection)
  cols_intersection <-
    intersect(colnames(to_data), colnames(from_data))
  if (length(cols_intersection) != ncol(to_data) ||
    length(cols_intersection) != ncol(from_data)) {
    assert_collection$push("'to_data' and 'from_data' must have the exact same columns.")
  }
  if (length(intersect(cols, group_cols)) > 0) {
    assert_collection$push("some names in 'cols' were also in 'group_cols'.")
  }
  checkmate::reportAssertions(assert_collection)
  if (length(setdiff(cols, colnames(to_data))) > 0) {
    assert_collection$push("some names in 'cols' were not columns in 'to_data'.")
  }
  if (!is.null(group_cols) &&
    length(setdiff(group_cols, colnames(to_data))) > 0) {
    assert_collection$push("some names in 'group_cols' were not columns in 'to_data'.")
  }
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Ungroup data frames
  to_data <- dplyr::ungroup(to_data)
  from_data <- dplyr::ungroup(from_data)

  # Group data frames if specified
  if (!is.null(group_cols)) {
    to_data <- dplyr::group_by(to_data, !!!rlang::syms(group_cols))
    from_data <-
      dplyr::group_by(from_data, !!!rlang::syms(group_cols))
  }

  # Find from centroids
  from_centroids <- from_data %>%
    dplyr::select(dplyr::all_of(c(group_cols, cols))) %>%
    dplyr::summarise_all(mean)

  # Find centroids in the new data
  to_centroids <- to_data %>%
    dplyr::select(dplyr::all_of(c(group_cols, cols))) %>%
    dplyr::summarise_all(mean)

  # Extract summarized group columns
  from_group_columns <- from_centroids %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(group_cols))

  # Extract summarized group columns
  to_group_columns <- to_centroids %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(group_cols))

  # Make sure the group columns are the same in
  # both summaries
  if (!all.equal(
    as.data.frame(from_group_columns, stringsAsFactors = FALSE),
    as.data.frame(to_group_columns, stringsAsFactors = FALSE),
    ignore_row_order = FALSE
  )) {
    stop("The summarized group columns from the two datasets are not equal.")
  }

  # Remove group columns
  from_centroids <- from_centroids %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(cols))

  # Remove group columns
  to_centroids <- to_centroids %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(cols))

  # How much to move centroids per dimension
  distances <- from_centroids - to_centroids

  # Group by group columns
  distances <- dplyr::bind_cols(from_group_columns, distances) %>%
    dplyr::group_by(!!!rlang::syms(group_cols))

  # Move centroids!
  to_data[, cols] <- purrr::map2_dfr(
    .x = split(x = to_data, f = dplyr::group_indices(to_data)),
    .y = split(x = distances, f = dplyr::group_indices(distances)),
    .f = ~ {
      .x[, cols, drop = FALSE] + .y[rep(1, nrow(.x)), cols, drop = FALSE]
    }
  )

  # Convert to tibble and return
  to_data %>%
    dplyr::as_tibble()
}
