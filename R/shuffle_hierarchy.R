

#   __________________ #< 563edba79bb23631ec71006e35dbc06a ># __________________
#   Shuffle hierarchy                                                       ####


#' @title Shuffle multi-column hierarchy of groups
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Shuffles a tree/hierarchy of groups, one column at a time.
#'  The levels in the last ("leaf") column are shuffled first, then the second-last column, and so on.
#'  Elements of the same group are ordered sequentially.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family rearrange functions
#' @param data \code{data.frame}.
#' @param group_cols Names of columns making up the group hierarchy.
#'  The last column is the \emph{leaf} and is shuffled first (if also in \code{`cols_to_shuffle`}).
#' @param cols_to_shuffle Names of columns to shuffle hierarchically.
#'  By default, all the \code{`group_cols`} are shuffled.
#' @param leaf_has_groups Whether the leaf column contains groups or values. (Logical)
#'
#'  When the elements are \emph{group identifiers}, they are ordered sequentially and shuffled together.
#'
#'  When the elements are \emph{values}, they are simply shuffled.
#' @return
#'  The shuffled \code{data.frame} (\code{tibble}).
#' @examples
#' # Attach packages
#' library(rearrr)
#' library(dplyr)
#'
#' df <- data.frame(
#'   'a' = rep(1:4, each = 4),
#'   'b' = rep(1:8, each = 2),
#'   'c' = 1:16
#' )
#'
#' # Set seed for reproducibility
#' set.seed(2)
#'
#' # Shuffle all columns
#' shuffle_hierarchy(df, group_cols = c('a', 'b', 'c'))
#'
#' # Don't shuffle 'b' but keep grouping by it
#' # So 'c' will be shuffled within each group in 'b'
#' shuffle_hierarchy(
#'   data = df,
#'   group_cols = c('a', 'b', 'c'),
#'   cols_to_shuffle = c('a', 'c')
#' )
#'
#' # Shuffle 'b' as if it's not a group column
#' # so elements are independent within their group
#' # (i.e. same-valued elements are not necessarily ordered sequentially)
#' shuffle_hierarchy(df, group_cols = c('a', 'b'), leaf_has_groups = FALSE)
shuffle_hierarchy <- function(data, group_cols, cols_to_shuffle = group_cols, leaf_has_groups = TRUE) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(data, add = assert_collection)
  checkmate::assert_character(group_cols, min.chars = 1, any.missing = FALSE,
                              unique = TRUE, add = assert_collection)
  checkmate::assert_character(cols_to_shuffle, min.chars = 1, any.missing = FALSE,
                              unique = TRUE, add = assert_collection)
  checkmate::assert_flag(leaf_has_groups, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  if (length(setdiff(cols_to_shuffle, group_cols))){
    assert_collection$push("'cols_to_shuffle' can only contain names that are also in 'group_cols'.")
  }
  if (dplyr::is_grouped_df(data)){
    warning("'data' is already grouped. Those groups will be ignored.")
  }
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Extract leaf column
  leaf_col <- tail(group_cols, 1)

  # Get environment to update 'data' in
  data_env <- environment()

  # Order 'data' by one column at the time
  plyr::l_ply(rev(seq_along(group_cols)), function(gc_idx) {
    if (group_cols[[gc_idx]] %in% cols_to_shuffle) {
      # Find the grouping columns to apply
      to_group_by <- head(group_cols, n = gc_idx - 1)

      # Group 'data'
      data <- dplyr::group_by(data, !!!rlang::syms(to_group_by))

      if (leaf_col == group_cols[[gc_idx]] &&
          !isTRUE(leaf_has_groups)) {
        # Shuffle leaf where each element is independent
        # (not in a group)
        data <- data %>%
          dplyr::sample_frac()
      } else {
        # Shuffle by unique group levels
        data <- run_by_group(data,
                             fn = shuffle_uniques_,
                             col = group_cols[[gc_idx]])
      }

      # Assign to parent environment
      data_env[["data"]] <- data
    }
  })

  # Ungroup and return
  data %>%
    dplyr::ungroup()

}

# Extract unique values in the column
# and sort 'data' by their shuffled index
shuffle_uniques_ <- function(data, grp_id, col){
  tmp_index <- create_tmp_var(data)

  # Extract and shuffle unique group levels
  uniques <- unique(data[[col]]) %>%
    tibble::enframe(name = NULL, value = col) %>%
    dplyr::sample_frac() %>%
    dplyr::mutate(!!tmp_index := dplyr::row_number())

  # Order 'data' by the shuffled group levels
  data %>%
    dplyr::left_join(uniques, by = col) %>%
    dplyr::arrange(!!as.name(tmp_index)) %>%
    base_deselect_(cols = tmp_index)
}
