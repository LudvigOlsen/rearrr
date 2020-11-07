

#   __________________ #< ee19996f71df3837277bf4e59fa40814 ># __________________
#   Pair extremes                                                           ####


#' @title Pair extreme values and sort by the pairs
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  The values are paired/grouped such that the highest and lowest values
#'  form the first group, the second highest and the second lowest values
#'  form the second group, and so on.
#'  The values are then sorted by these groups/pairs.
#'
#'  When \code{`data`} has an uneven number of rows, the \code{`unequal_method`}
#'  determines which group should have only \code{1} element.
#'
#'  The \code{*_vec()} version takes and returns a \code{vector}.
#'
#'  \strong{Example}:
#'
#'  The column values:
#'
#'  \code{c(1, 2, 3, 4, 5, 6)}
#'
#'  Creates the \strong{sorting factor}:
#'
#'  \code{c(1, 2, 3, 3, 2, 1)}
#'
#'  And are \strong{ordered as}:
#'
#'  \code{c(1, 6, 2, 5, 3, 4)}
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family rearrange functions
#' @inheritParams extreme_pairing_rearranger_
#' @return
#'  The sorted \code{data.frame} (\code{tibble}) / \code{vector}.
#'  Optionally with the sorting factor added.
#'
#'  When \code{`data`} is a \code{vector} and \code{`keep_factors`} is \code{FALSE},
#'  the output will be a \code{vector}. Otherwise, a \code{data.frame}.
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
#'   "index" = 1:10,
#'   "A" = sample(1:10),
#'   "B" = runif(10),
#'   "C" = LETTERS[1:10],
#'   "G" = c(
#'     1, 1, 1, 2, 2,
#'     2, 3, 3, 3, 3
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Pair extreme indices (row numbers)
#' pair_extremes(df)
#'
#' # Pair extremes in each of the columns
#' pair_extremes(df, col = "A")$A
#' pair_extremes(df, col = "B")$B
#' pair_extremes(df, col = "C")$C
#'
#' # Shuffle the members pair-wise
#' pair_extremes(df, col = "A", shuffle_members = TRUE)
#'
#' # Shuffle the order of the pairs
#' pair_extremes(df, col = "A", shuffle_pairs = TRUE)
#'
#' # Use recursive pairing
#' pair_extremes(df, col = "A", num_pairings = 2)
#'
#' # Grouped by G
#' df %>%
#'   dplyr::select(G, A) %>% # For clarity
#'   dplyr::group_by(G) %>%
#'   pair_extremes(col = "A")
#'
#' # Plot the extreme pairs
#' plot(
#'   x = 1:10,
#'   y = pair_extremes(df, col = "B")$B
#' )
#' # With shuffled pair members (run a few times)
#' plot(
#'   x = 1:10,
#'   y = pair_extremes(df, col = "B", shuffle_members = TRUE)$B
#' )
#' # With shuffled pairs (run a few times)
#' plot(
#'   x = rep(1:5, each = 2),
#'   y = pair_extremes(df, col = "B", shuffle_pairs = TRUE)$B
#' )
pair_extremes <- function(data,
                          col = NULL,
                          unequal_method = "middle",
                          num_pairings = 1,
                          balance = "mean",
                          shuffle_members = FALSE,
                          shuffle_pairs = FALSE,
                          factor_name = ifelse(num_pairings == 1, ".pair", ".pairing"),
                          overwrite = FALSE) {
  extreme_pairing_rearranger_(
    data = data,
    col = col,
    unequal_method = unequal_method,
    num_pairings = num_pairings,
    balance = balance,
    shuffle_members = shuffle_members,
    shuffle_pairs = shuffle_pairs,
    factor_name = factor_name,
    overwrite = overwrite
  )
}

#' @rdname pair_extremes
#' @export
pair_extremes_vec <- function(data,
                              unequal_method = "middle",
                              num_pairings = 1,
                              balance = "mean",
                              shuffle_members = FALSE,
                              shuffle_pairs = FALSE){
  checkmate::assert(checkmate::check_vector(data, strict = TRUE),
                    checkmate::check_factor(data))
  pair_extremes(
    data = data,
    unequal_method = unequal_method,
    num_pairings = num_pairings,
    balance = balance,
    shuffle_members = shuffle_members,
    shuffle_pairs = shuffle_pairs,
    factor_name = NULL,
    overwrite = TRUE
  )
}
