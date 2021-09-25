

#   __________________ #< f2534ad1fa5fa1ed436919456b424b60 ># __________________
#   Triplet extremes                                                        ####


# TODO In groupdata2::fold when it cannot create more unique fold columns
# we can do the numeric balancing with triple_extremes as not operating on
# the same pairs should allow more combinations with somewhat balanced splits!



#' @title Makes triplets of extreme values and sort by them
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  The values are grouped in three such that the first group is formed by
#'  the lowest and highest values and the value closest to the median,
#'  the second group is formed by the second lowest and second
#'  highest values and the value second closest to the median, and so on.
#'  The values are then sorted by these groups and their actual value.
#'
#'  When the number of rows/elements in \code{`data`} is not evenly divisible by three,
#'  the \code{`unequal_method_1`} (single excessive element) and
#'  \code{`unequal_method_2`} (two excessive elements)
#'  determines which element(s) should form a smaller group.
#'  This group will be the first group \emph{in a given grouping} (see \code{`num_groupings`})
#'  with the identifier \code{1}.
#'
#'  The \code{*_vec()} version takes and returns a \code{vector}.
#'
#'  \strong{Example}:
#'
#'  The column values:
#'
#'  \code{c(1, 2, 3, 4, 5, 6)}
#'
#'  Are sorted in triplets as:
#'
#'  \code{c(1, 3, 6, 2, 4, 5)}
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family rearrange functions
#' @inheritParams extreme_triplet_grouping_rearranger_
#' @param unequal_method_1,unequal_method_2 Method for dealing with either
#'  a single excessive element (\code{`unequal_method_1`}) or two excessive elements (\code{`unequal_method_2`})
#'  when the number of rows/elements in \code{`data`} are not evenly divisible by three.
#'
#'  \code{`unequal_method_1`}: One of: \code{min}, \code{middle} or \code{max}.
#'
#'  \code{`unequal_method_2`}: Vector with two of: \code{min}, \code{middle} or \code{max}. Can be the same value twice.
#'
#'  Note: The excessive element(s) are extracted before triplet grouping. These elements
#'  are put in their own group and given group identifier \code{1}.
#'
#'  E.g. When \code{`unequal_method_2`} is \code{c("middle", "middle")} the two elements
#'  closest to the median are extracted.
#' @return
#'  The sorted \code{data.frame} (\code{tibble}) / \code{vector}.
#'  Optionally with the sorting factor added.
#'
#'  When \code{`data`} is a \code{vector} and \code{`keep_factors`} is \code{`FALSE`},
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
#'   "index" = 1:12,
#'   "A" = sample(1:12),
#'   "B" = runif(12),
#'   "C" = LETTERS[1:12],
#'   "G" = c(
#'     1, 1, 1, 1, 2, 2,
#'     2, 2, 3, 3, 3, 3
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Triplet group extreme indices (row numbers)
#' triplet_extremes(df)
#'
#' # Triplet group extremes in each of the columns
#' triplet_extremes(df, col = "A")$A
#' triplet_extremes(df, col = "B")$B
#' triplet_extremes(df, col = "C")$C
#'
#' # Shuffle the members triplet-wise
#' # The triplets maintain their order
#' # but the rows within each triplet are shuffled
#' triplet_extremes(df, col = "A", shuffle_members = TRUE)
#'
#' # Shuffle the order of the triplets
#' # The triplets are shuffled but
#' # the rows within each triplet maintain their order
#' triplet_extremes(df, col = "A", shuffle_triplets = TRUE)
#'
#' # Use recursive grouping
#' # Mostly meaningful with much larger datasets
#' # Order initial grouping by group identifiers
#' triplet_extremes(df, col = "A", num_groupings = 2)
#' # Order initial grouping by aggregate values
#' triplet_extremes(df, col = "A", num_groupings = 2, order_by_aggregates = TRUE)
#'
#' # Grouped by G
#' # Each G group only has 4 elements
#' # so it only creates 1 triplet and a group
#' # with the single excessive element
#' # per G group
#' df %>%
#'   dplyr::select(G, A) %>% # For clarity
#'   dplyr::group_by(G) %>%
#'   triplet_extremes(col = "A")
#'
#' # Plot the extreme triplets
#' plot(
#'   x = 1:12,
#'   y = triplet_extremes(df, col = "A")$A,
#'   col = as.character(rep(1:4, each = 3))
#' )
#' # With shuffled triplet members (run a few times)
#' plot(
#'   x = 1:12,
#'   y = triplet_extremes(df, col = "A", shuffle_members = TRUE)$A,
#'   col = as.character(rep(1:4, each = 3))
#' )
#' # With shuffled triplets (run a few times)
#' plot(
#'   x = rep(1:6, each = 2),
#'   y = triplet_extremes(df, col = "A", shuffle_triplets = TRUE)$A,
#'   col = as.character(rep(1:4, each = 3))
#' )
triplet_extremes <- function(data,
                             col = NULL,
                             middle_is = "middle",
                             unequal_method_1 = "middle",
                             unequal_method_2 = c("middle", "middle"),
                             num_groupings = 1,
                             balance = "mean",
                             order_by_aggregates = FALSE,
                             shuffle_members = FALSE,
                             shuffle_triplets = FALSE,
                             factor_name = ifelse(num_groupings == 1, ".triplet", ".tripleting"),
                             overwrite = FALSE) {
  extreme_triplet_grouping_rearranger_(
    data = data,
    col = col,
    middle_is = middle_is,
    unequal_method_1 = unequal_method_1,
    unequal_method_2 = unequal_method_2,
    num_groupings = num_groupings,
    balance = balance,
    order_by_aggregates = order_by_aggregates,
    shuffle_members = shuffle_members,
    shuffle_triplets = shuffle_triplets,
    factor_name = factor_name,
    overwrite = overwrite
  )
}

#' @rdname triplet_extremes
#' @export
triplet_extremes_vec <- function(data,
                                 middle_is = "middle",
                                 unequal_method_1 = "middle",
                                 unequal_method_2 = c("middle", "middle"),
                                 num_groupings = 1,
                                 balance = "mean",
                                 order_by_aggregates = FALSE,
                                 shuffle_members = FALSE,
                                 shuffle_triplets = FALSE){
  checkmate::assert(checkmate::check_vector(data, strict = TRUE),
                    checkmate::check_factor(data))
  extreme_triplet_grouping_rearranger_(
    data = data,
    middle_is = middle_is,
    unequal_method_1 = unequal_method_1,
    unequal_method_2 = unequal_method_2,
    num_groupings = num_groupings,
    balance = balance,
    order_by_aggregates = order_by_aggregates,
    shuffle_members = shuffle_members,
    shuffle_triplets = shuffle_triplets,
    factor_name = NULL,
    overwrite = TRUE
  )
}

