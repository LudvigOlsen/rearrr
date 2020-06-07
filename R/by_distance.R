

#   __________________ #< b302a9a22d60de2964548d3fb69edf2d ># __________________
#   By distance                                                             ####



##  .................. #< d8417478f17fb09ccd87a81c78c0006b ># ..................
##  Closest to                                                              ####

#' @title Orders values by shortest distance to the target
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Values are ordered by how close they are from the target value.
#'
#'  \strong{Example}:
#'
#'  The column values:
#'
#'  \code{c(1, 2, 3, 4, 5}
#'
#'  and \code{target = 2}
#'
#'  are \strong{ordered as}:
#'
#'  \code{c(}\strong{\code{2}}\code{, 1, 3, 4, 5)}
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family rearrange functions
#' @inheritParams by_distance_rearranger
#' @return
#'  The sorted \code{data.frame} (\code{tibble}) / \code{vector}.
#' @examples
#' \donttest{
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
#'   "G" = c(1, 1, 1, 2, 2,
#'           2, 3, 3, 3, 3),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Furthest from the third row
#' closest_to(df, target = 3)$index
#'
#' # By each of the columns
#' closest_to(df, col = "A", target = 3)$A
#' closest_to(df, col = "A", target_fn = median)$A
#' closest_to(df, col = "B", target = 0.5)$B
#' closest_to(df, col = "B", target_fn = median)$B
#'
#' # Shuffle the elements with the same distance to the target
#' closest_to(df, col = "A", target_fn = median, shuffle_ties = TRUE)$A
#'
#' # Grouped by G
#' df %>%
#'   dplyr::select(G, A) %>%  # For clarity
#'   dplyr::group_by(G) %>%
#'   closest_to(col = "A", target_fn = median)
#'
#' # Plot the centered values
#' plot(
#'   x = 1:10,
#'   y = closest_to(df, col = "B", target_fn = median)$B
#' )
#' plot(
#'   x = 1:10,
#'   y = closest_to(df, col = "A", target_fn = median, shuffle_ties = TRUE)$A
#' )
#' }
closest_to <- function(data,
                       col = NULL,
                       target = NULL,
                       target_fn = NULL,
                       shuffle_ties = FALSE) {
  by_distance_rearranger(
    data = data,
    col = col,
    target = target,
    target_fn = target_fn,
    shuffle_ties = shuffle_ties,
    decreasing = FALSE
  )
}


##  .................. #< 367167a5b5eaf6fa777a992c4f250900 ># ..................
##  Farthest from                                                           ####

#' @title Orders values by longest distance to the target
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Values are ordered by how far they are from the target value.
#'
#'  \strong{Example}:
#'
#'  The column values:
#'
#'  \code{c(1, 2, 3, 4, 5}
#'
#'  and \code{target = 2}
#'
#'  are \strong{ordered as}:
#'
#'  \code{c(5, 4, 1, 3,} \strong{\code{2}}\code{)}
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family rearrange functions
#' @inheritParams by_distance_rearranger
#' @aliases farthest_from
#' @return
#'  The sorted \code{data.frame} (\code{tibble}) / \code{vector}.
#' @examples
#' \donttest{
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
#'   "G" = c(1, 1, 1, 2, 2,
#'           2, 3, 3, 3, 3),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Furthest from the third row
#' furthest_from(df, target = 3)$index
#'
#' # By each of the columns
#' furthest_from(df, col = "A", target = 3)$A
#' furthest_from(df, col = "A", target_fn = median)$A
#' furthest_from(df, col = "B", target = 0.5)$B
#' furthest_from(df, col = "B", target_fn = median)$B
#'
#' # Shuffle the elements with the same distance to the target
#' furthest_from(df, col = "A", target_fn = median, shuffle_ties = TRUE)$A
#'
#' # Grouped by G
#' df %>%
#'   dplyr::select(G, A) %>%  # For clarity
#'   dplyr::group_by(G) %>%
#'   furthest_from(col = "A", target_fn = median)
#'
#' # Plot the centered values
#' plot(
#'   x = 1:10,
#'   y = furthest_from(df, col = "B", target_fn = median)$B
#' )
#' plot(
#'   x = 1:10,
#'   y = furthest_from(df, col = "A", target_fn = median, shuffle_ties = TRUE)$A
#' )
#' }
furthest_from <- function(data,
                          col = NULL,
                          target = NULL,
                          target_fn = NULL,
                          shuffle_ties = FALSE){
  by_distance_rearranger(
    data = data,
    col = col,
    target = target,
    target_fn = target_fn,
    shuffle_ties = shuffle_ties,
    decreasing = TRUE
  )
}
