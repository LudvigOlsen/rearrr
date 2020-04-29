

#   __________________ #< 9839bd1092dbb2df7eddcf5af55ff3f2 ># __________________
#   Positioning                                                             ####


##  .................. #< 517be4e0a4cdf05713d79ec1bc5e9e50 ># ..................
##  Position max wrapper                                                    ####

#' @title Positions the highest values with values decreasing around it
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  The highest value is positioned at the given index/quantile with the other
#'  values decreasing around it.
#'
#'  \strong{Example}:
#'
#'  The column values:
#'
#'  \code{c(1, 2, 3, 4, }\strong{\code{5}}\code{)}
#'
#'  and \code{position = 2}
#'
#'  are \strong{ordered as}:
#'
#'  \code{c(3,} \strong{\code{5}}\code{, 4, 2, 1)}
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family rearrange functions
#' @inheritParams rearrange
#' @return
#'  The sorted \code{data frame} / \code{vector}.
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
#'   "C" = LETTERS[1:10],
#'   "G" = c(1, 1, 1, 2, 2,
#'           2, 3, 3, 3, 3),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Position the highest index (row number)
#' position_max(df, position = 3)$index
#' position_max(df, position = 8)$index
#'
#' # Position the maximum value in each of the columns
#' position_max(df, col = "A", position = 3)$A
#' position_max(df, col = "B", position = 3)$B
#' position_max(df, col = "C", position = 3)$C
#'
#' # Randomize which elements are left and right of the position
#' position_max(df, col = "A", position = 3, shuffle_members = TRUE)$A
#'
#' # Grouped by G
#' df %>%
#'   dplyr::select(G, A) %>%  # For clarity
#'   dplyr::group_by(G) %>%
#'   position_max(col = "A", position = 2)
#'
#' # Plot the rearranged values
#' plot(x = 1:10, y = position_max(df, col = "B", position = 3)$B)
#' plot(x = 1:10, y = position_max(df, col = "B", position = 3, shuffle_members = TRUE)$B)
#' }
position_max <- function(data,
                         col = NULL,
                         position = NULL,
                         shuffle_members = FALSE) {
  rearrange(
    data = data,
    col = col,
    position = position,
    method = "position_max",
    shuffle_members = shuffle_members,
    keep_factor = FALSE
  )
}


##  .................. #< 9c59547263f79174341cdf66a8365a51 ># ..................
##  Position min wrapper                                                    ####

#' @title Positions the lowest value with values increasing around it
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  The lowest value is positioned at the given index/quantile with the other
#'  values increasing around it.
#'
#'  \strong{Example}:
#'
#'  The column values:
#'
#'  \code{c(}\strong{\code{1}}\code{, 2, 3, 4, 5)}
#'
#'  and \code{position = 2}
#'
#'  are \strong{ordered as}:
#'
#'  \code{c(3,} \strong{\code{1}}\code{, 2, 4, 5)}
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family rearrange functions
#' @inheritParams rearrange
#' @return
#'  The sorted \code{data frame} / \code{vector}.
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
#'   "C" = LETTERS[1:10],
#'   "G" = c(1, 1, 1, 2, 2,
#'           2, 3, 3, 3, 3),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Position the smallest index (row number)
#' position_min(df, position = 3)$index
#' position_min(df, position = 8)$index
#'
#' # Position the minimum value in each of the columns
#' position_min(df, col = "A", position = 3)$A
#' position_min(df, col = "B", position = 3)$B
#' position_min(df, col = "C", position = 3)$C
#'
#' # Randomize which elements are left and right of the position
#' position_min(df, col = "A", position = 3, shuffle_members = TRUE)$A
#'
#' # Grouped by G
#' df %>%
#'   dplyr::select(G, A) %>%  # For clarity
#'   dplyr::group_by(G) %>%
#'   position_min(col = "A", position = 2)
#'
#' # Plot the rearranged values
#' plot(x = 1:10, y = position_min(df, col = "B", position = 3)$B)
#' plot(x = 1:10, y = position_min(df, col = "B", position = 3, shuffle_members = TRUE)$B)
#' }
position_min <- function(data,
                         col = NULL,
                         position = NULL,
                         shuffle_members = FALSE) {
  rearrange(
    data = data,
    col = col,
    position = position,
    method = "position_min",
    shuffle_members = shuffle_members,
    keep_factor = FALSE
  )
}
