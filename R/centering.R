

#   __________________ #< 7348c017e5f43ae4e3925ec083944e54 ># __________________
#   Centering                                                               ####


##  .................. #< a7871fe876eb8675acb8ff2fc16200b3 ># ..................
##  Center max                                                              ####


#' @title Centers the highest value with values decreasing around it
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  The highest value is positioned in the middle with the other
#'  values decreasing around it.
#'
#'  \strong{Example}:
#'
#'  The column values:
#'
#'  \code{c(1, 2, 3, 4,} \strong{\code{5}}\code{)}
#'
#'  are \strong{ordered as}:
#'
#'  \code{c(1, 3,} \strong{\code{5}}\code{, 4, 2)}
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family rearrange functions
#' @inheritParams centering_rearranger_
#' @return
#'  The sorted \code{data.frame} (\code{tibble}) / \code{vector}.
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
#' # Center by the index (row numbers)
#' center_max(df)
#'
#' # Center by each of the columns
#' center_max(df, col = "A")
#' center_max(df, col = "B")
#' center_max(df, col = "C")
#'
#' # Randomize which elements are left and right of the center
#' center_max(df, col = "A", shuffle_sides = TRUE)
#'
#' # Grouped by G
#' df %>%
#'   dplyr::select(G, A) %>% # For clarity
#'   dplyr::group_by(G) %>%
#'   center_max(col = "A")
#'
#' # Plot the centered values
#' plot(x = 1:10, y = center_max(df, col = "B")$B)
#' plot(x = 1:10, y = center_max(df, col = "B", shuffle_sides = TRUE)$B)
center_max <- function(data,
                       col = NULL,
                       shuffle_sides = FALSE) {
  centering_rearranger_(
    data = data,
    col = col,
    shuffle_sides = shuffle_sides,
    what = "max"
  )
}


##  .................. #< 4e75ce6b47c7a9a343bc39ec5b0ddf9c ># ..................
##  Center min                                                              ####


#' @title Centers the lowest value with values increasing around it
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  The lowest value is positioned in the middle with the other
#'  values increasing around it.
#'
#'  \strong{Example}:
#'
#'  The column values:
#'
#'  \code{c(}\strong{\code{1}}\code{, 2, 3, 4, 5)}
#'
#'  are \strong{ordered as}:
#'
#'  \code{c(5, 3,} \strong{\code{1}}\code{, 2, 4)}
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family rearrange functions
#' @inheritParams centering_rearranger_
#' @return
#'  The sorted \code{data.frame} (\code{tibble}) / \code{vector}.
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
#' # Center by the index (row numbers)
#' center_min(df)
#'
#' # Center by each of the columns
#' center_min(df, col = "A")
#' center_min(df, col = "B")
#' center_min(df, col = "C")
#'
#' # Randomize which elements are left and right of the center
#' center_min(df, col = "A", shuffle_sides = TRUE)
#'
#' # Grouped by G
#' df %>%
#'   dplyr::select(G, A) %>% # For clarity
#'   dplyr::group_by(G) %>%
#'   center_min(col = "A")
#'
#' # Plot the centered values
#' plot(x = 1:10, y = center_min(df, col = "B")$B)
#' plot(x = 1:10, y = center_min(df, col = "B", shuffle_sides = TRUE)$B)
center_min <- function(data,
                       col = NULL,
                       shuffle_sides = FALSE) {
  centering_rearranger_(
    data = data,
    col = col,
    shuffle_sides = shuffle_sides,
    what = "min"
  )
}
