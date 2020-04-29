

#   __________________ #< e6f9d8399b63846667e43cfef2db8cf1 ># __________________
#   Reverse windows                                                         ####


#' @title Reverse order window-wise
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  The values are windowed and reversed within windows.
#'
#'  \strong{Example}:
#'
#'  The column values:
#'
#'  \code{c(1, 2, 3, 4, 5, 6)}
#'
#'  With \code{window_size = 3}
#'
#'  Are \strong{ordered as}:
#'
#'  \code{c(3, 2, 1, 6, 4, 5)}
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family rearrange functions
#' @inheritParams rearrange
#' @param window_size Size of the windows.
#' @param keep_windows Whether to keep the factor with window identifiers. (Logical)
#' @param factor_name Name of windows factor.
#'
#'  N.B. Only used when \code{keep_windows} is \code{TRUE}.
#' @return
#'  The sorted \code{data frame} / \code{vector}.
#'  Optionally with the windows factor added.
#'
#'  When \code{data} is a \code{vector} and \code{keep_windows} is \code{FALSE},
#'  the output will be a \code{vector}. Otherwise, a \code{data frame}.
#' @examples
#' \donttest{
#' # Attach packages
#' library(rearrr)
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
#'   "G" = c(1, 1, 1, 1, 1,
#'           2, 2, 2, 2, 2),
#'   stringsAsFactors = FALSE
#' )
#'
#' # For vector
#' rev_windows(1:10, window_size = 3)
#'
#' # For data frame
#' rev_windows(df, window_size = 3)
#' rev_windows(df, window_size = 3, keep_windows = TRUE)
#'
#' # Grouped by G
#' df %>%
#'   dplyr::select(G, index) %>%  # For clarity
#'   dplyr::group_by(G) %>%
#'   rev_windows(window_size = 3)
#'
#' # Plot the extreme pairs
#' plot(x = 1:10,
#'      y = rev_windows(1:10, window_size = 3))
#' }
rev_windows <- function(data,
                        window_size,
                        keep_windows = FALSE,
                        factor_name = ".window") {
  rearrange(
    data,
    col = NULL,
    size = window_size,
    method = "rev_windows",
    keep_factor = keep_windows,
    factor_name = factor_name
  )
}



