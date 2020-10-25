

#   __________________ #< e6f9d8399b63846667e43cfef2db8cf1 ># __________________
#   Reverse windows                                                         ####


#' @title Reverse order window-wise
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  The values are windowed and reversed within windows.
#'
#'  The \code{*_vec()} version takes and returns a \code{vector}.
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
#' @inheritParams rev_windows_rearranger_
#' @return
#'  The sorted \code{data.frame} (\code{tibble}) / \code{vector}.
#'  Optionally with the windows factor added.
#'
#'  When \code{`data`} is a \code{vector} and \code{`keep_windows`} is \code{FALSE},
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
#'   "G" = rep(1:2, each = 5),
#'   stringsAsFactors = FALSE
#' )
#'
#' # For vector
#' rev_windows_vec(1:10, window_size = 3)
#'
#' # For data frame
#' rev_windows(df, window_size = 3)
#' rev_windows(df, window_size = 3, factor_name = NULL)
#'
#' # Grouped by G
#' df %>%
#'   dplyr::select(G, index) %>% # For clarity
#'   dplyr::group_by(G) %>%
#'   rev_windows(window_size = 3)
#'
#' # Plot the extreme pairs
#' plot(
#'   x = 1:10,
#'   y = rev_windows_vec(1:10, window_size = 3)
#' )
rev_windows <- function(data,
                        window_size,
                        factor_name = ".window",
                        overwrite = FALSE) {
  rev_windows_rearranger_(
    data,
    window_size = window_size,
    factor_name = factor_name,
    overwrite = overwrite
  )
}

#' @rdname rev_windows
#' @export
rev_windows_vec <- function(data, window_size){
  checkmate::assert(checkmate::check_vector(data, strict = TRUE),
                    checkmate::check_factor(data))
  rev_windows(
    data = data,
    window_size = window_size,
    factor_name = NULL,
    overwrite = TRUE
  )
}
