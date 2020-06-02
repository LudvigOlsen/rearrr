

#   __________________ #< 7987c9c27447df629ccf9474404fc327 ># __________________
#   Flip values                                                             ####


#' @title Flip the values around a center value
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  The values are flipped with the formula \code{x = 2 * c - x} where \code{x} is the value and \code{c} is
#'  the center value to flip the values around. The center value is determined by the
#'  supplied \code{center_fn} function.
#'
#'  \strong{Example}:
#'
#'  The column values:
#'
#'  \code{c(5, 2, 7, 4, 3, 1)}
#'
#'  and the \code{center_fn = median}
#'
#'  Changes the values to :
#'
#'  \code{c(2, 5, 0, 3, 4, 6)}
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @param center_fn Function for finding the center value to flip the values around.
#' @export
#' @family mutate functions
#' @inheritParams mutator
#' @examples
#' \donttest{
#' # Attach packages
#' library(rearrr)
#' library(dplyr)
#' library(ggplot2)
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
#' # Flip values of each column
#' flip_values(df, col = "A", center_fn = median)$A
#' flip_values(df, col = "B", center_fn = median)$B
#'
#' # Grouped by G
#' df %>%
#'   dplyr::select(G, A) %>%  # For clarity
#'   dplyr::group_by(G) %>%
#'   flip_values(col = "A",
#'               new_name="A_flipped",
#'               center_fn = median)
#'
#' # Plot A and flipped A
#' # First find flipped A around the median and around the value 3.
#' df <- df %>%
#'   flip_values(col = "A", new_name = "A_flip_median", center_fn = median) %>%
#'   flip_values(col = "A", new_name = "A_flip_3", center_fn = function(x){3})
#'
#' ggplot(df, aes(x=index, y=A)) +
#'     geom_line(aes(color="A")) +
#'     geom_line(aes(y=A_flip_median, color="Flipped A (median)")) +
#'     geom_hline(aes(color="Median A", yintercept = median(A))) +
#'     theme_minimal()
#'
#' ggplot(df, aes(x=index, y=A)) +
#'     geom_line(aes(color="A")) +
#'     geom_line(aes(y=A_flip_3, color="Flipped A (3)")) +
#'     geom_hline(aes(color="3", yintercept = 3)) +
#'     theme_minimal()
#' }
flip_values <- function(data,
                        col = NULL,
                        center_fn = median,
                        new_name = NULL) {
  mutator(
    data = data,
    mutate_fn = flip_mutator_method,
    check_fn = NULL,
    col = col,
    new_name = new_name,
    center_fn = center_fn
  )
}

flip_mutator_method <- function(data,
                                col,
                                new_name,
                                center_fn) {
  flip_around <- function(vec, around = median(x)) {
    2 * around - vec
  }

  # Find center value
  center <- center_fn(data[[col]])
  data[[new_name]] <- flip_around(vec = data[[col]], around = center)
  data
}
