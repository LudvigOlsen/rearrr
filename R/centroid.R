

#   __________________ #< 9d281822e7dafc7afc466a38db278b1e ># __________________
#   Centroid                                                                ####


#' @title Find the coordinates for the centroid
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Calculates the mean for each of the passed vectors.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @param ... Numeric vectors.
#' @export
#' @return \code{vector} with the means of each supplied vector.
#' @examples
#' \donttest{
#' # Attach packages
#' library(rearrr)
#'
#' # Set seed
#' set.seed(1)
#'
#' # Create three vectors
#' x <- runif(10)
#' y <- runif(10)
#' z <- runif(10)
#'
#' # Find centroid coordinates
#' # Aka. the means of each vector
#' centroid(x, y, z)
#' }
centroid <- function(...) {
  list(...) %>%
    purrr::map(mean) %>%
    unlist(recursive = TRUE, use.names = FALSE)
}
