

#   __________________ #< e97c82695f1a8c08d80cc425e75c4d3a ># __________________
#   Create dimming function                                                 ####

#' @title Create dimming_fn function
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Creates a function that multiplies its first input
#'  by an inverse-exponential (as in \eqn{1 / exp(d)}) of its second input.
#'
#'  With the default values, the returned function is:
#'
#'  \code{function(x, d)\{}
#'
#'  \verb{  }\code{x * (1 / ((1 + d) ^ 2))}
#'
#'  \code{\}}
#'
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @param numerator The numerator. Defaults to \code{1}.
#' @param exponent The exponent. Defaults to \code{2}.
#' @param add_to_distance Constant to add to the distance before exponentiation.
#'  Ensures dimming even when the distance (\code{d}) is below \code{1}. Defaults to \code{1}.
#' @export
#' @return Function with the arguments \code{x} and \code{d},
#'  with both expected to be \code{numeric vector}s. More specifically:
#'
#'  \code{function(x, d)\{}
#'
#'  \verb{  }\code{x * (numerator / ((add_to_distance + d) ^ exponent))}
#'
#'  \code{\}}
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
#' d <- runif(10, max = 0.5)
#'
#' # Create dimming_fn with and add_to_distance of 0
#' # Note: In practice this risks zero-division
#' non_smoothed_dimming_fn <- create_dimming_fn(add_to_distance = 0)
#' non_smoothed_dimming_fn
#' as.list(environment(non_smoothed_dimming_fn))
#'
#' # Use median_origin_fn
#' non_smoothed_dimming_fn(x, d)
#'
#' }
create_dimming_fn <- function(numerator = 1, exponent = 2, add_to_distance = 1){

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_number(numerator, add = assert_collection)
  checkmate::assert_number(exponent, add = assert_collection)
  checkmate::assert_number(add_to_distance, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  function(x, d) {
    x * (numerator / ((add_to_distance + d) ^ exponent))
  }
}
