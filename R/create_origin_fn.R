

#   __________________ #< 6aa81cdd89f8f5d4e6a860b87d507fc8 ># __________________
#   Create origin_fn                                                        ####

#' @title Create origin_fn function
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Creates a function that applies a supplied function to all input vectors.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @param fn Function to apply to each dimension. Should return a numeric scalar.
#' @export
#' @return Function with the dots (\code{...}) argument that applies the \code{`fn`} function to
#'  each element in \code{...} (usually one vector per dimension). More specifically:
#'
#'  \code{function(...)\{}
#'
#'  \verb{  }\code{list(...) \%>\%}
#'
#'  \verb{    }\code{purrr::map(fn) \%>\%}
#'
#'  \verb{    }\code{unlist(recursive = TRUE,}
#'
#'  \verb{           }\code{use.names = FALSE)}
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
#' y <- runif(10)
#' z <- runif(10)
#'
#' # Create origin_fn that gets the median of each dimension
#' median_origin_fn <- create_origin_fn(median)
#'
#' # Use median_origin_fn
#' median_origin_fn(x, y, z)
#'
#' # Should be the same as
#' c(median(x), median(y), median(z))
#'
#' }
create_origin_fn <- function(fn) {
  function(...){
    list(...) %>%
      purrr::map(fn) %>%
      unlist(recursive = TRUE, use.names = FALSE)
  }
}
