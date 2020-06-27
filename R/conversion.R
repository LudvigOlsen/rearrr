
#' @title Conversion between radians and degrees
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Convert degrees to radians or radians to degrees.
#'
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @param degrees \code{vector} of degrees to convert to radians with \deqn{`degrees` * (\pi / 180)}
#' @param radians \code{vector} of radians to convert to degrees with \deqn{`radians` / (\pi / 180)}
#' @export
#' @return \code{vector} with converted degrees/radians.
#' @examples
#' \donttest{
#' # Attach packages
#' library(rearrr)
#' library(dplyr)
#'
#' # Degrees to radians
#' degrees_to_radians(c(90, 180, 270))
#'
#' # Radians to degrees
#' radians_to_degrees(c(pi / 2, pi, 1.5 * pi))
#'
#' # Get back the original degrees
#' radians_to_degrees(degrees_to_radians(c(90, 180, 270)))
#' }
degrees_to_radians <- function(degrees){
  checkmate::assert_numeric(degrees, any.missing = FALSE)
  degrees * (pi / 180)
}

#' @rdname degrees_to_radians
radians_to_degrees <- function(radians){
  checkmate::assert_numeric(radians, any.missing = FALSE)
  radians / (pi / 180)
}