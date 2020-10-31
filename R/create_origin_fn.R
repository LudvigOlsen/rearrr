

#   __________________ #< 6aa81cdd89f8f5d4e6a860b87d507fc8 ># __________________
#   Create origin_fn                                                        ####

#' @title Create origin_fn function
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Creates a function that applies a supplied function to all input vectors.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @param fn Function to apply to each dimension. Should return a numeric scalar.
#' @param ... Arguments for \code{`fn`}. E.g. \code{`na.rm = TRUE`}.
#' @family coordinate functions
#' @family function creators
#' @export
#' @return Function with the dots (\code{...}) argument that applies the \code{`fn`} function to
#'  each element in \code{...} (usually one vector per dimension).
#' @examples
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
#' # Use mean and ignore missing values
#' mean_origin_fn <- create_origin_fn(mean, na.rm = TRUE)
#'
#' # Add missing values
#' x[[2]] <- NA
#' y[[5]] <- NA
#'
#' # Use mean_origin_fn
#' mean_origin_fn(x, y, z)
#'
#' # Should be the same as
#' c(mean(x, na.rm = TRUE),
#'   mean(y, na.rm = TRUE),
#'   mean(z, na.rm = TRUE)
#' )
create_origin_fn <- function(fn, ...) {
  args <- list(...)
  function(...) {
    list(...) %>%
      purrr::map(.f = function(x) {
        rlang::exec(.fn = fn, x, !!!args)
      }) %>%
      unlist(recursive = TRUE, use.names = FALSE)
  }
}
