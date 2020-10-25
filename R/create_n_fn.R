

#   __________________ #< 8992856648db85baa597628de5d7c049 ># __________________
#   Create n_fn function                                                    ####


#' @title Create n_fn function
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Creates a function that applies a supplied function to all
#'  input vectors, or their indices, and rounds the results.
#'
#'  As used with \code{\link[rearrr:roll_elements]{roll_elements()}}. E.g. to
#'  find the the median index in a subset of a grouped \code{data.frame}.
#'
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @param fn Function to apply to each dimension. Should return a numeric scalar.
#' @param use_index Whether to apply \code{`fn`} to the \emph{indices} of the vectors. (Logical)
#'
#'  The indices are created with \code{seq_along(x)}.
#' @param negate Whether to negate the result. I.e. to multiply it with \code{-1}. (Logical)
#' @param round_fn Function for rounding results of \code{`fn`}.
#'
#'  Rounding is done \emph{prior} to negation.
#'
#'  E.g. \code{\link[base:Round]{round}}, \code{\link[base:Round]{floor}}, or \code{\link[base:Round]{ceiling}}.
#'
#'  To avoid rounding, supply \code{\link[base:identity]{identity}}.
#' @param ... Arguments for \code{`fn`}. E.g. \code{`na.rm = TRUE`}.
#' @export
#' @family n functions
#' @family function creators
#' @return Function with the dots (\code{`...`}) argument
#'  that applies the \code{`fn`} function to
#'  each element in \code{`...`} (or indices thereof) (usually one vector per dimension).
#'  The results are rounded with \code{`round_fn`}.
#'
#'  Note: The dots argument in the generated function should not to be confused with the dots
#'  argument in \code{create_n_fn()}).
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
#' # Create n_fn that gets the median index
#' # and rounds down with floor()
#' median_index_fn <- create_n_fn(median, use_index = TRUE, round_fn = floor)
#'
#' # Use median_index_fn
#' median_index_fn(x, y, z)
#'
#' # Create n_fn that gets the median of each dimension
#' median_n_fn <- create_n_fn(median)
#'
#' # Use median_origin_fn
#' median_n_fn(x, y, z)
#'
#' # Should be the same as
#' round(c(median(x), median(y), median(z)))
#'
#' # Use mean and ignore missing values
#' mean_n_fn <- create_n_fn(mean, na.rm = TRUE)
#'
#' # Add missing values
#' x[[2]] <- NA
#' y[[5]] <- NA
#'
#' # Use mean_n_fn
#' mean_n_fn(x, y, z)
#'
#' # Should be the same as
#' round(c(
#'   mean(x, na.rm = TRUE),
#'   mean(y, na.rm = TRUE),
#'   mean(z, na.rm = TRUE)
#' ))
create_n_fn <- function(fn,
                        use_index = FALSE,
                        negate = FALSE,
                        round_fn = round,
                        ...) {
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_function(fn, add = assert_collection)
  checkmate::assert_function(round_fn, add = assert_collection)
  checkmate::assert_flag(use_index, add = assert_collection)
  checkmate::assert_flag(negate, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Either negate or identity
  negate_fn <- function(x) {
    x * (1 - 2 * isTRUE(negate))
  }

  args <- list(...)
  function(...) {
    input <- list(...)
    if (isTRUE(use_index)) {
      # Convert to indices
      input <- input %>%
        purrr::map(.f = ~ {
          seq_along(.x)
        })
    }
    input %>%
      purrr::map(
        .f = function(x) {
          negate_fn(round_fn(rlang::exec(.fn = fn, x, !!!args)))
        }
      ) %>%
      unlist(recursive = TRUE, use.names = FALSE)
  }
}


#' @title Find index of interest for each vector
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Applies function to the indices of each \code{vector} in \code{`...`}.
#'
#'  These functions were created with \code{\link[rearrr:create_n_fn]{create_n_fn()}}.
#'
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @param ... Numeric \code{vector}s.
#' @param negate Whether to negate the result. I.e. to multiply it with \code{-1}. (Logical)
#' @param round_fn Function for rounding output. Rounding is done \emph{prior} to negation.
#'
#'  E.g. \code{\link[base:Round]{round}}, \code{\link[base:Round]{floor}}, or \code{\link[base:Round]{ceiling}}.
#' @export
#' @family n functions
#' @return \code{numeric vector} with one element per supplied vector.
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
#' y <- runif(15)
#' z <- runif(20)
#'
#' median_index(x, y, z)
#' quantile_index(x, y, z, prob = 0.2)
#'
#' # Negate result
#' median_index(x, y, z, negate = TRUE)
#' }
median_index <- function(...,
                         negate = FALSE,
                         round_fn = round) {
  create_n_fn(median,
    use_index = TRUE,
    negate = negate,
    round_fn = round_fn
  )(...)
}

#' @rdname median_index
#' @export
#' @param prob Probability in \code{[0,1]} for \code{\link[stats:quantile]{quantile()}}.
#' @param type Quantile algorithm to use. See \code{\link[stats:quantile]{quantile()}}.
quantile_index <- function(...,
                           prob,
                           type = 7,
                           negate = FALSE,
                           round_fn = round) {
  create_n_fn(
    quantile,
    use_index = TRUE,
    negate = negate,
    round_fn = round_fn,
    prob = prob,
    type = type
  )(...)
}
