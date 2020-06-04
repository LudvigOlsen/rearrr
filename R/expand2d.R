

#   __________________ #< cd1a61becee10db96fdb9c8566818046 ># __________________
#   rotate2d                                                                ####


#' @title Expand the values around an origin
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  The values are expanded such that the distance to the specified origin is increased in both dimensions.
#'
#'  The origin can be supplied as coordinates or as a function that returns coordinates. The
#'  latter can be useful when supplying a grouped data frame and expanding around e.g. the centroid
#'  of each group.
#'
#'  The multiplier/exponent can can supplied as a constant or as a function that returns a constant.
#'  The latter can be useful when supplying a grouped data frame and the multiplier/exponent depends
#'  on the data in the groups.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @param x_col Name of x column in \code{data}. If \code{NULL} or \code{data} is a vector,
#'  the index of \code{data} is used.
#' @param y_col Name of y column in \code{data}. If \code{data} is a data frame, it must be specified.
#' @param suffix Suffix to add to the names of the generated columns.
#' @param origin Coordinates of the origin to expand around. Must be a vector with 2 elements (orig_x, orig_y).
#'  Ignored when \code{origin_fn} is not \code{NULL}.
#' @param origin_fn Function for finding the origin coordinates to expand the values around.
#'  Should have 2 input arguments (a vector with x-values, a vector with y-values) and
#'  return a vector with exactly 2 elements (orig_x, orig_y).
#'
#' @param multiplier TODO
#' @param multiplier_fn
#' @param exponentiate
#' @param add_one_exp Whether to add 1/-1 before exponentiating to ensure the values don't contract.
#'  The added value is subtracted after the exponentiation.
#'
#'  Added with:
#'  \code{x <- x + sign(x)} ; \code{y <- y + sign(y)}.
#'
#'  Ignored when \code{exponentiate} is \code{FALSE}.
#' @param mult_col_name Name of new column with the multiplier.
#' @param origin_col_name Name of new column with the origin coordinates.
#' @export
#' @return Data frame with three new columns containing the rotated x- and y-values and the degrees.
#' @details
#'  Applies the following rotation matrix:
#'
#'  | [ \eqn{cos \theta} |, \eqn{ -sin \theta} ] |
#'  | :--- | :--- |
#'  | [ \eqn{sin \theta} |, \eqn{ cos \theta}  ] |
#'
#'  That is:
#'
#'  \eqn{x' = x cos \theta - y sin \theta}
#'
#'  \eqn{y' = x sin \theta + y cos \theta}
#'
#'  Where \eqn{\theta} is the angle in radians.
#'
#'  As specified at [Wikipedia/Rotation_matrix](https://en.wikipedia.org/wiki/Rotation_matrix).
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
#'   "x" = runif(20),
#'   "y" = runif(20),
#'   "g" = c(1, 1, 1, 1, 1,
#'           2, 2, 2, 2, 2,
#'           3, 3, 3, 3, 3,
#'           4, 4, 4, 4, 4)
#' )
#'
#'
#' # Expand values
#' expand2d(df, multiplier = 1.2, x_col="x", y_col="y")
#'
#' # Expand x and y around the centroid
#' # We use exponentiation for a more drastic effect
#' # The add_one_exp makes sure it expands even
#' # when x or y is in the range [>-1, <1]
#' df_expanded <- df %>%
#'   expand2d(x_col = "x",
#'            y_col = "y",
#'            multiplier = c(1, 2.0, 3.0, 4.0),
#'            origin_fn = centroid,
#'            exponentiate = TRUE,
#'            add_one_exp = TRUE)
#' df_expanded
#'
#' # Plot the expansions of x and y around the overall centroid
#' ggplot(df_expanded, aes(x = x_expanded, y = y_expanded, color = factor(.exponent))) +
#'   geom_vline(xintercept = df_expanded[[".origin"]][[1]][[1]],
#'              size = 0.2, alpha = .4, linetype="dashed") +
#'   geom_hline(yintercept = df_expanded[[".origin"]][[1]][[2]],
#'              size = 0.2, alpha = .4, linetype="dashed") +
#'   geom_point() +
#'   theme_minimal() +
#'   labs(x = "x", y="y", color="Exponent")
#'
#' # Expand x and y around the centroid using multiplication
#' df_expanded <- df %>%
#'   expand2d(x_col = "x",
#'            y_col = "y",
#'            multiplier = c(1, 1.5, 2.0, 2.5),
#'            origin_fn = centroid,
#'            exponentiate = FALSE)
#' df_expanded
#'
#' # Plot the expansions of x and y around the overall centroid
#' ggplot(df_expanded, aes(x = x_expanded, y = y_expanded, color = factor(.multiplier))) +
#'   geom_vline(xintercept = df_expanded[[".origin"]][[1]][[1]],
#'              size = 0.2, alpha = .4, linetype="dashed") +
#'   geom_hline(yintercept = df_expanded[[".origin"]][[1]][[2]],
#'              size = 0.2, alpha = .4, linetype="dashed") +
#'   geom_point() +
#'   theme_minimal() +
#'   labs(x = "x", y="y", color="Multiplier")
#'
#' }
expand2d <- function(data,
                     x_col = NULL,
                     y_col = NULL,
                     suffix = "_expanded",
                     multiplier = 1,
                     multiplier_fn = NULL,
                     origin = c(0, 0),
                     origin_fn = NULL,
                     exponentiate = FALSE,
                     add_one_exp = TRUE,
                     mult_col_name = ifelse(isTRUE(exponentiate), ".exponent", ".multiplier"),
                     origin_col_name = ".origin") {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_string(x_col, null.ok = TRUE, add = assert_collection)
  checkmate::assert_string(suffix, add = assert_collection)
  checkmate::assert_string(mult_col_name, add = assert_collection)
  checkmate::assert_string(origin_col_name, add = assert_collection)
  checkmate::assert_numeric(origin,
                            len = 2,
                            any.missing = FALSE,
                            add = assert_collection)
  checkmate::assert_numeric(multiplier, any.missing = FALSE, min.len = 1, add = assert_collection)
  checkmate::assert_flag(exponentiate, add = assert_collection)
  checkmate::assert_flag(add_one_exp, add = assert_collection)
  checkmate::assert_function(origin_fn, null.ok = TRUE, add = assert_collection)
  checkmate::assert_function(multiplier_fn, null.ok = TRUE, add = assert_collection)
  if (is.data.frame(data) && is.null(y_col)){
    assert_collection$push("when 'data' is a data frame, 'y_col' must be specified.")
  }
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Mutate with each multiplier
  purrr::map_dfr(.x = multiplier, .f = function(mult){
    mutator(
      data = data,
      mutate_fn = expand2d_mutator_method,
      check_fn = NULL,
      force_df = TRUE,
      col = y_col,
      x_col = x_col,
      suffix = suffix,
      multiplier = mult,
      multiplier_fn = multiplier_fn,
      origin = origin,
      origin_fn = origin_fn,
      exponentiate = exponentiate,
      add_one_exp = add_one_exp,
      mult_col_name = mult_col_name,
      origin_col_name = origin_col_name
    )
  })


}

# col is the y_col
expand2d_mutator_method <- function(data,
                                    col,
                                    x_col,
                                    suffix,
                                    multiplier,
                                    multiplier_fn,
                                    origin,
                                    origin_fn,
                                    exponentiate,
                                    add_one_exp,
                                    mult_col_name,
                                    origin_col_name,
                                    new_name=NULL) {

  # Make it clear that col is the y_col
  y_col <- col

  # Extract x and y values
  if (is.null(x_col)) {
    x_col <- "Index"
    x <- seq_len(nrow(data))
  } else {
    x <- data[[x_col]]
  }
  y <- data[[y_col]]

  # Find origin if specified
  if (!is.null(origin_fn)){
    origin <- tryCatch(
      origin_fn(x, y),
      error = function(e) {
        stop(paste0("failed to apply 'origin_fn': ", e))
      }
    )
    if (length(origin) != 2){
      stop("output of 'origin_fn' did not have length 2.")
    }
    if (!is.numeric(origin)){
      stop("output of 'origin_fn' was not numeric.")
    }
  }

  # Find multiplier if specified
  if (!is.null(multiplier_fn)){
    multiplier <- tryCatch(
      multiplier_fn(x, y),
      error = function(e) {
        stop(paste0("failed to apply 'multiplier_fn': ", e))
      }
    )
    if (length(multiplier) != 1){
      stop("output of 'multiplier_fn' did not have length 1.")
    }
    if (!is.numeric(multiplier)){
      stop("output of 'multiplier_fn' was not numeric.")
    }
  }

  # Move origin
  x <- x - origin[[1]]
  y <- y - origin[[2]]

  # Apply expansion
  if (isTRUE(exponentiate)){
    if (isTRUE(add_one_exp)){
      x <- x + sign(x)
      y <- y + sign(y)
    }
    x <- sign(x) * abs(x) ^ multiplier
    y <- sign(y) * abs(y) ^ multiplier
    if (isTRUE(add_one_exp)){
      x <- x - sign(x)
      y <- y - sign(y)
    }
  } else {
    x <- x * multiplier
    y <- y * multiplier
  }

  # Move origin
  x <- x + origin[[1]]
  y <- y + origin[[2]]

  # Add expanded columns to data
  data[[paste0(x_col, suffix)]] <- x
  data[[paste0(y_col, suffix)]] <- y

  # Add info columns
  data[[mult_col_name]] <- multiplier
  data[[origin_col_name]] <- list(origin)
  data[[origin_col_name]] <- list(origin)

  data

}
