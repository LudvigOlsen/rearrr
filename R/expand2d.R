

#   __________________ #< cd1a61becee10db96fdb9c8566818046 ># __________________
#   rotate2d                                                                ####


#' @title Expand the values around an origin in 2 dimensions
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
                     multipliers = 1,
                     multiplier_fn = NULL,
                     origin = c(0, 0),
                     origin_fn = NULL,
                     exponentiate = FALSE,
                     add_one_exp = TRUE,
                     suffix = "_expanded",
                     keep_original = TRUE,
                     mult_col_name = ifelse(isTRUE(exponentiate), ".exponent", ".multipliers"),
                     origin_col_name = ".origin") {
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  if (is.data.frame(data)){
    if (!checkmate::test_string(x_col, min.chars = 1)){
      assert_collection$push("when 'data' is a data.frame, x_col must be a non-empty string.")
    }
    if (!checkmate::test_string(y_col, min.chars = 1)){
      assert_collection$push("when 'data' is a data.frame, y_col must be a non-empty string.")
    }
  } else {
    if (!checkmate::test_null(x_col)){
      assert_collection$push("when 'data' is a vector, x_col must be 'NULL'.")
    }
    if (!checkmate::test_null(y_col)){
      assert_collection$push("when 'data' is a vector, y_col must be 'NULL'.")
    }
  }
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Expansion!!!
  expand(
    data = data,
    cols = c(x_col, y_col),
    multipliers = multipliers,
    multipliers_fn = multiplier_fn,
    origin = origin,
    origin_fn = origin_fn,
    exponentiate = exponentiate,
    add_one_exp = add_one_exp,
    suffix = suffix,
    keep_original = keep_original,
    mult_col_name = mult_col_name,
    origin_col_name = origin_col_name
  )

}
