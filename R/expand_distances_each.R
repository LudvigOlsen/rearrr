

#   __________________ #< 9d281822e7dafc7afc466a38db278b1e ># __________________
#   Expand distances separately for each dimension                          ####


#' @title Expand the distances to an origin in each dimension
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Moves the data points in n-dimensional space such that their distance
#'  to the specified origin is increased/decreased \emph{in each dimension separately}.
#'  A \code{`multiplier`} greater than 1 leads to expansion,
#'  while a positive \code{`multiplier`} lower than 1 leads to contraction.
#'
#'  The origin can be supplied as coordinates or as a function that returns coordinates. The
#'  latter can be useful when supplying a grouped \code{data.frame} and expanding around e.g. the centroid
#'  of each group.
#'
#'  The multipliers/exponents can be supplied as constant(s) or as a function that returns constants.
#'  The latter can be useful when supplying a grouped \code{data.frame} and the multiplier/exponent depends
#'  on the data in the groups.
#'  If supplying multiple constants, there must be one per dimension (length of \code{`cols`}).
#'
#'  For expansion of the \emph{multidimensional} distance, use \code{\link[rearrr:expand_distances_each]{expand_distances()}}.
#'
#'  \strong{NOTE}: When exponentiating, the default is to first add \code{1} or \code{-1}
#'  (depending on the sign of the distance) to the distances,
#'  to ensure expansion even when the distance is between \code{-1} and \code{1}.
#'  If you need the purely exponentiated distances,
#'  disable \code{`add_one_exp`}.
#'
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @param cols Names of columns in \code{`data`} to expand.
#'  Each column is considered a dimension to expand in.
#' @param origin Coordinates of the origin to expand around.
#'  A scalar to use in all dimensions
#'  or a \code{vector} with one scalar per dimension.
#'
#'  \strong{N.B.} Ignored when \code{`origin_fn`} is not \code{NULL}.
#' @param multipliers Constant(s) to multiply/exponentiate the distance to the origin by.
#'  A scalar to use in all dimensions or
#'  a \code{vector} with one scalar per dimension.
#'
#'  \strong{N.B.} When \code{`exponentiate`} is \code{TRUE}, the \code{`multipliers`} become \emph{exponents}.
#' @param multipliers_fn Function for finding the \code{`multipliers`}.
#'
#'  \strong{Input}: Each column will be passed as a \code{vector} in the order of \code{`cols`}.
#'
#'  \strong{Output}: A \code{numeric vector} with one element per dimension.
#'
#'  Just as for \code{`origin_fn`}, it can be created with
#'  \code{\link[rearrr:create_origin_fn]{create_origin_fn()}} if you want to apply
#'  the same function to each dimension. See \code{`origin_fn`}.
#' @param exponentiate Whether to exponentiate instead of multiplying. (Logical)
#' @param add_one_exp Whether to add the \code{sign} (either \code{1} or \code{-1})
#'  before exponentiating to ensure the values don't contract.
#'  The added value is subtracted after the exponentiation. (Logical)
#'
#'  Exponentiation becomes:
#'
#'  \code{x <- x + sign(x)}
#'
#'  \code{x <- sign(x) * abs(x) ^ multiplier}
#'
#'  \code{x <- x - sign(x)}
#'
#'  \strong{N.B.} Ignored when \code{`exponentiate`} is \code{FALSE}.
#' @param mult_col_name Name of new column with the multiplier(s). If \code{NULL}, no column is added.
#' @param origin_col_name Name of new column with the origin coordinates. If \code{NULL}, no column is added.
#' @export
#' @return \code{data.frame} (\code{tibble}) with the expanded columns,
#'  along with the applied multiplier/exponent and origin coordinates.
#' @details
#'  For each value of each dimension (column), either multiply or exponentiate by the multiplier:
#'
#'  \code{# Multiplication}
#'
#'  \code{x <- x * multiplier}
#'
#'  \code{# Exponentiation}
#'
#'  \code{x <- sign(x) * abs(x) ^ multiplier}
#'
#'  Note: By default (when \code{`add_one_exp`} is \code{TRUE}),
#'  we add the sign (\code{1 / -1}) of the value before the exponentiation
#'  and subtract it afterwards. See \code{`add_one_exp`}.
#' @family mutate functions
#' @family expander functions
#' @family distance functions
#' @inheritParams multi_mutator_
#' @examples
#' # Attach packages
#' library(rearrr)
#' library(dplyr)
#' library(purrr)
#' library(ggplot2)
#'
#' # Set seed
#' set.seed(1)
#'
#' # Create a data frame
#' df <- data.frame(
#'   "x" = runif(20),
#'   "y" = runif(20),
#'   "g" = rep(1:4, each = 5)
#' )
#'
#' # Expand values in the two dimensions (x and y)
#' # With the origin at x=0.5, y=0.5
#' # We expand x by 2 and y by 4
#' expand_distances_each(
#'   data = df,
#'   cols = c("x", "y"),
#'   multipliers = c(2, 4),
#'   origin = c(0.5, 0.5)
#' )
#'
#' # Expand values in the two dimensions (x and y)
#' # With the origin at x=0.5, y=0.5
#' # We expand both by 3
#' expand_distances_each(
#'   data = df,
#'   cols = c("x", "y"),
#'   multipliers = 3,
#'   origin = 0.5
#' )
#'
#' # Expand values in one dimension (x)
#' # With the origin at x=0.5
#' # We expand by 3
#' expand_distances_each(
#'   data = df,
#'   cols = c("x"),
#'   multipliers = 3,
#'   origin = 0.5
#' )
#'
#' # Expand x and y around the centroid
#' # We use exponentiation for a more drastic effect
#' # The add_one_exp makes sure it expands
#' # even when x or y is in the range [>-1, <1]
#' # To compare multiple exponents, we wrap the
#' # call in purrr::map_dfr
#' df_expanded <- purrr::map_dfr(
#'   .x = c(1, 2.0, 3.0, 4.0),
#'   .f = function(exponent) {
#'     expand_distances_each(
#'       data = df,
#'       cols = c("x", "y"),
#'       multipliers = exponent,
#'       origin_fn = centroid,
#'       exponentiate = TRUE,
#'       add_one_exp = TRUE
#'     )
#'   }
#' )
#' df_expanded
#'
#' # Plot the expansions of x and y around the overall centroid
#' ggplot(df_expanded, aes(x = x_expanded, y = y_expanded, color = factor(.exponents))) +
#'   geom_vline(
#'     xintercept = df_expanded[[".origin"]][[1]][[1]],
#'     size = 0.2, alpha = .4, linetype = "dashed"
#'   ) +
#'   geom_hline(
#'     yintercept = df_expanded[[".origin"]][[1]][[2]],
#'     size = 0.2, alpha = .4, linetype = "dashed"
#'   ) +
#'   geom_point() +
#'   theme_minimal() +
#'   labs(x = "x", y = "y", color = "Exponent")
#'
#' # Expand x and y around the centroid using multiplication
#' # To compare multiple multipliers, we wrap the
#' # call in purrr::map_dfr
#' df_expanded <- purrr::map_dfr(
#'   .x = c(1, 2.0, 3.0, 4.0),
#'   .f = function(multiplier) {
#'     expand_distances_each(df,
#'       cols = c("x", "y"),
#'       multipliers = multiplier,
#'       origin_fn = centroid,
#'       exponentiate = FALSE
#'     )
#'   }
#' )
#' df_expanded
#'
#' # Plot the expansions of x and y around the overall centroid
#' ggplot(df_expanded, aes(x = x_expanded, y = y_expanded, color = factor(.multipliers))) +
#'   geom_vline(
#'     xintercept = df_expanded[[".origin"]][[1]][[1]],
#'     size = 0.2, alpha = .4, linetype = "dashed"
#'   ) +
#'   geom_hline(
#'     yintercept = df_expanded[[".origin"]][[1]][[2]],
#'     size = 0.2, alpha = .4, linetype = "dashed"
#'   ) +
#'   geom_point() +
#'   theme_minimal() +
#'   labs(x = "x", y = "y", color = "Multiplier")
#'
#' # Expand x and y with different multipliers
#' # around the centroid using multiplication
#' df_expanded <- expand_distances_each(
#'   df,
#'   cols = c("x", "y"),
#'   multipliers = c(1.25, 10),
#'   origin_fn = centroid,
#'   exponentiate = FALSE
#' )
#' df_expanded
#'
#' # Plot the expansions of x and y around the overall centroid
#' # Note how the y axis is expanded a lot more than the x-axis
#' ggplot(df_expanded, aes(x = x_expanded, y = y_expanded)) +
#'   geom_vline(
#'     xintercept = df_expanded[[".origin"]][[1]][[1]],
#'     size = 0.2, alpha = .4, linetype = "dashed"
#'   ) +
#'   geom_hline(
#'     yintercept = df_expanded[[".origin"]][[1]][[2]],
#'     size = 0.2, alpha = .4, linetype = "dashed"
#'   ) +
#'   geom_line(aes(color = "Expanded")) +
#'   geom_point(aes(color = "Expanded")) +
#'   geom_line(aes(x = x, y = y, color = "Original")) +
#'   geom_point(aes(x = x, y = y, color = "Original")) +
#'   theme_minimal() +
#'   labs(x = "x", y = "y", color = "Multiplier")
#'
#' #
#' # Contraction
#' #
#'
#' # Group-wise contraction to create clusters
#' df_contracted <- df %>%
#'   dplyr::group_by(g) %>%
#'   expand_distances_each(
#'     cols = c("x", "y"),
#'     multipliers = 0.07,
#'     suffix = "_contracted",
#'     origin_fn = centroid
#'   )
#'
#' # Plot the clustered data point on top of the original data points
#' ggplot(df_contracted, aes(x = x_contracted, y = y_contracted, color = factor(g))) +
#'   geom_point(aes(x = x, y = y, color = factor(g)), alpha = 0.3, shape = 16) +
#'   geom_point() +
#'   theme_minimal() +
#'   labs(x = "x", y = "y", color = "g")
expand_distances_each <- function(data,
                                  cols = NULL,
                                  multipliers = NULL,
                                  multipliers_fn = NULL,
                                  origin = NULL,
                                  origin_fn = NULL,
                                  exponentiate = FALSE,
                                  add_one_exp = TRUE,
                                  suffix = "_expanded",
                                  keep_original = TRUE,
                                  mult_col_name = ifelse(isTRUE(exponentiate), ".exponents", ".multipliers"),
                                  origin_col_name = ".origin",
                                  overwrite = FALSE) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_string(mult_col_name, null.ok = TRUE, add = assert_collection)
  checkmate::assert_string(origin_col_name, null.ok = TRUE, add = assert_collection)
  checkmate::assert_numeric(origin,
    min.len = 1,
    any.missing = FALSE,
    null.ok = TRUE,
    add = assert_collection
  )
  checkmate::assert_numeric(
    multipliers,
    any.missing = FALSE,
    min.len = 1,
    null.ok = TRUE,
    add = assert_collection
  )
  checkmate::assert_flag(exponentiate, add = assert_collection)
  checkmate::assert_flag(add_one_exp, add = assert_collection)
  checkmate::assert_function(origin_fn, null.ok = TRUE, add = assert_collection)
  checkmate::assert_function(multipliers_fn, null.ok = TRUE, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # Check if we will need to overwrite columns
  check_unique_colnames_(cols, origin_col_name, mult_col_name)
  check_overwrite_(data = data, nm = mult_col_name, overwrite = overwrite)
  check_overwrite_(data = data, nm = origin_col_name, overwrite = overwrite)
  # End of argument checks ####

  # Mutate with each multiplier
  multi_mutator_(
    data = data,
    mutate_fn = expand_each_mutator_method_,
    check_fn = NULL,
    cols = cols,
    suffix = suffix,
    overwrite = overwrite,
    force_df = TRUE,
    keep_original = keep_original,
    multipliers = multipliers,
    multipliers_fn = multipliers_fn,
    origin = origin,
    origin_fn = origin_fn,
    exponentiate = exponentiate,
    add_one_exp = add_one_exp,
    mult_col_name = mult_col_name,
    origin_col_name = origin_col_name
  )
}


expand_each_mutator_method_ <- function(data,
                                        grp_id,
                                        cols,
                                        suffix,
                                        overwrite,
                                        multipliers,
                                        multipliers_fn,
                                        origin,
                                        origin_fn,
                                        exponentiate,
                                        add_one_exp,
                                        mult_col_name,
                                        origin_col_name,
                                        ...) {
  # Number of dimensions
  # Each column is a dimension
  num_dims <- length(cols)

  # Convert columns to list of vectors
  dim_vectors <- as.list(data[, cols, drop = FALSE])

  # Find origin if specified
  origin <- apply_coordinate_fn_(
    dim_vectors = dim_vectors,
    coordinates = origin,
    fn = origin_fn,
    num_dims = length(cols),
    coordinate_name = "origin",
    fn_name = "origin_fn",
    dim_var_name = "cols",
    grp_id = grp_id,
    allow_len_one = TRUE
  )

  # Find multiplier if specified
  multipliers <- apply_coordinate_fn_(
    dim_vectors = dim_vectors,
    coordinates = multipliers,
    fn = multipliers_fn,
    num_dims = length(cols),
    coordinate_name = "multipliers",
    fn_name = "multipliers_fn",
    dim_var_name = "cols",
    grp_id = grp_id,
    allow_len_one = TRUE
  )

  # Move origin
  # x <- x - origin_coordinate
  if (!is_zero_vector_(origin)){
    dim_vectors <-
      purrr::map2(.x = dim_vectors, .y = origin, .f = ~ {
        .x - .y
      })
  }

  # Apply expansion
  if (isTRUE(exponentiate)) {
    if (isTRUE(add_one_exp)) {
      # x <- x + sign(x)
      dim_vectors <-
        purrr::map(.x = dim_vectors, .f = ~ {
          .x + sign(.x)
        })
    }
    # x <- sign(x) * abs(x) ^ multiplier
    dim_vectors <-
      purrr::map2(.x = dim_vectors, .y = multipliers, .f = ~ {
        sign(.x) * abs(.x)^.y
      })
    if (isTRUE(add_one_exp)) {
      # x <- x - sign(x)
      dim_vectors <-
        purrr::map(.x = dim_vectors, .f = ~ {
          .x - sign(.x)
        })
    }
  } else {
    # x <- x * multiplier
    dim_vectors <-
      purrr::map2(.x = dim_vectors, .y = multipliers, .f = ~ {
        .x * .y
      })
  }

  # Move origin
  if (!is_zero_vector_(origin)){
    dim_vectors <-
      purrr::map2(.x = dim_vectors, .y = origin, .f = ~ {
        .x + .y
      })
  }

  # Add expanded columns to data

  # Add dim_vectors as columns with the suffix
  data <-
    add_dimensions_(
      data = data,
      new_vectors = dim_vectors,
      suffix = suffix,
      overwrite = overwrite
    )

  # Add info columns
  if (!is.null(mult_col_name)) {
    if (length(multipliers) > 1) {
      data[[mult_col_name]] <- list_coordinates_(multipliers, cols)
      data <- paste_coordinates_column_(data = data, col = mult_col_name)
    } else {
      data[[mult_col_name]] <- multipliers
    }
  }
  if (!is.null(origin_col_name)) {
    data[[origin_col_name]] <- list_coordinates_(origin, cols)
  }
  data
}
