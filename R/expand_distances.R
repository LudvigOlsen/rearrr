

#   __________________ #< 9d281822e7dafc7afc466a38db278b1e ># __________________
#   Expand distances                                                        ####


#' @title Expand the distances to an origin
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Moves the data points in n-dimensional space such that their distance
#'  to a specified origin is increased/decreased.
#'  A \code{`multiplier`} greater than 1 leads to expansion,
#'  while a positive \code{`multiplier`} lower than 1 leads to contraction.
#'
#'  The origin can be supplied as coordinates or as a function that returns coordinates. The
#'  latter can be useful when supplying a grouped \code{data.frame} and expanding around e.g. the centroid
#'  of each group.
#'
#'  The multiplier/exponent can be supplied as a constant or as a function that returns a constant.
#'  The latter can be useful when supplying a grouped \code{data.frame} and the multiplier/exponent depends
#'  on the data in the groups.
#'
#'  For expansion in each dimension separately, use \code{\link[rearrr:expand_distances_each]{expand_distances_each()}}.
#'
#'  \strong{NOTE}: When exponentiating, the default is to first add \code{1} to the distances,
#'  to ensure expansion even when the distance is between \code{0} and \code{1}.
#'  If you need the purely exponentiated distances,
#'  disable \code{`add_one_exp`}.
#'
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @param cols Names of columns in \code{`data`} to expand coordinates of.
#'  Each column is considered a dimension.
#' @param origin Coordinates of the origin to expand around.
#'  A scalar to use in all dimensions
#'  or a \code{vector} with one scalar per dimension.
#'
#'  \strong{N.B.} Ignored when \code{`origin_fn`} is not \code{NULL}.
#' @param multiplier Constant to multiply/exponentiate the distances to the origin by.
#'
#'  \strong{N.B.} When \code{`exponentiate`} is \code{TRUE}, the \code{`multiplier`} becomes an \emph{exponent}.
#' @param multiplier_fn Function for finding the \code{`multiplier`}.
#'
#'  \strong{Input}: Each column will be passed as a \code{vector} in the order of \code{`cols`}.
#'
#'  \strong{Output}: A \code{numeric scalar}.
#' @param exponentiate Whether to exponentiate instead of multiplying. (Logical)
#' @param add_one_exp Whether to add \code{1} to the distances
#'  before exponentiating to ensure they don't contract when between \code{0} and \code{1}.
#'  The added value is subtracted after the exponentiation. (Logical)
#'
#'  The distances to the origin (\code{`d`}) are exponentiated as such:
#'
#'  \code{d <- d + 1}
#'
#'  \code{d <- d ^ multiplier}
#'
#'  \code{d <- d - 1}
#'
#'  \strong{N.B.} Ignored when \code{`exponentiate`} is \code{FALSE}.
#' @param mult_col_name Name of new column with the \code{`multiplier`}.
#'  If \code{NULL}, no column is added.
#' @param origin_col_name Name of new column with the origin coordinates.
#'  If \code{NULL}, no column is added.
#' @export
#' @return \code{data.frame} (\code{tibble}) with the expanded columns,
#'  along with the applied multiplier/exponent and origin coordinates.
#' @details
#'  Increases the distance to the origin in n-dimensional space by
#'  multiplying or exponentiating it by the multiplier.
#'
#'  We first move the origin to the zero-coordinates (e.g. \code{c(0, 0, 0)})
#'  and normalize each vector to unit length. We then multiply this unit vector by the
#'  multiplied/exponentiated distance and moves the origin back to its original coordinates.
#'
#'  The distance to the specified origin is calculated with:
#'  \deqn{d(P1, P2) = sqrt( (x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2 + ... )}
#'
#'  Note: By default (when \code{`add_one_exp`} is \code{TRUE}),
#'  we add \code{1} to the distance before the exponentiation
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
#' # Expand distances in the two dimensions (x and y)
#' # With the origin at x=0.5, y=0.5
#' # We multiply the distances by 2
#' expand_distances(
#'   data = df,
#'   cols = c("x", "y"),
#'   multiplier = 2,
#'   origin = c(0.5, 0.5)
#' )
#'
#' # Expand distances in the two dimensions (x and y)
#' # With the origin at x=0.5, y=0.5
#' # We exponentiate the distances by 2
#' expand_distances(
#'   data = df,
#'   cols = c("x", "y"),
#'   multiplier = 2,
#'   exponentiate = TRUE,
#'   origin = 0.5
#' )
#'
#' # Expand values in one dimension (x)
#' # With the origin at x=0.5
#' # We exponentiate the distances by 3
#' expand_distances(
#'   data = df,
#'   cols = c("x"),
#'   multiplier = 3,
#'   exponentiate = TRUE,
#'   origin = 0.5
#' )
#'
#' # Expand x and y around the centroid
#' # We use exponentiation for a more drastic effect
#' # The add_one_exp makes sure it expands
#' # even when x or y is in the range [0, <1]
#' # To compare multiple exponents, we wrap the
#' # call in purrr::map_dfr
#' df_expanded <- purrr::map_dfr(
#'   .x = c(1, 3, 5),
#'   .f = function(exponent) {
#'     expand_distances(
#'       data = df,
#'       cols = c("x", "y"),
#'       multiplier = exponent,
#'       origin_fn = centroid,
#'       exponentiate = TRUE,
#'       add_one_exp = TRUE
#'     )
#'   }
#' )
#' df_expanded
#'
#' # Plot the expansions of x and y around the overall centroid
#' ggplot(df_expanded, aes(x = x_expanded, y = y_expanded, color = factor(.exponent))) +
#'   geom_vline(
#'     xintercept = df_expanded[[".origin"]][[1]][[1]],
#'     size = 0.2, alpha = .4, linetype = "dashed"
#'   ) +
#'   geom_hline(
#'     yintercept = df_expanded[[".origin"]][[1]][[2]],
#'     size = 0.2, alpha = .4, linetype = "dashed"
#'   ) +
#'   geom_path(size = 0.2) +
#'   geom_point() +
#'   theme_minimal() +
#'   labs(x = "x", y = "y", color = "Exponent")
#'
#' # Expand x and y around the centroid using multiplication
#' # To compare multiple multipliers, we wrap the
#' # call in purrr::map_dfr
#' df_expanded <- purrr::map_dfr(
#'   .x = c(1, 3, 5),
#'   .f = function(multiplier) {
#'     expand_distances(df,
#'       cols = c("x", "y"),
#'       multiplier = multiplier,
#'       origin_fn = centroid,
#'       exponentiate = FALSE
#'     )
#'   }
#' )
#' df_expanded
#'
#' # Plot the expansions of x and y around the overall centroid
#' ggplot(df_expanded, aes(x = x_expanded, y = y_expanded, color = factor(.multiplier))) +
#'   geom_vline(
#'     xintercept = df_expanded[[".origin"]][[1]][[1]],
#'     size = 0.2, alpha = .4, linetype = "dashed"
#'   ) +
#'   geom_hline(
#'     yintercept = df_expanded[[".origin"]][[1]][[2]],
#'     size = 0.2, alpha = .4, linetype = "dashed"
#'   ) +
#'   geom_path(size = 0.2, alpha = .8) +
#'   geom_point() +
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
#'   expand_distances(
#'     cols = c("x", "y"),
#'     multiplier = 0.07,
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
expand_distances <- function(data,
                             cols = NULL,
                             multiplier = NULL,
                             multiplier_fn = NULL,
                             origin = NULL,
                             origin_fn = NULL,
                             exponentiate = FALSE,
                             add_one_exp = TRUE,
                             suffix = "_expanded",
                             keep_original = TRUE,
                             mult_col_name = ifelse(isTRUE(exponentiate), ".exponent", ".multiplier"),
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
  checkmate::assert_number(multiplier, null.ok = TRUE, add = assert_collection)
  checkmate::assert_flag(exponentiate, add = assert_collection)
  checkmate::assert_flag(add_one_exp, add = assert_collection)
  checkmate::assert_function(origin_fn, null.ok = TRUE, add = assert_collection)
  checkmate::assert_function(multiplier_fn, null.ok = TRUE, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # Check if we will need to overwrite columns
  check_unique_colnames_(cols, origin_col_name, mult_col_name)
  check_overwrite_(data = data, nm = mult_col_name, overwrite = overwrite)
  check_overwrite_(data = data, nm = origin_col_name, overwrite = overwrite)
  # End of argument checks ####

  # Mutate with each multiplier
  multi_mutator_(
    data = data,
    mutate_fn = expand_mutator_method_,
    check_fn = NULL,
    cols = cols,
    suffix = suffix,
    overwrite = overwrite,
    force_df = TRUE,
    keep_original = keep_original,
    multiplier = multiplier,
    multiplier_fn = multiplier_fn,
    origin = origin,
    origin_fn = origin_fn,
    exponentiate = exponentiate,
    add_one_exp = add_one_exp,
    mult_col_name = mult_col_name,
    origin_col_name = origin_col_name
  )
}


expand_mutator_method_ <- function(data,
                                   grp_id,
                                   cols,
                                   suffix,
                                   overwrite,
                                   multiplier,
                                   multiplier_fn,
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
  multiplier <- apply_coordinate_fn_(
    dim_vectors = dim_vectors,
    coordinates = multiplier,
    fn = multiplier_fn,
    num_dims = 1,
    coordinate_name = "multiplier",
    fn_name = "multiplier_fn",
    dim_var_name = NULL,
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

  # Calculate distances
  # formula: sqrt( (x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2 )
  distances <- calculate_distances_(
    dim_vectors = dim_vectors,
    to = rep(0, num_dims)
  )

  # Unit length points
  norm_dim_vectors <- to_unit_lengths_rowwise_(dim_vectors)

  # Apply expansion
  if (isTRUE(exponentiate)) {
    # Add 1 to distances to avoid contraction when below 1
    distances <- distances + sum(isTRUE(add_one_exp))
    # Exponentiate distances
    expo_distances <- distances^multiplier
    # Substract the added 1
    expo_distances <- expo_distances - sum(isTRUE(add_one_exp))

    # Expand with exponentiation
    expanded_dim_vectors <-
      purrr::map(.x = norm_dim_vectors, .f = ~ {
        .x * expo_distances
      })
  } else {
    # Add the multiplied distance
    expanded_dim_vectors <-
      purrr::map(.x = norm_dim_vectors, .f = ~ {
        .x * distances * multiplier
      })
  }

  # Move origin
  if (!is_zero_vector_(origin)){
    expanded_dim_vectors <-
      purrr::map2(.x = expanded_dim_vectors, .y = origin, .f = ~ {
        .x + .y
      })
  }

  # Add expanded columns to data

  # Add dim_vectors as columns with the suffix
  data <- add_dimensions_(
    data = data,
    new_vectors = setNames(expanded_dim_vectors, cols),
    suffix = suffix,
    overwrite = overwrite
  )

  # Add info columns
  if (!is.null(mult_col_name)) {
    data[[mult_col_name]] <- multiplier
  }
  if (!is.null(origin_col_name)) {
    data[[origin_col_name]] <- list_coordinates_(origin, cols)
  }

  data
}
