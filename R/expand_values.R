

#   __________________ #< 98e58be6387b888b9531fcb3a3d4f0b7 ># __________________
#   Expand values                                                           ####


#' @title Expand the values around an origin
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  The distance to the specified origin is increased/decreased in all dimensions.
#'  A multiplier greater than 1 leads to expansion, while a positive multiplier lower than 1 leads to contraction.
#'
#'  The origin can be supplied as coordinates or as a function that returns coordinates. The
#'  latter can be useful when supplying a grouped data frame and expanding around e.g. the centroid
#'  of each group.
#'
#'  The multipliers/exponents can can supplied as constant(s) or as a function that returns constant(s).
#'  The latter can be useful when supplying a grouped data frame and the multiplier/exponent depends
#'  on the data in the groups.
#'  If supplying multiple constants, there must be one per dimension (length of \code{`cols`}).
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
#'  Must be either a single constant to use in all dimensions
#'  or a vector with one constant per dimension.
#'
#'  \strong{N.B.} Ignored when \code{`origin_fn`} is not \code{NULL}.
#' @param origin_fn Function for finding the origin coordinates to expand the values around.
#'  Each column will be passed as a vector in the order of \code{`cols`}.
#'  It should return either a single constant to be used in
#'  all dimensions or a vector with one constant per dimension.
#'
#'  Can be created with \code{\link[rearrr:create_origin_fn]{create_origin_fn()}} if you want to apply
#'  the same function to each dimension.
#'
#'  E.g. the \code{\link[rearrr:centroid]{centroid()}}, which is created with:
#'
#'  \code{create_origin_fn(mean)}
#'
#'  Which returns the following function:
#'
#'  \code{function(...)\{}
#'
#'  \verb{  }\code{list(...) \%>\%}
#'
#'  \verb{    }\code{purrr::map(mean) \%>\%}
#'
#'  \verb{    }\code{unlist(recursive = TRUE,}
#'
#'  \verb{           }\code{use.names = FALSE)}
#'
#'  \code{\}}
#' @param multipliers Constant(s) to multiply/exponentiate the distance to the origin by.
#'  Must be either a single constant to use in all dimensions or
#'  a vector with one constant per dimension.
#'
#'  \strong{N.B.} When \code{`exponentiate`} is \code{TRUE}, the multipliers become \emph{exponents}.
#' @param multiplier_fn Function for finding the multipliers.
#'  Each column will be passed as a vector in the order of \code{`cols`}.
#'  It should return either a single constant to be used in
#'  all dimensions or a vector with one constant per dimension.
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
#' @param mult_col_name Name of new column with the multiplier. If \code{NULL}, no column is added.
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
#'  Note: By default (when \code{`add_one_exp`} is TRUE),
#'  we add the sign (\code{1 / -1}) of the value before the exponentiation
#'  and subtracts it afterwards. See \code{`add_one_exp`}.
#' @family mutate functions
#' @inheritParams multi_mutator
#' @examples
#' \donttest{
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
#'   "g" = c(1, 1, 1, 1, 1,
#'           2, 2, 2, 2, 2,
#'           3, 3, 3, 3, 3,
#'           4, 4, 4, 4, 4)
#' )
#'
#' # Expand values in the two dimensions (x and y)
#' # With the origin at x=0.5, y=0.5
#' # We expand x by 2 and y by 4
#' expand_values(
#'   data = df,
#'   cols = c("x", "y"),
#'   multipliers = c(2, 4),
#'   origin = c(0.5, 0.5)
#' )
#'
#' # Expand values in the two dimensions (x and y)
#' # With the origin at x=0.5, y=0.5
#' # We expand both by 3
#' expand_values(
#'   data = df,
#'   cols = c("x", "y"),
#'   multipliers = 3,
#'   origin = 0.5
#' )
#'
#' # Expand values in one dimension (x)
#' # With the origin at x=0.5
#' # We expand by 3
#' expand_values(
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
#'   .f = function(exponent){
#'   expand_values(
#'     data = df,
#'     cols = c("x", "y"),
#'     multipliers = exponent,
#'     origin_fn = centroid,
#'     exponentiate = TRUE,
#'     add_one_exp = TRUE)
#' })
#' df_expanded
#'
#' # Plot the expansions of x and y around the overall centroid
#' ggplot(df_expanded, aes(x = x_expanded, y = y_expanded, color = factor(.exponents))) +
#'   geom_vline(xintercept = df_expanded[[".origin"]][[1]][[1]],
#'              size = 0.2, alpha = .4, linetype="dashed") +
#'   geom_hline(yintercept = df_expanded[[".origin"]][[1]][[2]],
#'              size = 0.2, alpha = .4, linetype="dashed") +
#'   geom_point() +
#'   theme_minimal() +
#'   labs(x = "x", y="y", color="Exponent")
#'
#' # Expand x and y around the centroid using multiplication
#' # To compare multiple multipliers, we wrap the
#' # call in purrr::map_dfr
#' df_expanded <- purrr::map_dfr(
#'   .x = c(1, 2.0, 3.0, 4.0),
#'   .f = function(multiplier){
#'   expand_values(df,
#'          cols = c("x", "y"),
#'          multipliers = multiplier,
#'          origin_fn = centroid,
#'          exponentiate = FALSE)
#' })
#' df_expanded
#'
#' # Plot the expansions of x and y around the overall centroid
#' ggplot(df_expanded, aes(x = x_expanded, y = y_expanded, color = factor(.multipliers))) +
#'   geom_vline(xintercept = df_expanded[[".origin"]][[1]][[1]],
#'              size = 0.2, alpha = .4, linetype = "dashed") +
#'   geom_hline(yintercept = df_expanded[[".origin"]][[1]][[2]],
#'              size = 0.2, alpha = .4, linetype = "dashed") +
#'   geom_point() +
#'   theme_minimal() +
#'   labs(x = "x", y = "y", color = "Multiplier")
#'
#' # Expand x and y with different multipliers
#' # around the centroid using multiplication
#' df_expanded <- expand_values(
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
#'   geom_vline(xintercept = df_expanded[[".origin"]][[1]][[1]],
#'              size = 0.2, alpha = .4, linetype = "dashed") +
#'   geom_hline(yintercept = df_expanded[[".origin"]][[1]][[2]],
#'              size = 0.2, alpha = .4, linetype = "dashed") +
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
#'   expand_values(
#'     cols = c("x", "y"),
#'     multipliers = 0.07,
#'     suffix = "_contracted",
#'     origin_fn = centroid
#'   )
#'
#' # Plot the clustered data point on top of the original data points
#' ggplot(df_contracted, aes(x = x_contracted, y = y_contracted, color = factor(g))) +
#'   geom_point(aes(x=x, y=y, color = factor(g)), alpha = 0.3, shape=16) +
#'   geom_point() +
#'   theme_minimal() +
#'   labs(x = "x", y="y", color="g")
#'
#' }
expand_values <- function(data,
                          cols = NULL,
                          multipliers = 1,
                          multipliers_fn = NULL,
                          origin = 0,
                          origin_fn = NULL,
                          exponentiate = FALSE,
                          add_one_exp = TRUE,
                          suffix = "_expanded",
                          keep_original = TRUE,
                          mult_col_name = ifelse(isTRUE(exponentiate), ".exponents", ".multipliers"),
                          origin_col_name = ".origin") {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_string(mult_col_name, null.ok = TRUE, add = assert_collection)
  checkmate::assert_string(origin_col_name, null.ok = TRUE, add = assert_collection)
  checkmate::assert_numeric(origin,
                            min.len = 1,
                            any.missing = FALSE,
                            add = assert_collection)
  checkmate::assert_numeric(
    multipliers,
    any.missing = FALSE,
    min.len = 1,
    add = assert_collection
  )
  checkmate::assert_flag(exponentiate, add = assert_collection)
  checkmate::assert_flag(add_one_exp, add = assert_collection)
  checkmate::assert_function(origin_fn, null.ok = TRUE, add = assert_collection)
  checkmate::assert_function(multipliers_fn, null.ok = TRUE, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Mutate with each multiplier
  multi_mutator(
    data = data,
    mutate_fn = expand_mutator_method,
    check_fn = NULL,
    cols = cols,
    suffix = suffix,
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


expand_mutator_method <- function(data,
                                  cols,
                                  suffix,
                                  multipliers,
                                  multipliers_fn,
                                  origin,
                                  origin_fn,
                                  exponentiate,
                                  add_one_exp,
                                  mult_col_name,
                                  origin_col_name) {
  # Number of dimensions
  # Each column is a dimension
  num_dims <- length(cols)

  # Convert columns to list of vectors
  dim_vectors <- as.list(data[, cols, drop = FALSE])

  # Find origin if specified
  if (!is.null(origin_fn)) {
    origin <- tryCatch(
      do.call(origin_fn, dim_vectors),
      error = function(e) {
        stop(paste0("failed to apply 'origin_fn': ", e))
      }
    )
    origin_check_msg <- "output of 'origin_fn'"
  } else {
    origin_check_msg <- "'origin'"
  }

  if (length(origin) %ni% c(1, num_dims)) {
    stop(
      paste0(
        origin_check_msg,
        " must have either length 1 or same",
        " length as 'cols' (",
        num_dims,
        ") but had length ",
        length(origin),
        "."
      )
    )
  }
  if (!is.numeric(origin)) {
    stop(paste0(origin_check_msg, " was not numeric."))
  }

  # Find multiplier if specified
  if (!is.null(multipliers_fn)) {
    multipliers <- tryCatch(
      do.call(multipliers_fn, dim_vectors),
      error = function(e) {
        stop(paste0("failed to apply 'multipliers_fn': ", e))
      }
    )
    multipliers_check_msg <- "output of 'multipliers_fn'"
  } else {
    multipliers_check_msg <- "'multipliers'"
  }

  if (length(multipliers) %ni% c(1, num_dims)) {
    stop(
      paste0(
        multipliers_check_msg,
        " must have either length 1 or same",
        " length as 'cols' (",
        num_dims,
        ") but had length ",
        length(multipliers),
        "."
      )
    )
  }
  if (!is.numeric(multipliers)) {
    stop(paste0(multipliers_check_msg, " was not numeric."))
  }

  # Move origin
  # x <- x - origin_coordinate
  dim_vectors <-
    purrr::map2(.x = dim_vectors, .y = origin, .f = ~ {
      .x - .y
    })

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
        sign(.x) * abs(.x) ^ .y
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
  dim_vectors <-
    purrr::map2(.x = dim_vectors, .y = origin, .f = ~ {
      .x + .y
    })

  # Add expanded columns to data

  # Add dim_vectors as columns with the suffix
  data <-
    add_dimensions(data = data,
                   new_vectors = dim_vectors,
                   suffix = suffix)

  # Add info columns
  if (!is.null(mult_col_name)){
    if (length(multipliers) > 1) {
      data[[mult_col_name]] <- list_coordinates(multipliers, cols)
    } else{
      data[[mult_col_name]] <- setNames(multipliers, cols)
    }
  }
  if (!is.null(origin_col_name)) {
    data[[origin_col_name]] <- list_coordinates(origin, cols)
  }
  data

}
