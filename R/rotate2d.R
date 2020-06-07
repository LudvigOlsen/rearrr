


#   __________________ #< cd1a61becee10db96fdb9c8566818046 ># __________________
#   rotate2d                                                                ####


#' @title Rotate the values around an origin
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  The values are rotated counterclockwise around a specified origin.
#'
#'  The origin can be supplied as coordinates or as a function that returns coordinates. The
#'  latter can be useful when supplying a grouped data frame and rotating around e.g. the centroid
#'  of each group.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @param degrees Degrees to rotate values counterclockwise. In \code{[-360, 360]}.
#'  Can be a vector with multiple degrees.
#' @param x_col Name of x column in \code{`data`}. If \code{NULL} and \code{`data`} is a \code{vector},
#'  the index of \code{`data`} is used. If \code{`data`} is a \code{data.frame}, it must be specified.
#' @param y_col Name of y column in \code{`data`}. If \code{`data`} is a \code{data.frame}, it must be specified.
#' @param origin Coordinates of the origin to rotate around. Must be a vector with 2 elements (orig_x, orig_y).
#'  Ignored when \code{`origin_fn`} is not \code{NULL}.
#' @param origin_fn Function for finding the origin coordinates to rotate the values around.
#'  Should have 2 input arguments (a vector with x-values, a vector with y-values) and
#'  return a vector with exactly 2 elements (orig_x, orig_y).
#' @param degree_col_name Name of new column with the degrees. If \code{NULL}, no column is added.
#' @export
#' @return \code{data.frame} (\code{tibble}) with three new columns containing the rotated x- and y-values and the degrees.
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
#' @inheritParams multi_mutator
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
#'   "Index" = 1:12,
#'   "A" = c(
#'     1, 2, 3, 4, 9, 10,
#'     11, 12, 15, 16, 17, 18
#'   ),
#'   "G" = c(1, 1, 1, 1, 2, 2,
#'           2, 2, 3, 3, 3, 3)
#' )
#'
#' # Rotate values
#' rotate2d(df, 45, x_col="Index", y_col="A")
#'
#' # Rotate A around the centroid
#' df_rotated <- df %>%
#'   rotate2d(x_col = "Index",
#'            y_col = "A",
#'            degrees = c(0, 120, 240),
#'            origin_fn = centroid)
#' df_rotated
#'
#' # Plot A and A rotated around overall centroid
#' ggplot(df_rotated, aes(x = Index_rotated, y = A_rotated, color = factor(.degrees))) +
#'   geom_hline(yintercept = mean(df$A), size = 0.2, alpha = .4, linetype="dashed") +
#'   geom_vline(xintercept = mean(df$Index), size = 0.2, alpha = .4, linetype="dashed") +
#'   geom_line(alpha = .4) +
#'   geom_point() +
#'   theme_minimal() +
#'   labs(x = "Index", y="Value", color="Degrees")
#'
#' # Rotate around group centroids
#' df_grouped <- df %>%
#'   dplyr::group_by(G) %>%
#'   rotate2d(x_col = "Index",
#'            y_col = "A",
#'            degrees = c(0, 120, 240),
#'            origin_fn = centroid)
#' df_grouped
#'
#' # Plot A and A rotated around group centroids
#' ggplot(df_grouped, aes(x=Index_rotated, y=A_rotated, color = factor(.degrees))) +
#'   geom_point() +
#'   theme_minimal() +
#'   labs(x = "Index", y="Value", color="Degrees")
#'
#' }
rotate2d <- function(data,
                     degrees,
                     x_col = NULL,
                     y_col = NULL,
                     suffix = "_rotated",
                     origin = c(0, 0),
                     origin_fn = NULL,
                     keep_original = TRUE,
                     degree_col_name = ".degrees") {
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_numeric(
    degrees,
    lower = -360,
    upper = 360,
    any.missing = FALSE,
    min.len = 1,
    add = assert_collection
  )
  checkmate::assert_string(x_col, null.ok = TRUE, add = assert_collection)
  checkmate::assert_string(y_col, null.ok = TRUE, add = assert_collection)
  checkmate::assert_string(suffix, add = assert_collection)
  checkmate::assert_string(degree_col_name, null.ok = TRUE, add = assert_collection)
  checkmate::assert_numeric(origin,
                            len = 2,
                            any.missing = FALSE,
                            add = assert_collection)
  checkmate::assert_function(origin_fn, null.ok = TRUE, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  if (is.data.frame(data) && is.null(y_col)) {
    assert_collection$push("when 'data' is a data frame, 'y_col' must be specified.")
  }
  if (is.data.frame(data) && is.null(x_col)) {
    assert_collection$push("when 'data' is a data frame, 'x_col' must be specified.")
  }
  if (length(c(x_col, y_col)) == 2 && x_col == y_col){
    assert_collection$push("'x_col' and 'y_col' cannot be the same column.")
  }
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Mutate for each degree
  purrr::map_dfr(
    .x = degrees,
    .f = function(degree) {
      out <- multi_mutator(
        data = data,
        mutate_fn = rotate2d_mutator_method,
        check_fn = NULL,
        force_df = TRUE,
        min_dims = 2,
        keep_original = keep_original,
        cols = c(x_col, y_col),
        degrees = degree,
        suffix = suffix,
        origin = origin,
        origin_fn = origin_fn
      )
      if (!is.null(degree_col_name)) {
        out[[degree_col_name]] <- degree
      }

      out
    }
  )

}

# col is the y_col
rotate2d_mutator_method <- function(data,
                                    cols,
                                    degrees,
                                    suffix,
                                    origin,
                                    origin_fn,
                                    new_name = NULL) {
  # Extract columns
  x_col <- cols[[1]]
  y_col <- cols[[2]]

  # Create rotation matrix based on the degrees
  if (degrees %in% c(-360, 0, 360)) {
    rotation_matrix <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2)
  } else if (degrees %in% c(90, -270)) {
    rotation_matrix <- matrix(c(0, 1,-1, 0), nrow = 2, ncol = 2)
  } else if (degrees %in% c(180, -180)) {
    rotation_matrix <- matrix(c(-1, 0, 0,-1), nrow = 2, ncol = 2)
  } else if (degrees %in% c(270, -90)) {
    rotation_matrix <- matrix(c(0,-1, 1, 0), nrow = 2, ncol = 2)
  } else {
    # 360 degrees == 2pi
    radian <- degrees * (pi / 180)
    rotation_matrix <- matrix(c(cos(radian), sin(radian),-sin(radian), cos(radian)),
                              nrow = 2,
                              ncol = 2)
  }

  # Extract x and y values
  if (is.null(x_col)) {
    x_col <- "Index"
    x <- seq_len(nrow(data))
  } else {
    x <- data[[x_col]]
  }
  y <- data[[y_col]]

  # Find origin if specified
  if (!is.null(origin_fn)) {
    origin <- tryCatch(
      origin_fn(x, y),
      error = function(e) {
        stop(paste0("failed to apply 'origin_fn': ", e))
      }
    )
    if (length(origin) != 2) {
      stop("output of 'origin_fn' did not have length 2.")
    }
    if (!is.numeric(origin)) {
      stop("output of 'origin_fn' was not numeric.")
    }
  }

  # Move origin
  x <- x - origin[[1]]
  y <- y - origin[[2]]

  # Convert to matrix
  xy_matrix <- rbind(x, y)

  # Apply rotation matrix
  xy_matrix <- rotation_matrix %*% xy_matrix

  # Extract x and y
  x <- xy_matrix[1,]
  y <- xy_matrix[2,]

  # Move origin
  x <- x + origin[[1]]
  y <- y + origin[[2]]

  # Add rotated columns to data
  data[[paste0(x_col, suffix)]] <- x
  data[[paste0(y_col, suffix)]] <- y

  data

}
