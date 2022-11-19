

#   __________________ #< 60cfc78f594e5611a6eaaf34a2b212ae ># __________________
#   Apply transformation matrix                                             ####


#' @title Apply transformation matrix to a set of columns
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Perform \link[base:matmult]{matrix multiplication} with a transformation matrix and a set of \code{data.frame} columns.
#'
#'  The data points in \code{`data`} are moved prior to the transformation, to bring
#'  the origin to \code{0} in all dimensions. After the transformation, the
#'  inverse move is applied to bring the origin back to its original position. See \code{`Details`} section.
#'
#'  The columns in \code{`data`} are transposed, making the operation (without the origin movement):
#'  \deqn{mat · data[, cols]^T}
#'
#'  The origin can be supplied as coordinates or as a function that returns coordinates. The
#'  latter can be useful when supplying a grouped \code{data.frame} and transforming around e.g. the centroid
#'  of each group.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @param mat Transformation \code{matrix}. Must have the same number of columns as \code{`cols`}.
#' @param origin Coordinates of the origin. \code{Vector} with the same number
#'  of elements as \code{`cols`} (i.e. origin_x, origin_y, ...).
#'  Ignored when \code{`origin_fn`} is not \code{NULL}.
#' @param origin_col_name Name of new column with the origin coordinates. If \code{NULL}, no column is added.
#' @export
#' @return \code{data.frame} (\code{tibble}) with the new, transformed columns and the origin coordinates.
#' @details
#'  Example with 2 columns (\code{x}, \code{y}) and a 2x2 transformation matrix:
#'
#'  \itemize{
#'   \item{Move origin to \code{(0, 0)}:
#'
#'   \code{x = x - origin_x}
#'
#'   \code{y = y - origin_y}}
#'   \item{Convert to transposed matrix:
#'
#'   \code{data_mat = rbind(x, y)}}
#'   \item{Matrix multiplication:
#'
#'   \code{transformed = mat \%*\% data_mat}}
#'   \item{Move origin to original position (after extraction from \code{transformed}):
#'
#'   \code{x = x + origin_x}
#'
#'   \code{y = y + origin_y}}
#'  }
#' @family mutate functions
#' @inheritParams multi_mutator_
#' @examples
#' # Attach packages
#' library(rearrr)
#' library(dplyr)
#' has_ggplot <- require(ggplot2)  # Attach if installed
#'
#' # Set seed
#' set.seed(3)
#'
#' # Create a data frame
#' df <- data.frame(
#'   "x" = 1:12,
#'   "y" = 13:24,
#'   "z" = runif(12),
#'   "g" = c(
#'     1, 1, 1, 1, 2, 2,
#'     2, 2, 3, 3, 3, 3
#'   )
#' )
#'
#' # Apply identity matrix
#' mat <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = 3)
#' apply_transformation_matrix(
#'   data = df,
#'   mat = mat,
#'   cols = c("x", "y", "z"),
#'   origin = c(0, 0, 0)
#' )
#'
#' # Apply rotation matrix
#' # 90 degrees around z-axis
#' # Origin is the most centered point
#' mat <- matrix(c(0, 1, 0, -1, 0, 0, 0, 0, 1), nrow = 3)
#' res <- apply_transformation_matrix(
#'   data = df,
#'   mat = mat,
#'   cols = c("x", "y", "z"),
#'   origin_fn = most_centered
#' )
#'
#' # Plot the rotation
#' # z wasn't changed so we plot x and y
#' if (has_ggplot){
#'   res %>%
#'     ggplot(aes(x = x, y = y)) +
#'     geom_point() +
#'     geom_point(aes(x = x_transformed, y = y_transformed)) +
#'     theme_minimal()
#' }
#'
#' # Apply rotation matrix to grouped data frame
#' # Around centroids
#' # Same matrix as before
#' res <- apply_transformation_matrix(
#'   data = dplyr::group_by(df, g),
#'   mat = mat,
#'   cols = c("x", "y", "z"),
#'   origin_fn = centroid
#' )
#'
#' # Plot the rotation
#' if (has_ggplot){
#'   res %>%
#'     ggplot(aes(x = x, y = y, color = g)) +
#'     geom_point() +
#'     geom_point(aes(x = x_transformed, y = y_transformed)) +
#'     theme_minimal()
#' }
apply_transformation_matrix <- function(data,
                                        mat,
                                        cols,
                                        origin = NULL,
                                        origin_fn = NULL,
                                        suffix = "_transformed",
                                        keep_original = TRUE,
                                        origin_col_name = ".origin",
                                        overwrite = FALSE) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(data, add = assert_collection)
  checkmate::assert_matrix(mat, add = assert_collection)
  checkmate::assert_character(
    cols,
    any.missing = FALSE,
    min.chars = 1,
    min.len = 1,
    unique = TRUE,
    add = assert_collection
  )
  checkmate::assert_data_frame(data, min.cols = 3, add = assert_collection)
  checkmate::assert_numeric(
    origin,
    min.len = 1,
    any.missing = FALSE,
    null.ok = TRUE,
    add = assert_collection
  )
  checkmate::assert_function(origin_fn, null.ok = TRUE, add = assert_collection)
  checkmate::assert_string(suffix, add = assert_collection)
  checkmate::assert_string(origin_col_name, null.ok = TRUE, add = assert_collection)
  checkmate::assert_flag(overwrite, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  checkmate::assert_names(
    cols,
    subset.of = colnames(data),
    add = assert_collection
  )
  checkmate::reportAssertions(assert_collection)
  if (ncol(mat) != length(cols)){
    assert_collection$push("The number of columns in 'mat' must be the same as the length of 'cols'.")
  }
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Mutate with each multiplier
  multi_mutator_(
    data = data,
    mutate_fn = apply_transformation_matrix_mutator_method_,
    check_fn = NULL,
    cols = cols,
    mat = mat,
    suffix = suffix,
    overwrite = overwrite,
    force_df = TRUE,
    keep_original = keep_original,
    origin = origin,
    origin_fn = origin_fn,
    origin_col_name = origin_col_name
  )

}

apply_transformation_matrix_mutator_method_ <- function(data,
                                                        grp_id,
                                                        cols,
                                                        overwrite,
                                                        mat,
                                                        suffix,
                                                        origin,
                                                        origin_fn,
                                                        origin_col_name,
                                                        ...) {
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
    allow_len_one = FALSE
  )

  # Apply transformation matrix
  dim_vectors <- apply_transformation_matrix_dim_vectors_(
    dim_vectors = dim_vectors,
    mat = mat,
    cols = cols,
    origin = origin
  )

  # Add transformed columns to data
  # Add dim_vectors as columns with the suffix
  data <- add_dimensions_(
    data = data,
    new_vectors = setNames(dim_vectors, cols),
    suffix = suffix,
    overwrite = overwrite
  )

  # Add info columns
  if (!is.null(origin_col_name)) {
    data[[origin_col_name]] <- list_coordinates_(origin, cols)
  }

  data
}


# Apply transformation to dim_vectors
# Used internally
apply_transformation_matrix_dim_vectors_ <- function(dim_vectors, mat, cols, origin){

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_list(dim_vectors, types = "numeric", any.missing = FALSE, add = assert_collection)
  checkmate::assert_matrix(mat, mode = "numeric", any.missing = FALSE, add = assert_collection)
  checkmate::assert_character(cols, min.len = 1, any.missing = FALSE, add = assert_collection)
  checkmate::assert_numeric(origin, any.missing = FALSE, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Move origin
  # x <- x - origin_coordinate
  if (!is_zero_vector_(origin)){
    dim_vectors <-
      purrr::map2(.x = dim_vectors, .y = origin, .f = ~ {
        .x - .y
      })
  }

  # Convert to matrix
  cols_matrix <- do.call(rbind, dim_vectors)

  # Apply rotation matrix
  cols_matrix <- mat %*% cols_matrix

  # Extract dimensions
  dim_vectors <-
    purrr::map(.x = seq_len(nrow(cols_matrix)), .f = ~ {
      as.vector(cols_matrix[.x, ])
    })

  # Move origin
  # x <- x + origin_coordinate
  if (!is_zero_vector_(origin)){
    dim_vectors <-
      purrr::map2(.x = dim_vectors, .y = origin, .f = ~ {
        .x + .y
      })
  }

  dim_vectors

}
