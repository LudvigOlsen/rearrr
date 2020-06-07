

#   __________________ #< 7987c9c27447df629ccf9474404fc327 ># __________________
#   Flip values                                                             ####


#' @title Flip the values around a center value
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  The values are flipped with the formula \code{x = 2 * c - x} where \code{x} is the value and \code{c} is
#'  the center value to flip the values around. The center value is determined by the
#'  supplied \code{center_fn} function.
#'
#'  \strong{Example}:
#'
#'  The column values:
#'
#'  \code{c(5, 2, 7, 4, 3, 1)}
#'
#'  and the \code{center_fn = median}
#'
#'  Changes the values to :
#'
#'  \code{c(2, 5, 0, 3, 4, 6)}
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @param center_fn Function for finding the center value to flip the values around.
#' @export
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
#'   "index" = 1:10,
#'   "A" = sample(1:10),
#'   "B" = runif(10),
#'   "G" = c(1, 1, 1, 2, 2,
#'           2, 3, 3, 3, 3),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Flip values of each column
#' flip_values(df, col = "A", center_fn = median)$A
#' flip_values(df, col = "B", center_fn = median)$B
#'
#' # Grouped by G
#' df %>%
#'   dplyr::select(G, A) %>%  # For clarity
#'   dplyr::group_by(G) %>%
#'   flip_values(col = "A",
#'               new_name="A_flipped",
#'               center_fn = median)
#'
#' # Plot A and flipped A
#'
#' # First find flipped A around the median and around the value 3.
#' df <- df %>%
#'   flip_values(col = "A", new_name = "A_flip_median", center_fn = median) %>%
#'   flip_values(col = "A", new_name = "A_flip_3", center_fn = function(x){3})
#'
#' # Plot A and A flipped around its median
#' ggplot(df, aes(x=index, y=A)) +
#'   geom_line(aes(color="A")) +
#'   geom_line(aes(y=A_flip_median, color="Flipped A (median)")) +
#'   geom_hline(aes(color="Median A", yintercept = median(A))) +
#'   theme_minimal()
#'
#' # Plot A and A flipped around the value 3
#' ggplot(df, aes(x=index, y=A)) +
#'   geom_line(aes(color="A")) +
#'   geom_line(aes(y=A_flip_3, color="Flipped A (3)")) +
#'   geom_hline(aes(color="3", yintercept = 3)) +
#'   theme_minimal()
#' }
flip_values <- function(data,
                        cols = NULL,
                        center = 0,
                        center_fn = create_origin_fn(median),
                        suffix = "_flipped",
                        keep_original = TRUE,
                        center_col_name = ".center") {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_numeric(center,
                            min.len = 1,
                            any.missing = FALSE,
                            add = assert_collection)
  checkmate::assert_function(center_fn, null.ok = TRUE, add = assert_collection)
  checkmate::assert_string(center_col_name, null.ok = TRUE, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  multi_mutator(
    data = data,
    mutate_fn = flip_mutator_method,
    check_fn = NULL,
    suffix = suffix,
    keep_original = keep_original,
    cols = cols,
    center = center,
    center_fn = center_fn,
    center_col_name = center_col_name
  )
}

flip_mutator_method <- function(data,
                                cols,
                                suffix,
                                center,
                                center_fn,
                                center_col_name) {

  # Number of dimensions
  # Each column is a dimension
  num_dims <- length(cols)

  # Convert columns to list of vectors
  dim_vectors <- as.list(data[, cols, drop = FALSE])

  # Find center if specified
  if (!is.null(center_fn)) {
    center <- tryCatch(
      do.call(center_fn, dim_vectors),
      error = function(e) {
        stop(paste0("failed to apply 'center_fn': ", e))
      }
    )
    center_check_msg <- "output of 'center_fn'"
  } else {
    center_check_msg <- "'center'"
  }

  if (length(center) %ni% c(1, num_dims)) {
    stop(
      paste0(
        center_check_msg,
        " must have either length 1 or same",
        " length as 'cols' (",
        num_dims,
        ") but had length ",
        length(center),
        "."
      )
    )
  }
  if (!is.numeric(center)) {
    stop(paste0(center_check_msg, " was not numeric."))
  }

  # Flip around the center
  dim_vectors <-
    purrr::map2(.x = dim_vectors, .y = center, .f = ~ {
      flip_around_(vec=.x, around = .y)
    })

  # Add dim_vectors as columns with the suffix
  data <-
    add_dimensions(data = data,
                   new_vectors = dim_vectors,
                   suffix = suffix)

  # Add info columns
  if (!is.null(center_col_name)) {
    data[[center_col_name]] <- list_coordinates(center, cols)
  }

  data
}

# The flipper
flip_around_ <- function(vec, around = median(vec)) {
  2 * around - vec
}
