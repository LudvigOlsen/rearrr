

#   __________________ #< 7987c9c27447df629ccf9474404fc327 ># __________________
#   Flip values                                                             ####


#' @title Flip the values around an origin
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  The values are flipped with the formula \code{x = 2 * c - x} where \code{x} is the value and \code{c} is
#'  the origin coordinate to flip the values around.
#'
#'  The origin can be supplied as coordinates or as a function that returns coordinates. The
#'  latter can be useful when supplying a grouped \code{data.frame} and flipping around e.g. the centroid
#'  of each group.
#'
#'  \strong{Example}:
#'
#'  The column values:
#'
#'  \code{c(5, 2, 7, 4, 3, 1)}
#'
#'  and the \code{origin_fn = create_origin_fn(median)}
#'
#'  Changes the values to :
#'
#'  \code{c(2, 5, 0, 3, 4, 6)}
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @param cols Names of columns in \code{`data`} to flip values of.
#' @param origin Coordinates of the origin to flip around.
#'  Must be either a single constant to use in all dimensions (columns)
#'  or a \code{vector} with one constant per dimension.
#'
#'  \strong{N.B.} Ignored when \code{`origin_fn`} is not \code{NULL}.
#' @param origin_fn Function for finding the origin value to flip the values around.
#'  Each column will be passed as a \code{vector} in the order of \code{`cols`}.
#'  It should return a \code{vector} with one constant per dimension.
#'
#'  Can be created with \code{\link[rearrr:create_origin_fn]{create_origin_fn()}} if you want to apply
#'  the same function to each dimension.
#'
#'  E.g. the \code{\link[rearrr:centroid]{centroid()}} function, which is created with:
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
#'
#' @param origin_col_name Name of new column with the origin coordinates. If \code{NULL}, no column is added.
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
#'   "Index" = 1:10,
#'   "A" = sample(1:10),
#'   "B" = runif(10),
#'   "G" = c(1, 1, 1, 2, 2,
#'           2, 3, 3, 3, 3),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Flip values of the columns
#' flip_values(df$A)
#' flip_values(df, cols = "A")
#' flip_values(df, cols = "B", origin = 0.3, keep_original = FALSE)
#' flip_values(df, cols = c("A", "B"), origin = c(3, 0.3),
#'             suffix = "",  keep_original = FALSE)
#' flip_values(df, cols = c("A", "B"), origin_fn = create_origin_fn(max))
#'
#' # Grouped by G
#' df %>%
#'   dplyr::group_by(G) %>%
#'   flip_values(cols = c("A", "B"),
#'               origin_fn = create_origin_fn(median),
#'               keep_original = FALSE)
#'
#' # Plot A and flipped A
#'
#' # First flip A around the median and then around the value 3.
#' df <- df %>%
#'   flip_values(cols = "A", suffix = "_flip_median", origin_col_name = NULL) %>%
#'   flip_values(cols = "A", suffix = "_flip_3", origin = 3, origin_col_name = NULL)
#'
#' # Plot A and A flipped around its median
#' ggplot(df, aes(x=Index, y=A)) +
#'   geom_line(aes(color="A")) +
#'   geom_line(aes(y=A_flip_median, color="Flipped A (median)")) +
#'   geom_hline(aes(color="Median A", yintercept = median(A))) +
#'   theme_minimal()
#'
#' # Plot A and A flipped around the value 3
#' ggplot(df, aes(x=Index, y=A)) +
#'   geom_line(aes(color="A")) +
#'   geom_line(aes(y=A_flip_3, color="Flipped A (3)")) +
#'   geom_hline(aes(color="3", yintercept = 3)) +
#'   theme_minimal()
#' }
flip_values <- function(data,
                        cols = NULL,
                        origin = 0,
                        origin_fn = create_origin_fn(median),
                        suffix = "_flipped",
                        keep_original = TRUE,
                        origin_col_name = ".origin") {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_numeric(origin,
                            min.len = 1,
                            any.missing = FALSE,
                            add = assert_collection)
  checkmate::assert_function(origin_fn, null.ok = TRUE, add = assert_collection)
  checkmate::assert_string(origin_col_name, null.ok = TRUE, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  multi_mutator(
    data = data,
    mutate_fn = flip_mutator_method,
    check_fn = NULL,
    suffix = suffix,
    keep_original = keep_original,
    cols = cols,
    origin = origin,
    origin_fn = origin_fn,
    origin_col_name = origin_col_name
  )
}

flip_mutator_method <- function(data,
                                cols,
                                suffix,
                                origin,
                                origin_fn,
                                origin_col_name) {

  # Number of dimensions
  # Each column is a dimension
  num_dims <- length(cols)

  # Convert columns to list of vectors
  dim_vectors <- as.list(data[, cols, drop = FALSE])

  # Find origin if specified
  origin <- apply_coordinate_fn(
    dim_vectors = dim_vectors,
    coordinates = origin,
    fn = origin_fn,
    num_dims = length(cols),
    coordinate_name = "origin",
    fn_name = "origin_fn",
    dim_var_name = "cols",
    allow_len_one = TRUE
  )

  # Flip around the origin
  dim_vectors <-
    purrr::map2(.x = dim_vectors, .y = origin, .f = ~ {
      flip_around_(vec=.x, around = .y)
    })

  # Add dim_vectors as columns with the suffix
  data <-
    add_dimensions(data = data,
                   new_vectors = dim_vectors,
                   suffix = suffix)

  # Add info columns
  if (!is.null(origin_col_name)) {
    data[[origin_col_name]] <- list_coordinates(origin, cols)
  }

  data
}

# The flipper
flip_around_ <- function(vec, around = median(vec)) {
  2 * around - vec
}
