

#   __________________ #< b302a9a22d60de2964548d3fb69edf2d ># __________________
#   By distance                                                             ####



##  .................. #< d8417478f17fb09ccd87a81c78c0006b ># ..................
##  Closest to                                                              ####

#' @title Orders values by shortest distance to an origin
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Values are ordered by how close they are to the origin.
#'
#'  In 1d (when
#'  \code{`cols`} has length \code{1}), the origin can be thought of as a target value.
#'  In \emph{n} dimensions, the origin can be thought of as coordinates.
#'
#'  The origin can be supplied as coordinates or as a function that returns coordinates. The
#'  latter can be useful when supplying a grouped \code{data.frame} and ordering the rows by
#'  their distance to the centroid of each group.
#'
#'  The \code{*_vec()} version takes and returns a \code{vector}.
#'
#'  \strong{Example}:
#'
#'  The column values:
#'
#'  \code{c(1, 2, 3, 4, 5)}
#'
#'  and \code{origin = 2}
#'
#'  are \strong{ordered as}:
#'
#'  \code{c(}\strong{\code{2}}\code{, 1, 3, 4, 5)}
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family rearrange functions
#' @family distance functions
#' @inheritParams by_distance_rearranger_
#' @return
#'  The sorted \code{data.frame} (\code{tibble}) / \code{vector}.
#' @examples
#' # Attach packages
#' library(rearrr)
#' library(dplyr)
#'
#' # Set seed
#' set.seed(1)
#'
#' # Create a data frame
#' df <- data.frame(
#'   "index" = 1:10,
#'   "A" = sample(1:10),
#'   "B" = runif(10),
#'   "G" = c(
#'     1, 1, 1, 2, 2,
#'     2, 3, 3, 3, 3
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Closest to 3 in a vector
#' closest_to_vec(1:10, origin = 3)
#'
#' # Closest to the third row (index of data.frame)
#' closest_to(df, origin = 3)$index
#'
#' # By each of the columns
#' closest_to(df, cols = "A", origin = 3)$A
#' closest_to(df, cols = "A", origin_fn = most_centered)$A
#' closest_to(df, cols = "B", origin = 0.5)$B
#' closest_to(df, cols = "B", origin_fn = centroid)$B
#'
#' # Shuffle the elements with the same distance to the origin
#' closest_to(df,
#'   cols = "A",
#'   origin_fn = create_origin_fn(median),
#'   shuffle_ties = TRUE
#' )$A
#'
#' # Grouped by G
#' df %>%
#'   dplyr::select(G, A) %>% # For clarity
#'   dplyr::group_by(G) %>%
#'   closest_to(
#'     cols = "A",
#'     origin_fn = create_origin_fn(median)
#'   )
#'
#' # Plot the rearranged values
#' plot(
#'   x = 1:10,
#'   y = closest_to(df,
#'     cols = "B",
#'     origin_fn = create_origin_fn(median)
#'   )$B,
#'   xlab = "Position",
#'   ylab = "B"
#' )
#' plot(
#'   x = 1:10,
#'   y = closest_to(df,
#'     cols = "A",
#'     origin_fn = create_origin_fn(median),
#'     shuffle_ties = TRUE
#'   )$A,
#'   xlab = "Position",
#'   ylab = "A"
#' )
#'
#' # In multiple dimensions
#' df %>%
#'   closest_to(cols = c("A", "B"), origin_fn = most_centered)
closest_to <- function(data,
                       cols = NULL,
                       origin = NULL,
                       origin_fn = NULL,
                       shuffle_ties = FALSE,
                       origin_col_name = ".origin",
                       distance_col_name = ".distance",
                       overwrite = FALSE) {
  by_distance_rearranger_(
    data = data,
    cols = cols,
    origin = origin,
    origin_fn = origin_fn,
    shuffle_ties = shuffle_ties,
    decreasing = FALSE,
    origin_col_name = origin_col_name,
    distance_col_name = distance_col_name,
    overwrite = overwrite
  )
}

#' @rdname closest_to
#' @export
closest_to_vec <- function(data,
                           origin = NULL,
                           origin_fn = NULL,
                           shuffle_ties = FALSE){
  checkmate::assert_numeric(data)
  closest_to(
    data = data,
    origin = origin,
    origin_fn = origin_fn,
    shuffle_ties = shuffle_ties,
    origin_col_name = NULL,
    distance_col_name = NULL,
    overwrite = TRUE
  )
}



##  .................. #< 367167a5b5eaf6fa777a992c4f250900 ># ..................
##  Farthest from                                                           ####

#' @title Orders values by longest distance to an origin
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Values are ordered by how far they are from the origin.
#'
#'  In 1d (when
#'  \code{`cols`} has length \code{1}), the origin can be thought of as a target value.
#'  In \emph{n} dimensions, the origin can be thought of as coordinates.
#'
#'  The origin can be supplied as coordinates or as a function that returns coordinates. The
#'  latter can be useful when supplying a grouped \code{data.frame} and ordering the rows by
#'  their distance to the centroid of each group.
#'
#'  The \code{*_vec()} version takes and returns a \code{vector}.
#'
#'  \strong{Example}:
#'
#'  The column values:
#'
#'  \code{c(1, 2, 3, 4, 5)}
#'
#'  and \code{origin = 2}
#'
#'  are \strong{ordered as}:
#'
#'  \code{c(5, 4, 1, 3,} \strong{\code{2}}\code{)}
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family rearrange functions
#' @family distance functions
#' @inheritParams by_distance_rearranger_
#' @aliases farthest_from
#' @return
#'  The sorted \code{data.frame} (\code{tibble}) / \code{vector}.
#' @examples
#' \donttest{
#' # Attach packages
#' library(rearrr)
#' library(dplyr)
#'
#' # Set seed
#' set.seed(1)
#'
#' # Create a data frame
#' df <- data.frame(
#'   "index" = 1:10,
#'   "A" = sample(1:10),
#'   "B" = runif(10),
#'   "G" = c(
#'     1, 1, 1, 2, 2,
#'     2, 3, 3, 3, 3
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Furthest from 3 in a vector
#' furthest_from_vec(1:10, origin = 3)
#'
#' # Furthest from the third row (index of data.frame)
#' furthest_from(df, origin = 3)$index
#'
#' # By each of the columns
#' furthest_from(df, cols = "A", origin = 3)$A
#' furthest_from(df, cols = "A", origin_fn = most_centered)$A
#' furthest_from(df, cols = "B", origin = 0.5)$B
#' furthest_from(df, cols = "B", origin_fn = centroid)$B
#'
#' # Shuffle the elements with the same distance to the origin
#' furthest_from(df,
#'   cols = "A",
#'   origin_fn = create_origin_fn(median),
#'   shuffle_ties = TRUE
#' )$A
#'
#' # Grouped by G
#' df %>%
#'   dplyr::select(G, A) %>% # For clarity
#'   dplyr::group_by(G) %>%
#'   furthest_from(
#'     cols = "A",
#'     origin_fn = create_origin_fn(median)
#'   )
#'
#' # Plot the rearranged values
#' plot(
#'   x = 1:10,
#'   y = furthest_from(df,
#'     cols = "B",
#'     origin_fn = create_origin_fn(median)
#'   )$B,
#'   xlab = "Position", ylab = "B"
#' )
#' plot(
#'   x = 1:10,
#'   y = furthest_from(df,
#'     cols = "A",
#'     origin_fn = create_origin_fn(median),
#'     shuffle_ties = TRUE
#'   )$A,
#'   xlab = "Position", ylab = "A"
#' )
#'
#' # In multiple dimensions
#' df %>%
#'   furthest_from(cols = c("A", "B"), origin_fn = most_centered)
#' }
furthest_from <- function(data,
                          cols = NULL,
                          origin = NULL,
                          origin_fn = NULL,
                          shuffle_ties = FALSE,
                          origin_col_name = ".origin",
                          distance_col_name = ".distance",
                          overwrite = FALSE) {
  by_distance_rearranger_(
    data = data,
    cols = cols,
    origin = origin,
    origin_fn = origin_fn,
    shuffle_ties = shuffle_ties,
    decreasing = TRUE,
    origin_col_name = origin_col_name,
    distance_col_name = distance_col_name,
    overwrite = overwrite
  )
}

#' @rdname furthest_from
#' @export
furthest_from_vec <- function(data,
                              origin = NULL,
                              origin_fn = NULL,
                              shuffle_ties = FALSE) {
  checkmate::assert_numeric(data)
  furthest_from(
    data = data,
    origin = origin,
    origin_fn = origin_fn,
    shuffle_ties = shuffle_ties,
    origin_col_name = NULL,
    distance_col_name = NULL,
    overwrite = TRUE
  )
}
