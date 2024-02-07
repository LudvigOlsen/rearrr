

#' Parent function for documentation inheritance
#'
#' Do not call. Only used to avoid repetition of documentation.
#'
#' @param data \code{data.frame} or \code{vector}.
#' @param origin_fn Function for finding the origin coordinates.
#'
#'  \strong{Input}: Each column will be passed as a \code{vector} in the order of \code{`cols`}.
#'
#'  \strong{Output}: A \code{vector} with one scalar per dimension.
#'
#'  Can be created with \code{\link[rearrr:create_origin_fn]{create_origin_fn()}} if you want to apply
#'  the same function to each dimension.
#'
#'  E.g. \code{`create_origin_fn(median)`} would find the median of each column.
#'
#'  \strong{Built-in functions} are \code{\link[rearrr:centroid]{centroid()}},
#'  \code{\link[rearrr:most_centered]{most_centered()}},
#'  and \code{\link[rearrr:midrange]{midrange()}}
#' @param check_fn Function with checks post-preparation of \code{`data`} and \code{`col(s)`}.
#'  Should not return anything.
#' @param allowed_types Allowed types of the \code{`col(s)`} columns. The type restrictions do not apply to
#'  columns not mentioned in the \code{`col(s)`} argument.
#' @param force_df Whether to return a \code{data.frame} when \code{`data`} is a \code{vector}.
#' @param min_dims Minimum number of dimensions (cols) after preparations. When \code{`data`} is a \code{vector}
#'  setting \code{`min_dims`} to \code{2} will use both the index and the values as columns.
#' @param overwrite Whether to allow overwriting of existing columns. (Logical)
#' @keywords internal
rearrr_fn_ <- function(data, origin_fn, check_fn, allowed_types, force_df, min_dims, overwrite){
  stop("'rearrr_fn' should not be called. Is used for documentation only.")
}


# Import R6 to avoid note

#' @import R6
NULL

