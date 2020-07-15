
# Wrapper functions for applying coordinate / origin functions
# to either vectors or data frame columns


#' Apply coordinate function (internal wrapper)
#' @keywords internal
#' @param ... \code{Numeric vectors} or a single \code{data.frame}.
#' @param cols Names of columns to use when \code{`...`} is a single \code{data.frame}.
#' @param coord_fn Function that takes each \code{vector}/\code{column} as separate args via \code{...} and
#'  returns a \code{vector} with one value per input \code{vector}/\code{column}.
#' @param fn_name Name of applied function for messages.
#' @param coordinate_name Name of coordinates (e.g. \code{"centroid"}) for messages.
apply_coord_fn_ <- function(...,
                            cols,
                            coord_fn,
                            fn_name,
                            coordinate_name) {
  if (is.data.frame(list(...)[[1]])) {
    if (rlang::dots_n(...) != 1) {
      stop("When passing a data.frame in '...', the length of '...' should be 1.")
    }
    apply_coord_fn_df_(
      data = list(...)[[1]],
      cols = cols,
      coord_fn = coord_fn,
      fn_name = fn_name,
      coordinate_name = coordinate_name
    )
  } else {
    if (!is.null(cols)) {
      stop("'cols' should only be specified when '...' contains a single data.frame.")
    }
    coord_fn(...)
  }
}

apply_coord_fn_df_ <- function(data,
                               cols,
                               coord_fn,
                               fn_name,
                               coordinate_name) {
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(data, add = assert_collection)
  checkmate::assert_character(
    cols,
    min.chars = 1,
    any.missing = FALSE,
    min.len = 1,
    unique = TRUE,
    add = assert_collection
  )
  checkmate::assert_string(fn_name, add = assert_collection)
  checkmate::assert_string(coordinate_name, add = assert_collection)
  checkmate::assert_function(coord_fn, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  multi_mutator_(
    data = data,
    mutate_fn = apply_coord_fn_df_mutator_method_,
    check_fn = NULL,
    min_dims = 1,
    cols = cols,
    force_df = TRUE,
    keep_original = TRUE,
    coord_fn = coord_fn,
    fn_name = fn_name,
    coordinate_name = coordinate_name
  )
}


apply_coord_fn_df_mutator_method_ <- function(data,
                                              grp_id,
                                              cols,
                                              coord_fn,
                                              fn_name,
                                              coordinate_name,
                                              ...) {
  # Convert columns to list of vectors
  dim_vectors <- as.list(data[, cols, drop = FALSE])

  # Find origin if specified
  coords <- apply_coordinate_fn_(
    dim_vectors = dim_vectors,
    coordinates = NULL,
    fn = coord_fn,
    num_dims = length(cols),
    coordinate_name = coordinate_name,
    fn_name = fn_name,
    dim_var_name = "cols",
    grp_id = grp_id,
    allow_len_one = TRUE
  )

  as.data.frame(as.list(coords)) %>%
    setNames(cols)
}
