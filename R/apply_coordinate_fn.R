

#   __________________ #< 440b147b963f8a7fd202661bfc3b068e ># __________________
#   Apply coordinate function                                               ####


apply_coordinate_fn_ <- function(dim_vectors,
                                 coordinates,
                                 fn,
                                 num_dims,
                                 coordinate_name,
                                 fn_name,
                                 dim_var_name,
                                 grp_id,
                                 allow_len_one = FALSE,
                                 extra_args = NULL) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_list(dim_vectors, types = "numeric", any.missing = FALSE, min.len = 1, add = assert_collection)
  checkmate::assert_numeric(coordinates, any.missing = FALSE, null.ok = TRUE, min.len = 1, add = assert_collection)
  checkmate::assert_function(fn, null.ok = TRUE, add = assert_collection)
  checkmate::assert_number(num_dims, lower = 1, add = assert_collection)
  checkmate::assert_number(grp_id, lower = 1, add = assert_collection)
  checkmate::assert_string(coordinate_name, add = assert_collection)
  checkmate::assert_string(fn_name, add = assert_collection)
  checkmate::assert_string(dim_var_name, null.ok = TRUE, add = assert_collection)
  checkmate::assert_flag(allow_len_one, add = assert_collection)
  checkmate::reportAssertions(assert_collection)

  if ((is.null(coordinates) && is.null(fn))) {
    assert_collection$push(
      paste0(
        "At least one of {'",
        coordinate_name,
        "', '",
        fn_name,
        "'} must be specified (not 'NULL')."
      )
    )
  }
  if (!is.null(coordinates) && !is.null(fn)){
    if (grp_id == 1){
      message(paste0(
        "When '", fn_name,
        "' is specified, '",
        coordinate_name,
        "' is ignored."
      ))
    }
    coordinates <- NULL
  }
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Find coordinates if specified
  if (!is.null(fn)) {
    coordinates <- tryCatch(
      rlang::exec(fn, !!!dim_vectors, !!!extra_args),
      error = function(e) {
        stop(paste0("failed to apply '", fn_name, "': ", e))
      },
      warning = function(w) {
        warning(
          paste0(
            "warning when applying '",
            fn_name,
            "': ",
            w,
            "\nHint: Does ",
            fn_name,
            " take multiple vectors as arguments?"
          )
        )
        do.call(fn, dim_vectors)
      },
      message = function(m) {
        message(paste0("message when applying '", fn_name, "': ", m))
      }
    )
    check_msg <- paste0("output of '", fn_name, "'")
    allow_len_one <- FALSE
  } else {
    check_msg <- paste0("'", coordinate_name, "'")
  }

  if (is.null(dim_var_name) && num_dims == 1) {
    if (length(coordinates) != 1) {
      stop(
        paste0(
          check_msg,
          " must have length 1 but had length ",
          length(coordinates),
          "."
        )
      )
    }
  } else if (isTRUE(allow_len_one)) {
    if (length(coordinates) %ni% c(1, num_dims)) {
      stop(
        paste0(
          check_msg,
          " must have either length 1 or same",
          " length as '",
          dim_var_name,
          "' (",
          num_dims,
          ") but had length ",
          length(coordinates),
          "."
        )
      )
    }
  } else if (length(coordinates) != num_dims) {
    stop(
      paste0(
        check_msg,
        " must have same length as '",
        dim_var_name,
        "' (",
        num_dims,
        ") but had length ",
        length(coordinates),
        "."
      )
    )
  }

  if (!is.numeric(coordinates)) {
    stop(paste0(check_msg, " was not numeric."))
  }

  if (any(is.na(coordinates))) {
    stop(paste0(check_msg, " contained missing values (NAs)."))
  }

  coordinates
}
