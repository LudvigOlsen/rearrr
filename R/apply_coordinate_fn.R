

#   __________________ #< 440b147b963f8a7fd202661bfc3b068e ># __________________
#   Apply coordinate function                                               ####


apply_coordinate_fn <- function(dim_vectors,
                                coordinates,
                                fn,
                                num_dims,
                                coordinate_name,
                                fn_name,
                                dim_var_name,
                                allow_len_one = FALSE) {

  # Find coordinates if specified
  if (!is.null(fn)) {
    coordinates <- tryCatch(
      do.call(fn, dim_vectors),
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

  if (isTRUE(allow_len_one)) {
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

  coordinates
}

