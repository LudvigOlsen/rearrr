
# Doesn't seem to be that meaningful. Leaving in case I get ideas.
sine_up_ <- function(data,
                     grp_id,
                     cols,
                     fs = NULL,
                     periods = NULL,
                     phase = 0,
                     suffix = "_sined",
                     overwrite = FALSE) {

  # Number of dimensions
  # Each column is a dimension
  num_dims <- length(cols)

  # Convert columns to list of vectors
  dim_vectors <- as.list(data[, cols, drop = FALSE])

  if (is.null(fs)) {
    fs <-
      apply_coordinate_fn_(
        dim_vectors = dim_vectors,
        coordinates = fs,
        fn = create_origin_fn(function(x) {
          max(x) - min(x)
        }),
        num_dims = num_dims,
        coordinate_name = "fs",
        fn_name = "fs_fn",
        dim_var_name = "cols",
        grp_id = grp_id,
        allow_len_one = TRUE
      )
  }

  if (!is.null(periods)) {
    fs <- fs / periods
  }

  print(fs)

  sined_dim_vectors <-
    purrr::map2(.x = dim_vectors, .y = fs, .f = ~ {
      .x * generate_sine_wave(.x,
        fs = .y,
        amplitude = 1,
        phase = phase
      )
    })

  # Add dim_vectors as columns with the suffix
  data <- add_dimensions_(
    data = data,
    new_vectors = setNames(sined_dim_vectors, cols),
    suffix = suffix,
    overwrite = overwrite
  )

  data
}
