
# TODO ADD:
# triangle
# square, cube, etc.

# 'sphere' is surface, 'ball' is content of the sphere

sample_shape_ <- function(n, dims = 3, shape = "sphere") {
  if (shape == "sphere") {
    # Generalized Muller Method (19) from
    # http://extremelearning.com.au/how-to-generate-uniformly-random-points-on-n-spheres-and-n-balls/

    cnames <- paste0("x", seq_len(dims))
    df <- matrix(
      data = rnorm(
        n = n * dims,
        mean = 0,
        sd = 1
      ),
      nrow = n,
      ncol = dims
    ) %>%
      as.data.frame() %>%
      setNames(nm = cnames) %>%
      vector_length(cols = cnames,
                    by_row = TRUE,
                    len_col_name = "norm") %>%
      dplyr::mutate_all( ~ . / .data$norm) %>%
      dplyr::select(-.data$norm)
  } else if (shape == "ball") {
    # Generalized Muller Method (20) from
    # http://extremelearning.com.au/how-to-generate-uniformly-random-points-on-n-spheres-and-n-balls/
    r <- runif(n = n, min = 0, max = 1) ^ (1 / dims)
    sample_shape_(n = n,
                 dims = dims,
                 shape = "sphere") %>%
      dplyr::mutate_all(
        .funs = function(x) {
          x * r
        }
      )
  }
}
