

# TODO Add docs
flip_values <- function(data,
                        col = NULL,
                        center_fn = median) {
  mutator(
    data = data,
    mutate_fn = flip_mutator_method,
    check_fn = NULL,
    col = col,
    center_fn = center_fn
  )
}

flip_mutator_method <- function(data,
                                col,
                                center_fn,
                                center_what = "index") {
  flip_around <- function(vec, around = median(x)) {
    2 * around - vec
  }

  # Find center value
  center <- center_fn(data[[col]])
  flipped_vals = flip_around(vec = data[[col]], around = center)
  data <- data[order(flipped_vals), , drop = FALSE]
  data
}
