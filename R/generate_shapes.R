


# generate_shapes(100) %>%
#   distance(cols = c(".object_x", ".object_y"), origin=c(0,0)) %>%
#   ggplot(aes(x = .object_x, y = .object_y, color = .object, alpha=-.distance)) +
#   geom_point(size=0.1) +
#   theme_minimal() +
#   theme(legend.position = "none")

# generate_shapes(100, shape="circle") %>%
#   distance(cols = c(".object_x", ".object_y"), origin=c(0,0)) %>%
#   ggplot(aes(x = .object_x, y = .object_y, color = .object, alpha=-.distance)) +
#   geom_path(size=0.2) +
#   theme_minimal() +
#   theme(legend.position = "none")

# generate_shapes(50, shape="triangle") %>%
#   distance(cols = c(".object_x", ".object_y"), origin=c(0,0)) %>%
#   ggplot(aes(x = .object_x, y = .object_y, color = .object, alpha=-.distance)) +
#   geom_path(size=0.2) +
#   theme_minimal() +
#   theme(legend.position = "none")

generate_shapes <- function(num_objects,
                            num_points = 300 * num_objects,
                            .min = -1,
                            .max = 1,
                            shape = "square",
                            compactness = 3.0,
                            ensure_one = TRUE) {
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_count(num_objects, positive = TRUE, add = assert_collection)
  checkmate::assert_count(num_points, positive = TRUE, add = assert_collection)
  checkmate::assert_number(.min, finite = TRUE, add = assert_collection)
  checkmate::assert_number(.max, finite = TRUE, add = assert_collection)
  checkmate::assert_number(compactness, finite = TRUE, add = assert_collection)
  checkmate::assert_flag(ensure_one, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  if (.min >= .max) {
    assert_collection$push("'.min' must be lower than '.max'.")
  }

  # Check 'shape'
  checkmate::assert(
    checkmate::check_character(
      shape,
      min.chars = 1,
      min.len = 1,
      any.missing = FALSE,
      unique = TRUE
    ),
    checkmate::check_list(
      shape,
      types = "numeric",
      any.missing = FALSE,
      min.len = 1,
      names = "unique"
    )
  )
  shape_nums <- NULL
  if (is.list(shape)) {
    shape_nums <- unlist(shape, use.names = FALSE, recursive = TRUE)
    shape <- names(shape)
    checkmate::assert_numeric(
      shape_nums,
      lower = 0,
      finite = TRUE,
      len = length(shape),
      add = assert_collection
    )
    if (mean(!is_between_(shape_nums, a = 0, b = 1)) %ni% c(0, 1)) {
      assert_collection$push("numbers in 'shape' must either be {all *between* 0 and 1} or {all >= 1}.")
    }
    checkmate::reportAssertions(assert_collection)
    if (all(is_between_(shape_nums, a = 0, b = 1)) &&
      round(sum(shape_nums), digits = 4) != 1) {
      assert_collection$push("when numbers in 'shape' are between 0 and 1, they must sum to 1.")
    }
    if (all(shape_nums >= 1)) {
      if (!checkmate::test_integerish(shape_nums)) {
        assert_collection$push("when numbers in 'shape' are >= 1, they must be counts (integer-like).")
      }
      if (sum(shape_nums) != num_objects) {
        assert_collection$push("when numbers in 'shape' are >= 1, they must sum to 'num_objects'.")
      }
    }
  }
  checkmate::assert_names(
    shape,
    subset.of = c("square", "triangle", "circle", "hexagon"),
    add = assert_collection
  )

  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Create initial clusters of data points
  data <- gen_initial_clusters_(
    num_points = num_points,
    num_objects = num_objects,
    compactness = compactness,
    .min = .min,
    .max = .max
  )

  # Distribute shapes for the objects
  shapes <- distribute_shapes_(
    shape = shape,
    shape_nums = shape_nums,
    num_objects = num_objects,
    ensure_one = ensure_one
  )

  # Add shape to cluster data
  clusters <- data %>%
    dplyr::count(.data$.object) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(.shape = shapes)
  data <- data %>%
    dplyr::left_join(clusters, by = ".object")

  # The extra columns per shape
  info_cols <- c()
  object_dfs <- list()

  if ("square" %in% shapes) {
    tmp_data <- data %>%
      dplyr::filter(.data$.shape == "square")
    object_dfs <- append(object_dfs, gen_square_(data = tmp_data))
    info_cols <- append(info_cols, ".edge")
  }

  if ("circle" %in% shapes) {
    tmp_data <- data %>%
      dplyr::filter(.data$.shape == "circle")
    object_dfs <- append(object_dfs, gen_circle_(data = tmp_data))
    info_cols <- append(info_cols, ".degrees")
  }

  if ("triangle" %in% shapes) {
    tmp_data <- data %>%
      dplyr::filter(.data$.shape == "triangle")
    object_dfs <-
      append(object_dfs, gen_triangle_(data = tmp_data))
    info_cols <- append(info_cols, ".edge")
  }

  if ("hexagon" %in% shapes) {
    tmp_data <- data %>%
      dplyr::filter(.data$.shape == "hexagon")
    object_dfs <-
      append(object_dfs, gen_hexagon_(data = tmp_data))
    info_cols <- append(info_cols, ".edge")
  }

  dplyr::bind_rows(object_dfs) %>%
    dplyr::mutate(.object_x = .data$.object_x + .data$centroid_x) %>%
    dplyr::select(
      .data$.object,
      .data$.shape,
      .data$.object_x,
      .data$.object_y,
      dplyr::one_of(unique(info_cols))
    )
}

distribute_shapes_ <- function(shape,
                               shape_nums,
                               num_objects,
                               ensure_one) {
  # If only one shape
  if (length(shape) == 1) {
    return(rep(shape, num_objects))
  }

  # Specific counts
  if (!is.null(shape_nums) &&
    all(shape_nums >= 1)) {
    return(sample(rep(shape, shape_nums)))
  }

  # Probabilities (sampling)

  # Init shape distribution vector
  sampled_shapes <- c()
  # Copy 'num_objects' so we can change it
  sampling_num_objects <- num_objects

  if (isTRUE(ensure_one) &&
    length(shape) <= num_objects) {
    sampled_shapes <- append(sampled_shapes, shape)
    sampling_num_objects <- sampling_num_objects - length(shape)
  }
  if (sampling_num_objects > 0) {
    sampled_shapes <- append(
      sampled_shapes,
      sample(
        shape,
        size = sampling_num_objects,
        replace = TRUE,
        prob = shape_nums
      )
    )
  }

  return(sample(sampled_shapes))
}

gen_square_ <- function(data) {
  data %>%
    square(
      y_col = ".object_y",
      x_col_name = ".object_x"
    ) %>%
    dplyr::arrange(
      .data$.object,
      .data$.edge,
      .data$.object_x,
      .data$.object_y
    ) %>%
    dplyr::group_by(.data$.object) %>%
    rotate_2d(
      degrees = 45,
      x_col = ".object_x",
      y_col = ".object_y",
      suffix = "",
      origin_fn = midrange,
      degrees_col_name = NULL
    ) %>%
    list()
}

gen_circle_ <- function(data) {
  data %>%
    circularize(y_col = ".object_y", x_col_name = ".object_x") %>%
    dplyr::arrange(
      .data$.object,
      .data$.degrees,
      .data$.object_x,
      .data$.object_y
    ) %>%
    list()
}

gen_triangle_ <- function(data) {
  data %>%
    triangularize(y_col = ".object_y", x_col_name = ".object_x") %>%
    dplyr::arrange(
      .data$.object,
      .data$.edge,
      .data$.object_x,
      .data$.object_y
    ) %>%
    list()
}

gen_hexagon_ <- function(data) {
  data %>%
    hexagonalize(y_col = ".object_y", x_col_name = ".object_x") %>%
    dplyr::arrange(
      .data$.object,
      .data$.edge,
      .data$.object_x,
      .data$.object_y
    ) %>%
    list()
}

gen_initial_clusters_ <- function(num_points,
                                  num_objects,
                                  compactness,
                                  .min,
                                  .max) {
  generate_clusters(
    num_rows = num_points,
    num_cols = 2,
    num_clusters = num_objects,
    compactness = compactness
  ) %>%
    dplyr::group_by(.data$.cluster) %>%
    dplyr::mutate(
      D1 = min_max_scale(
        .data$D1,
        new_min = .min,
        new_max = .max,
        old_min = 0,
        old_max = 1
      ),
      D2 = min_max_scale(
        .data$D2,
        new_min = .min,
        new_max = .max,
        old_min = 0,
        old_max = 1
      ),
      centroid_x = centroid(.data$D2, .data$D1)[[1]]
    ) %>%
    dplyr::rename(
      .object_y = .data$D1,
      .object = .data$.cluster
    )
}
