
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

generate_shapes <- function(num_shapes, num_points = 300 * num_shapes, .min = -1, .max = 1, shape="square"){

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_names(shape, subset.of = c("square", "triangle", "circle", "hexagon"), add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  clusters <- generate_clusters(num_rows = num_points, num_cols = 2, num_clusters = num_shapes, compactness = 2.5) %>%
    dplyr::group_by(.data$.cluster) %>%
    dplyr::mutate(D1 = min_max_scale(.data$D1, new_min = .min, new_max = .max, old_min = 0, old_max = 1),
                  D2 = min_max_scale(.data$D2, new_min = .min, new_max = .max, old_min = 0, old_max = 1),
                  centroid_x = centroid(.data$D2, .data$D1)[[1]]) %>%
    dplyr::rename(.object_y = .data$D1,
                  .object = .data$.cluster)

  if (shape == "square"){
    objects <- square(data = clusters, y_col = ".object_y", x_col_name = ".object_x") %>%
      dplyr::arrange(.data$.object, .data$.edge, .data$.object_x, .data$.object_y) %>%
      dplyr::group_by(.data$.object) %>%
      rotate_2d(degrees = 45, x_col = ".object_x", y_col = ".object_y", suffix = "", origin_fn = centroid,degrees_col_name = NULL)

    info_col <- ".edge"
  } else if (shape == "circle"){
    objects <- circularize(data = clusters, y_col = ".object_y", x_col_name = ".object_x") %>%
      dplyr::arrange(.data$.object, .data$.degrees, .data$.object_x, .data$.object_y)

    info_col <- ".degrees"
  } else if (shape == "triangle"){
    objects <- triangularize(data = clusters, y_col = ".object_y", x_col_name = ".object_x") %>%
      dplyr::arrange(.data$.object, .data$.edge, .data$.object_x, .data$.object_y)

    info_col <- ".edge"
  } else if (shape == "hexagon"){
    objects <- hexagonalize(data = clusters, y_col = ".object_y", x_col_name = ".object_x") %>%
      dplyr::arrange(.data$.object, .data$.edge, .data$.object_x, .data$.object_y)

    info_col <- ".edge"
  }

  objects %>%
    dplyr::mutate(.object_x = .data$.object_x + .data$centroid_x) %>%
    dplyr::select(.object, .object_x, .object_y, dplyr::one_of(info_col))

}
