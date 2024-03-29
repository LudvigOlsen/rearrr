library(rearrr)
context("scalers")


test_that("min_max_scale()", {
  xpectr::set_test_seed(42)

  vec <- runif(10, min = -4, max = 3)

  # Generate expectations for 'min_max_scale'
  # Tip: comment out the gxs_function() call
  # so it is easy to regenerate the tests
  xpectr::set_test_seed(42)
  # xpectr::gxs_function(
  #   fn = min_max_scale,
  #   args_values = list(
  #     "x" = list(vec, c(0, 0, 0), "hej", c(vec, NA)),
  #     "new_min" = list(0, 5, NA),
  #     "new_max" = list(1, 6, NA),
  #     "old_min" = list(NULL, -5, NA),
  #     "old_max" = list(NULL, 6, NA),
  #     "na.rm" = list(TRUE, FALSE)
  #   ),extra_combinations = list(
  #     list("x" = c(vec, NA), "na.rm" = FALSE)
  #   ),
  #   indentation = 2
  # )


  ## Testing 'min_max_scale'                                                  ####
  ## Initially generated by xpectr
  # Testing different combinations of argument values

  # Testing min_max_scale(x = vec, new_min = 0, new_max ...
  xpectr::set_test_seed(42)
  # Assigning output
  output_19148 <- min_max_scale(x = vec, new_min = 0, new_max = 1, old_min = NULL, old_max = NULL, na.rm = TRUE)
  # Testing class
  expect_equal(
    class(output_19148),
    "numeric",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_19148,
    type = "double")
  # Testing values
  expect_equal(
    output_19148,
    c(0.97225, 1, 0.18877, 0.86712, 0.63195, 0.47909, 0.75014, 0, 0.65095,
      0.71086),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_19148),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_19148),
    10L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_19148)),
    10L)

  # Testing min_max_scale(x = c(0, 0, 0), new_min = 0, n...
  # Changed from baseline: x = c(0, 0, 0)
  xpectr::set_test_seed(42)
  # Assigning output
  output_19370 <- min_max_scale(x = c(0, 0, 0), new_min = 0, new_max = 1, old_min = NULL, old_max = NULL, na.rm = TRUE)
  # Testing class
  expect_equal(
    class(output_19370),
    "numeric",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_19370,
    type = "double")
  # Testing values
  expect_equal(
    output_19370,
    c(0, 0, 0),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_19370),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_19370),
    3L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_19370)),
    3L)

  # Testing min_max_scale(x = "hej", new_min = 0, new_ma...
  # Changed from baseline: x = "hej"
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_12861 <- xpectr::capture_side_effects(min_max_scale(x = "hej", new_min = 0, new_max = 1, old_min = NULL, old_max = NULL, na.rm = TRUE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_12861[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'x': Must be of type 'numeric', not 'character'."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_12861[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing min_max_scale(x = c(vec, NA), new_min = 0, n...
  # Changed from baseline: x = c(vec, NA)
  xpectr::set_test_seed(42)
  # Assigning output
  output_18304 <- min_max_scale(x = c(vec, NA), new_min = 0, new_max = 1, old_min = NULL, old_max = NULL, na.rm = TRUE)
  # Testing class
  expect_equal(
    class(output_18304),
    "numeric",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_18304,
    type = "double")
  # Testing values
  expect_equal(
    output_18304,
    c(0.97225, 1, 0.18877, 0.86712, 0.63195, 0.47909, 0.75014, 0, 0.65095,
      0.71086, NA),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_18304),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_18304),
    11L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_18304)),
    11L)

  # Testing min_max_scale(x = NULL, new_min = 0, new_max...
  # Changed from baseline: x = NULL
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_16417 <- xpectr::capture_side_effects(min_max_scale(x = NULL, new_min = 0, new_max = 1, old_min = NULL, old_max = NULL, na.rm = TRUE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_16417[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'x': Must be of type 'numeric', not 'NULL'."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_16417[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing min_max_scale(x = c(vec, NA), new_min = 0, n...
  # Changed from baseline: x, na.rm
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_15190 <- xpectr::capture_side_effects(min_max_scale(x = c(vec, NA), new_min = 0, new_max = 1, old_min = NULL, old_max = NULL, na.rm = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_15190[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'x': Contains missing values (element 11)."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_15190[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing min_max_scale(x = vec, new_min = 5, new_max ...
  # Changed from baseline: new_min = 5
  xpectr::set_test_seed(42)
  # Assigning output
  output_17365 <- min_max_scale(x = vec, new_min = 5, new_max = 1, old_min = NULL, old_max = NULL, na.rm = TRUE)
  # Testing class
  expect_equal(
    class(output_17365),
    "numeric",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_17365,
    type = "double")
  # Testing values
  expect_equal(
    output_17365,
    c(1.11101, 1, 4.24491, 1.53154, 2.47222, 3.08362, 1.99943, 5, 2.39621,
      2.15657),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_17365),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_17365),
    10L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_17365)),
    10L)

  # Testing min_max_scale(x = vec, new_min = NA, new_max...
  # Changed from baseline: new_min = NA
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_11346 <- xpectr::capture_side_effects(min_max_scale(x = vec, new_min = NA, new_max = 1, old_min = NULL, old_max = NULL, na.rm = TRUE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_11346[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'new_min': May not be NA."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_11346[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing min_max_scale(x = vec, new_min = NULL, new_m...
  # Changed from baseline: new_min = NULL
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_16569 <- xpectr::capture_side_effects(min_max_scale(x = vec, new_min = NULL, new_max = 1, old_min = NULL, old_max = NULL, na.rm = TRUE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_16569[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'new_min': Must be of type 'number', not 'NULL'."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_16569[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing min_max_scale(x = vec, new_min = 0, new_max ...
  # Changed from baseline: new_max = 6
  xpectr::set_test_seed(42)
  # Assigning output
  output_17050 <- min_max_scale(x = vec, new_min = 0, new_max = 6, old_min = NULL, old_max = NULL, na.rm = TRUE)
  # Testing class
  expect_equal(
    class(output_17050),
    "numeric",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_17050,
    type = "double")
  # Testing values
  expect_equal(
    output_17050,
    c(5.83348, 6, 1.13264, 5.20269, 3.79168, 2.87456, 4.50086, 0, 3.90568,
      4.26514),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_17050),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_17050),
    10L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_17050)),
    10L)

  # Testing min_max_scale(x = vec, new_min = 0, new_max ...
  # Changed from baseline: new_max = NA
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_14577 <- xpectr::capture_side_effects(min_max_scale(x = vec, new_min = 0, new_max = NA, old_min = NULL, old_max = NULL, na.rm = TRUE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_14577[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'new_max': May not be NA."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_14577[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing min_max_scale(x = vec, new_min = 0, new_max ...
  # Changed from baseline: new_max = NULL
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_17191 <- xpectr::capture_side_effects(min_max_scale(x = vec, new_min = 0, new_max = NULL, old_min = NULL, old_max = NULL, na.rm = TRUE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_17191[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'new_max': Must be of type 'number', not 'NULL'."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_17191[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing min_max_scale(x = vec, new_min = 0, new_max ...
  # Changed from baseline: old_min = -5
  xpectr::set_test_seed(42)
  # Assigning output
  output_19346 <- min_max_scale(x = vec, new_min = 0, new_max = 1, old_min = -5, old_max = NULL, na.rm = TRUE)
  # Testing class
  expect_equal(
    class(output_19346),
    "numeric",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_19346,
    type = "double")
  # Testing values
  expect_equal(
    output_19346,
    c(0.97938, 1, 0.39724, 0.90126, 0.72653, 0.61296, 0.81435, 0.25698,
      0.74065, 0.78516),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_19346),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_19346),
    10L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_19346)),
    10L)

  # Testing min_max_scale(x = vec, new_min = 0, new_max ...
  # Changed from baseline: old_min = NA
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_12554 <- xpectr::capture_side_effects(min_max_scale(x = vec, new_min = 0, new_max = 1, old_min = NA, old_max = NULL, na.rm = TRUE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_12554[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'old_min': May not be NA."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_12554[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing min_max_scale(x = vec, new_min = 0, new_max ...
  # Changed from baseline: old_max = 6
  xpectr::set_test_seed(42)
  # Assigning output
  output_14622 <- min_max_scale(x = vec, new_min = 0, new_max = 1, old_min = NULL, old_max = 6, na.rm = TRUE)
  # Testing class
  expect_equal(
    class(output_14622),
    "numeric",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_14622,
    type = "double")
  # Testing values
  expect_equal(
    output_14622,
    c(0.60293, 0.62015, 0.11707, 0.53774, 0.3919, 0.29711, 0.4652, 0,
      0.40368, 0.44083),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_14622),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_14622),
    10L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_14622)),
    10L)

  # Testing min_max_scale(x = vec, new_min = 0, new_max ...
  # Changed from baseline: old_max = NA
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_19400 <- xpectr::capture_side_effects(min_max_scale(x = vec, new_min = 0, new_max = 1, old_min = NULL, old_max = NA, na.rm = TRUE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19400[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'old_max': May not be NA."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19400[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing min_max_scale(x = vec, new_min = 0, new_max ...
  # Changed from baseline: na.rm = FALSE
  xpectr::set_test_seed(42)
  # Assigning output
  output_19782 <- min_max_scale(x = vec, new_min = 0, new_max = 1, old_min = NULL, old_max = NULL, na.rm = FALSE)
  # Testing class
  expect_equal(
    class(output_19782),
    "numeric",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_19782,
    type = "double")
  # Testing values
  expect_equal(
    output_19782,
    c(0.97225, 1, 0.18877, 0.86712, 0.63195, 0.47909, 0.75014, 0, 0.65095,
      0.71086),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_19782),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_19782),
    10L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_19782)),
    10L)

  # Testing min_max_scale(x = vec, new_min = 0, new_max ...
  # Changed from baseline: na.rm = NULL
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_11174 <- xpectr::capture_side_effects(min_max_scale(x = vec, new_min = 0, new_max = 1, old_min = NULL, old_max = NULL, na.rm = NULL), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_11174[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'na.rm': Must be of type 'logical flag', not 'NULL'."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_11174[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  ## Finished testing 'min_max_scale'                                         ####
  #


})

test_that("testing to_unit_length()", {
  xpectr::set_test_seed(42)

  # Examples

  # Create a data frame
  df <- data.frame(
    "x" = runif(20),
    "y" = runif(20),
    "g" = rep(1:4, each = 5)
  )

  # Scale row-wise

  # Test all have length 1 row-wise
  expect_true(
  to_unit_length(df, cols = c("x", "y"), by_row = TRUE) %>%
    vector_length(cols = c("x_row_unit", "y_row_unit"), by_row = TRUE) %>%
    dplyr::mutate(.vec_len = round(.data$.vec_len, digits = 10),
                  is_one = .data$.vec_len == 1) %>%
    dplyr::pull(.data$is_one) %>%
    all()
  )

  ## Testing 'to_unit_length(df, cols = c("x", "y"), by_ro...'              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Assigning output
  output_13795 <- to_unit_length(df, cols = c("x", "y"), by_row = TRUE)
  # Testing class
  expect_equal(
    class(output_13795),
    c("tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_13795[["x"]],
    c(0.91481, 0.93708, 0.28614, 0.83045, 0.64175, 0.5191, 0.73659,
      0.13467, 0.65699, 0.70506, 0.45774, 0.71911, 0.93467, 0.25543,
      0.46229, 0.94001, 0.97823, 0.11749, 0.475, 0.56033),
    tolerance = 1e-4)
  expect_equal(
    output_13795[["y"]],
    c(0.90403, 0.13871, 0.98889, 0.94667, 0.08244, 0.51421, 0.3902,
      0.90574, 0.44697, 0.836, 0.7376, 0.81106, 0.38811, 0.68517,
      0.00395, 0.83292, 0.00733, 0.20766, 0.9066, 0.61178),
    tolerance = 1e-4)
  expect_equal(
    output_13795[["g"]],
    c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4),
    tolerance = 1e-4)
  expect_equal(
    output_13795[["x_row_unit"]],
    c(0.71128, 0.98922, 0.27795, 0.65945, 0.99185, 0.71044, 0.88367,
      0.14706, 0.8268, 0.6447, 0.5273, 0.66342, 0.92355, 0.34931,
      0.99996, 0.74846, 0.99997, 0.49242, 0.46409, 0.67542),
    tolerance = 1e-4)
  expect_equal(
    output_13795[["y_row_unit"]],
    c(0.70291, 0.14643, 0.9606, 0.75174, 0.12741, 0.70376, 0.46812,
      0.98913, 0.56249, 0.76443, 0.84968, 0.74825, 0.38349, 0.93701,
      0.00854, 0.66318, 0.0075, 0.87036, 0.88579, 0.73743),
    tolerance = 1e-4)
  # Testing column names
  expect_equal(
    names(output_13795),
    c("x", "y", "g", "x_row_unit", "y_row_unit"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_13795),
    c("numeric", "numeric", "integer", "numeric", "numeric"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_13795),
    c("double", "double", "integer", "double", "double"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_13795),
    c(20L, 5L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_13795)),
    character(0),
    fixed = TRUE)
  ## Finished testing 'to_unit_length(df, cols = c("x", "y"), by_ro...'     ####

  # Scale column-wise

  # Test all have length 1 column-wise
  expect_true(
    all(to_unit_length(df, cols = c("x", "y"), by_row = FALSE) %>%
      vector_length(cols = c("x_col_unit", "y_col_unit"), by_row = FALSE) %>%
      unlist(recursive = TRUE, use.names = FALSE) %>%
      round(digits = 10) == 1)
  )

  ## Testing 'to_unit_length(df, cols = c("x", "y"), by_ro...'              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Assigning output
  output_19148 <- to_unit_length(df, cols = c("x", "y"), by_row = FALSE)
  # Testing class
  expect_equal(
    class(output_19148),
    c("tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_19148[["x"]],
    c(0.91481, 0.93708, 0.28614, 0.83045, 0.64175, 0.5191, 0.73659,
      0.13467, 0.65699, 0.70506, 0.45774, 0.71911, 0.93467, 0.25543,
      0.46229, 0.94001, 0.97823, 0.11749, 0.475, 0.56033),
    tolerance = 1e-4)
  expect_equal(
    output_19148[["y"]],
    c(0.90403, 0.13871, 0.98889, 0.94667, 0.08244, 0.51421, 0.3902,
      0.90574, 0.44697, 0.836, 0.7376, 0.81106, 0.38811, 0.68517,
      0.00395, 0.83292, 0.00733, 0.20766, 0.9066, 0.61178),
    tolerance = 1e-4)
  expect_equal(
    output_19148[["g"]],
    c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4),
    tolerance = 1e-4)
  expect_equal(
    output_19148[["x_col_unit"]],
    c(0.30612, 0.31357, 0.09575, 0.27789, 0.21475, 0.1737, 0.24648,
      0.04506, 0.21985, 0.23594, 0.15317, 0.24064, 0.31277, 0.08547,
      0.1547, 0.31456, 0.32734, 0.03931, 0.15895, 0.1875),
    tolerance = 1e-4)
  expect_equal(
    output_19148[["y_col_unit"]],
    c(0.30813, 0.04728, 0.33705, 0.32266, 0.0281, 0.17526, 0.13299,
      0.30871, 0.15234, 0.28494, 0.2514, 0.27644, 0.13228, 0.23353,
      0.00135, 0.28389, 0.0025, 0.07078, 0.309, 0.20852),
    tolerance = 1e-4)
  # Testing column names
  expect_equal(
    names(output_19148),
    c("x", "y", "g", "x_col_unit", "y_col_unit"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_19148),
    c("numeric", "numeric", "integer", "numeric", "numeric"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_19148),
    c("double", "double", "integer", "double", "double"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_19148),
    c(20L, 5L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_19148)),
    character(0),
    fixed = TRUE)
  ## Finished testing 'to_unit_length(df, cols = c("x", "y"), by_ro...'     ####


  ## Testing 'to_unit_length(df, cols = c("x", "y"), suffi...'              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Assigning output
  output_19148 <- to_unit_length(df, cols = c("x", "y"), suffix = "", overwrite = TRUE)
  # Testing class
  expect_equal(
    class(output_19148),
    c("tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_19148[["g"]],
    c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4),
    tolerance = 1e-4)
  expect_equal(
    output_19148[["x"]],
    c(0.71128, 0.98922, 0.27795, 0.65945, 0.99185, 0.71044, 0.88367,
      0.14706, 0.8268, 0.6447, 0.5273, 0.66342, 0.92355, 0.34931,
      0.99996, 0.74846, 0.99997, 0.49242, 0.46409, 0.67542),
    tolerance = 1e-4)
  expect_equal(
    output_19148[["y"]],
    c(0.70291, 0.14643, 0.9606, 0.75174, 0.12741, 0.70376, 0.46812,
      0.98913, 0.56249, 0.76443, 0.84968, 0.74825, 0.38349, 0.93701,
      0.00854, 0.66318, 0.0075, 0.87036, 0.88579, 0.73743),
    tolerance = 1e-4)
  # Testing column names
  expect_equal(
    names(output_19148),
    c("g", "x", "y"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_19148),
    c("integer", "numeric", "numeric"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_19148),
    c("integer", "double", "double"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_19148),
    c(20L, 3L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_19148)),
    character(0),
    fixed = TRUE)
  ## Finished testing 'to_unit_length(df, cols = c("x", "y"), suffi...'     ####



  ## Testing 'to_unit_length(df, cols = c("x", "y"), suffi...'              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_19148 <- xpectr::capture_side_effects(to_unit_length(df, cols = c("x", "y"), suffix = "", overwrite = FALSE), reset_seed = TRUE)
  expect_match(
    xpectr::strip(side_effects_19148[['error']]),
    xpectr::strip("Adding these dimensions would overwrite existing columns: x, y."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['error_class']]),
    xpectr::strip(c(purrr_error, "error", "condition")),
    fixed = TRUE)
  ## Finished testing 'to_unit_length(df, cols = c("x", "y"), suffi...'     ####


  # By groups in 'g'

  ## Testing 'df %>% dplyr::group_by(g) %>% to_unit_length...'              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Assigning output
  output_19148 <- df %>%
      dplyr::group_by(g) %>%
      to_unit_length(cols = c("x", "y"), by_row = FALSE)
  # Testing class
  expect_equal(
    class(output_19148),
    c("tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_19148[["x"]],
    c(0.91481, 0.93708, 0.28614, 0.83045, 0.64175, 0.5191, 0.73659,
      0.13467, 0.65699, 0.70506, 0.45774, 0.71911, 0.93467, 0.25543,
      0.46229, 0.94001, 0.97823, 0.11749, 0.475, 0.56033),
    tolerance = 1e-4)
  expect_equal(
    output_19148[["y"]],
    c(0.90403, 0.13871, 0.98889, 0.94667, 0.08244, 0.51421, 0.3902,
      0.90574, 0.44697, 0.836, 0.7376, 0.81106, 0.38811, 0.68517,
      0.00395, 0.83292, 0.00733, 0.20766, 0.9066, 0.61178),
    tolerance = 1e-4)
  expect_equal(
    output_19148[["g"]],
    c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4),
    tolerance = 1e-4)
  expect_equal(
    output_19148[["x_col_unit"]],
    c(0.53735, 0.55043, 0.16808, 0.4878, 0.37695, 0.3914, 0.5554, 0.10154,
      0.49538, 0.53163, 0.33391, 0.52458, 0.68182, 0.18633, 0.33723,
      0.60754, 0.63224, 0.07593, 0.307, 0.36215),
    tolerance = 1e-4)
  expect_equal(
    output_19148[["y_col_unit"]],
    c(0.54841, 0.08415, 0.59989, 0.57428, 0.05001, 0.35186, 0.267, 0.61977,
      0.30585, 0.57205, 0.54645, 0.60087, 0.28753, 0.50761, 0.00293,
      0.59906, 0.00527, 0.14936, 0.65206, 0.44001),
    tolerance = 1e-4)
  # Testing column names
  expect_equal(
    names(output_19148),
    c("x", "y", "g", "x_col_unit", "y_col_unit"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_19148),
    c("numeric", "numeric", "integer", "numeric", "numeric"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_19148),
    c("double", "double", "integer", "double", "double"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_19148),
    c(20L, 5L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_19148)),
    character(0),
    fixed = TRUE)
  ## Finished testing 'df %>% dplyr::group_by(g) %>% to_unit_length...'     ####

  # to_unit_length_vec


  ## Testing 'to_unit_length_vec(c(1:10))'                                  ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Assigning output
  output_19148 <- to_unit_length_vec(c(1:10))
  # Testing class
  expect_equal(
    class(output_19148),
    "numeric",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_19148,
    type = "double")
  # Testing values
  expect_equal(
    output_19148,
    c(0.05096, 0.10193, 0.15289, 0.20386, 0.25482, 0.30579, 0.35675,
      0.40772, 0.45868, 0.50965),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_19148),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_19148),
    10L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_19148)),
    10L)
  ## Finished testing 'to_unit_length_vec(c(1:10))'                         ####


  ## Testing 'to_unit_length(c(1:10), suffix = "")'                         ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Assigning output
  output_19148 <- to_unit_length(c(1:10), suffix = "", overwrite = TRUE)
  # Testing class
  expect_equal(
    class(output_19148),
    "numeric",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_19148,
    type = "double")
  # Testing values
  expect_equal(
    output_19148,
    c(0.05096, 0.10193, 0.15289, 0.20386, 0.25482, 0.30579, 0.35675,
      0.40772, 0.45868, 0.50965),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_19148),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_19148),
    10L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_19148)),
    10L)
  ## Finished testing 'to_unit_length(c(1:10), suffix = "")'                ####


  ## Testing 'vector_length(to_unit_length(c(1:10), suffix...'              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Assigning output
  output_19148 <- vector_length(to_unit_length(c(1:10), suffix = ""))
  # Testing class
  expect_equal(
    class(output_19148),
    "numeric",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_19148,
    type = "double")
  # Testing values
  expect_equal(
    output_19148,
    1,
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_19148),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_19148),
    1L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_19148)),
    1L)
  ## Finished testing 'vector_length(to_unit_length(c(1:10), suffix...'     ####



})

test_that("to_unit_vector_()", {
  # Set seed
  xpectr::set_test_seed(42)

  vec1 <- runif(10)
  vecNormed <- to_unit_vector_(vec1)


  ## Testing 'vecNormed'                                                    ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing class
  expect_equal(
    class(vecNormed),
    "numeric",
    fixed = TRUE)
  # Testing type
  expect_type(
    vecNormed,
    type = "double")
  # Testing values
  expect_equal(
    vecNormed,
    c(0.4239, 0.43422, 0.13259, 0.38481, 0.29737, 0.24054, 0.34132,
      0.0624, 0.30444, 0.32671),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(vecNormed),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(vecNormed),
    10L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(vecNormed)),
    10L)
  ## Finished testing 'vecNormed'                                           ####


  ## Testing 'sqrt(sum(vecNormed^2))'                                       ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Assigning output
  output_19148 <- sqrt(sum(vecNormed^2))
  # Testing class
  expect_equal(
    class(output_19148),
    "numeric",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_19148,
    type = "double")
  # Testing values
  expect_equal(
    output_19148,
    1,
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_19148),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_19148),
    1L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_19148)),
    1L)
  ## Finished testing 'sqrt(sum(vecNormed^2))'                              ####


  # Generate expectations for 'to_unit_vector_'
  # Tip: comment out the gxs_function() call
  # so it is easy to regenerate the tests
  xpectr::set_test_seed(42)
  # xpectr::gxs_function(
  #   fn = to_unit_vector_,
  #   args_values = list(
  #     "x" = list(c(1,1,1), c(0,0,0), 1, 0, "hej", NA)
  #   ),
  #   indentation = 2,
  #   copy_env = FALSE
  # )


  ## Testing 'to_unit_vector_'                                                 ####
  ## Initially generated by xpectr
  # Testing different combinations of argument values

  # Testing to_unit_vector_(x = c(1, 1, 1))
  xpectr::set_test_seed(42)
  # Assigning output
  output_19148 <- to_unit_vector_(x = c(1, 1, 1))
  # Testing class
  expect_equal(
    class(output_19148),
    "numeric",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_19148,
    type = "double")
  # Testing values
  expect_equal(
    output_19148,
    c(0.57735, 0.57735, 0.57735),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_19148),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_19148),
    3L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_19148)),
    3L)

  # Testing to_unit_vector_(x = c(0, 0, 0))
  # Changed from baseline: x = c(0, 0, 0)
  xpectr::set_test_seed(42)
  # Assigning output
  output_19370 <- to_unit_vector_(x = c(0, 0, 0))
  # Testing class
  expect_equal(
    class(output_19370),
    "numeric",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_19370,
    type = "double")
  # Testing values
  expect_equal(
    output_19370,
    c(0, 0, 0),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_19370),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_19370),
    3L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_19370)),
    3L)

  # Testing to_unit_vector_(x = 1)
  # Changed from baseline: x = 1
  xpectr::set_test_seed(42)
  # Assigning output
  output_12861 <- to_unit_vector_(x = 1)
  # Testing class
  expect_equal(
    class(output_12861),
    "numeric",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_12861,
    type = "double")
  # Testing values
  expect_equal(
    output_12861,
    1,
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_12861),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_12861),
    1L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_12861)),
    1L)

  # Testing to_unit_vector_(x = 0)
  # Changed from baseline: x = 0
  xpectr::set_test_seed(42)
  # Assigning output
  output_18304 <- to_unit_vector_(x = 0)
  # Testing class
  expect_equal(
    class(output_18304),
    "numeric",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_18304,
    type = "double")
  # Testing values
  expect_equal(
    output_18304,
    0,
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_18304),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_18304),
    1L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_18304)),
    1L)

  # Testing to_unit_vector_(x = "hej")
  # Changed from baseline: x = "hej"
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_16417 <- xpectr::capture_side_effects(to_unit_vector_(x = "hej"), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_16417[['error']]),
    xpectr::strip("Assertion on 'x' failed: Must be of type 'numeric', not 'character'."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_16417[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing to_unit_vector_(x = NA)
  # Changed from baseline: x = NA
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_15190 <- xpectr::capture_side_effects(to_unit_vector_(x = NA), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_15190[['error']]),
    xpectr::strip("Assertion on 'x' failed: Contains missing values (element 1)."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_15190[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing to_unit_vector_(x = NULL)
  # Changed from baseline: x = NULL
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_17365 <- xpectr::capture_side_effects(to_unit_vector_(x = NULL), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_17365[['error']]),
    xpectr::strip("Assertion on 'x' failed: Must be of type 'numeric', not 'NULL'."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_17365[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  ## Finished testing 'to_unit_vector_'                                        ####
  #

})


