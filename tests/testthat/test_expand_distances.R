library(rearrr)
context("expand_distances()")

test_that("expand_distances()", {
  # Set seed
  xpectr::set_test_seed(42)

  df <- data.frame(
    "x" = runif(20),
    "y" = runif(20),
    "g" = rep(1:4, each=5)
  )

  # TODO!!!

  # Generate expectations for 'expand_distances'
  # Tip: comment out the gxs_function() call
  # so it is easy to regenerate the tests
  xpectr::set_test_seed(42)
  # xpectr::gxs_function(
  #   fn = expand_distances,
  #   args_values = list(
  #     "data" = list(),
  #     "cols" = list(NULL),
  #     "multiplier" = list(1),
  #     "multiplier_fn" = list(NULL),
  #     "origin" = list(0),
  #     "origin_fn" = list(NULL),
  #     "exponentiate" = list(FALSE),
  #     "add_one_exp" = list(TRUE),
  #     "suffix" = list("_expanded"),
  #     "keep_original" = list(TRUE),
  #     "mult_col_name" = list(ifelse(isTRUE(exponentiate), ".exponent", ".multiplier")),
  #     "origin_col_name" = list(".origin")
  #   ),
  #   indentation = 2,
  #   copy_env = FALSE
  # )

  #


})
