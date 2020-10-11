

create_shearing_matrix_3d_ <- function(x_shear,
                                       y_shear,
                                       z_shear) {
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  if (length(c(x_shear, y_shear, z_shear)) != 2){
    assert_collection$push(
      "Exactly 2 of {x_shear, y_shear, z_shear} must be non-null."
    )
  }
  checkmate::reportAssertions(assert_collection)

  if (is.null(x_shear)){
    return(matrix(
      c(1, y_shear, z_shear,
        0, 1, 0,
        0, 0, 1
      ),
      nrow = 3, ncol = 3
    ))
  } else if (is.null(y_shear)){
    return(matrix(
      c(1, 0, 0,
        x_shear, 1, z_shear,
        0, 0, 1
      ),
      nrow = 3, ncol = 3
    ))
  } else if (is.null(z_shear)){
    return(matrix(
      c(1, 0, 0,
        0, 1, 0,
        x_shear, y_shear, 1
      ),
      nrow = 3, ncol = 3
    ))
  }

}

create_shearing_matrix_2d_ <- function(x_shear, y_shear) {
  matrix(c(1, y_shear,
           x_shear, 1),
         nrow = 2)
}
