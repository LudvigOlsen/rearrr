

# https://en.wikipedia.org/wiki/Rotation_matrix
# x = roll(γ) ; y = pitch(β) ; z = yaw(α)
create_rotation_matrix3d <- function(x_deg = 0,
                                     y_deg = 0,
                                     z_deg = 0) {
  # Convert to radians
  x_radians <- degrees_to_radians(x_deg)
  y_radians <- degrees_to_radians(y_deg)
  z_radians <- degrees_to_radians(z_deg)

  # Prepare cos and sin versions
  cos_x <- cos(x_radians)
  sin_x <- sin(x_radians)
  cos_y <- cos(y_radians)
  sin_y <- sin(y_radians)
  cos_z <- cos(z_radians)
  sin_z <- sin(z_radians)

  matrix(c(
    c(cos_z * cos_y, sin_z * cos_y, -sin_y),
    c(
      cos_z * sin_y * sin_x - sin_z * cos_x,
      sin_z * sin_y * sin_x + cos_z * cos_x,
      cos_y * sin_x
    ),
    c(
      cos_z * sin_y * cos_x + sin_z * sin_x,
      sin_z * sin_y * cos_x - cos_z * sin_x,
      cos_y * cos_x
    )
  ),
  nrow = 3,
  ncol = 3)
}

create_rotation_matrix2d <- function(deg = 0) {
  if (deg %in% c(-360, 0, 360)) {
    rotation_matrix <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2)
  } else if (deg %in% c(90,-270)) {
    rotation_matrix <- matrix(c(0, 1, -1, 0), nrow = 2, ncol = 2)
  } else if (deg %in% c(180,-180)) {
    rotation_matrix <- matrix(c(-1, 0, 0, -1), nrow = 2, ncol = 2)
  } else if (deg %in% c(270,-90)) {
    rotation_matrix <- matrix(c(0, -1, 1, 0), nrow = 2, ncol = 2)
  } else {
    radian <- degrees_to_radians(deg)
    cos_rad <- cos(radian)
    sin_rad <- sin(radian)
    rotation_matrix <- matrix(c(cos_rad, sin_rad, -sin_rad, cos_rad),
                              nrow = 2,
                              ncol = 2)
  }

  rotation_matrix

}
