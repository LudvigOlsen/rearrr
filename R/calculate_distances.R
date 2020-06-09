

calculate_distances <- function(dim_vectors, to){

  # Distance formula:
  # d(P1, P2) = sqrt( (x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2 )

  # Calculate (x2-x1)^2, (y2-y1)^2, etc.
  distance_terms <-
    purrr::map2(.x = dim_vectors, .y = to, .f = ~ {
      (.x - .y)^2
    })

  # Calculate sqrt(sum(x,y,z))
  # of the squared differences dim-wise
  distances <- distance_terms %>%
    purrr::transpose() %>%
    purrr::simplify_all() %>%
    purrr::map_dbl(~{sqrt(sum(.x))})

  distances

}
