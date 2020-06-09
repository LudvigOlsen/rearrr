
# rearrr 0.0.0.9000

## New rearrangers

* Adds `center_max()` for centering the highest value with values decreasing around it. (Rearranger)

* Adds `center_min()` for centering the lowest value with values increasing around it. (Rearranger)

* Adds `position_max()` for positioning the highest value with values decreasing around it. (Rearranger)

* Adds `position_min()` for positioning the lowest value with values increasing around it. (Rearranger)

* Adds `pair_extremes()` for arranging values as highest, lowest, second highest, second lowest, etc. (Rearranger)

* Adds `rev_windows()` for reversing order window-wise. (Rearranger)

* Adds `closest_to()` and `furthest_from()` for arranging values by how far they are from a target value / index. (Rearranger)

## New mutators

* Adds `cluster_groups()` for moving data points into clusters around their group centroid. (Mutator)

* Adds `expand_distances()` and `expand_distances_each()` for expanding values around an origin in any number of dimensions. (Mutator)

* Adds `dim_values()` for dimming values in one dimension by their distance to around an n-dimensional origin. (Mutator)

* Adds `rotate_2d()` and `rotate_3d()` for rotating values around an origin. (Mutator)

* Adds `swirl_2d()` and `swirl_3d()` for rotating values around an origin. (Mutator)

* Adds `flip_values()` for flipping values around a center value. (Mutator)

## New helpers

* Adds `most_centered()` for finding the coordinates of the data point closest to the centroid. (Helper)

* Adds `centroid()` for finding the centroid from multiple vectors (dimensions). (Helper)

* Adds `transfer_centroids()` for transfering centroids from one `data.frame` to another. (Helper)

* Adds `min_max_scale()` for scaling values to a specified range. (Helper)

* Adds `create_origin_fn()` for creating a function for finding origin coordinates (like `centroid()`). (Helper)

* Adds `generate_clusters()` for generating n-dimensional clusters. (Helper)

## Welcome message (important!)

* Please welcome `rearrr`! :-)
