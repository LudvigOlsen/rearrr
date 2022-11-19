
# rearrr 0.3.2

* Makes use of suggested packages conditional on their presence.

* Makes testing conditional on the availability of `xpectr`.

* Makes tests compatible with the upcoming release of `purrr v1`. (#1)

* Fixes `tidyselect`-related warnings.

# rearrr 0.3.1

* Regenerates documentation to fix html5 problem.

* Imports R6 to resolve note in `R CMD check`.

# rearrr 0.3.0

* Breaking: Fixes bug in `pair_extremes()` where the `shuffle_pairs` and `shuffle_members` were kind of switched. Now `shuffle_pairs` changes the order of pairs while `shuffle_members` only shuffles the values of `col`.

* Adds pipeline classes for piping transformations: `Pipeline`, `GeneratedPipeline`, `FixedGroupsPipeline`.

* Adds transformation classes: `Transformation`, `GeneratedTransformation`, `FixedGroupsTransformation`. Used by their respective pipeline class.

* Adds `triplet_extremes()` for arranging values in triplets with (highest, most middle, lowest), (second highest, second most middle, second lowest), etc. (Rearranger)

* Adds `order_by_aggregates` argument to `pair_extremes()`. When using recursive pairing, this allows ordering the first `num_pairings`-1 pair identifier columns by the aggregate values instead of the identifiers.

* Prepares package's tests for `checkmate 2.1.0`.

# rearrr 0.2.0

* Adds `shuffle_hierarchy()` for shuffling a multi-column hierarchy of groups. (Rearranger)

* Adds optional recursion to `pair_extremes()`. This adds the `num_pairings` and `balance` arguments. Now returns one additional sorting factor per `num_pairing` level.

# rearrr 0.1.0

Note: Multiple of the new functions also have `*_vec()` versions that take and return vectors. The same can *usually* be achieved with the regular versions, but these wrappers make it easier and less verbose.

## New rearrangers

* Adds `center_max()` for centering the highest value with values decreasing around it. (Rearranger)

* Adds `center_min()` for centering the lowest value with values increasing around it. (Rearranger)

* Adds `position_max()` for positioning the highest value with values decreasing around it. (Rearranger)

* Adds `position_min()` for positioning the lowest value with values increasing around it. (Rearranger)

* Adds `pair_extremes()` for arranging values as highest, lowest, second highest, second lowest, etc. (Rearranger)

* Adds `rev_windows()` for reversing order window-wise. (Rearranger)

* Adds `closest_to()` and `furthest_from()` for arranging values by how far they are from an origin (e.g. a target value / index in 1d). (Rearranger)

* Adds `roll_elements()` for rolling/shifting the position of the elements. (Rearranger)


## New mutators

* Adds `cluster_groups()` for moving data points into clusters around their group centroid. (Mutator)

* Adds `expand_distances()` and `expand_distances_each()` for expanding values around an origin in any number of dimensions. (Mutator)

* Adds `dim_values()` for dimming values in one dimension by their distance to around an n-dimensional origin. (Mutator)

* Adds `rotate_2d()` and `rotate_3d()` for rotating values around an origin. (Mutator)

* Adds `swirl_2d()` and `swirl_3d()` for rotating values around an origin. (Mutator)

* Adds `shear_2d()` and `shear_3d()` for shearing values around an origin. (Mutator)

* Adds `flip_values()` for flipping values around a center value. (Mutator)

* Adds `roll_values()` and `wrap_to_range()` for rolling/shifting values and wrapping to a range. (Mutator)

* Adds `transfer_centroids()` for transferring centroids from one `data.frame` to another. (Mutator)

* Adds `apply_transformation_matrix()` for performing matrix multiplication with a given transformation matrix
and a set of `data.frame` columns. Allows moving the origin before and after the transformation.

## Formers

* Adds `circularize()` for creating x-coordinates to a set of y-coordinates so they form a circle. (Former)

* Adds `hexagonalize()` for creating x-coordinates to a set of y-coordinates so they form a hexagon. (Former)

* Adds `square()` for creating x-coordinates to a set of y-coordinates so they form a square. (Former)

* Adds `triangularize()` for creating x-coordinates to a set of y-coordinates so they form a triangle. (Former)


## New generators

* Adds `generate_clusters()` for generating n-dimensional clusters. (Generator)


## New measuring functions

* Adds `distance()` for calculating distances to a specified origin. (Measuring function)

* Adds `angle()` for calculating the angle between points and an origin. (Measuring function)

* Adds `vector_length()` for calculating vector length/magnitude row-wise or column-wise. (Measuring function)


## New scalers

* Adds `min_max_scale()` for scaling values to a specified range. (Scaler)

* Adds `to_unit_length()` for scaling vectors to unit length row-wise or column-wise. (Scaler)


## New converters

* Adds `radians_to_degrees()` and `degrees_to_radians()`. (Converters)


## New helpers

* Adds `most_centered()` for finding the coordinates of the data point closest to the centroid. (Helper)

* Adds `centroid()` for finding the centroid from multiple vectors/columns (dimensions). (Helper)

* Adds `midrange()` for finding the midrange of vectors/columns. (Helper)

* Adds `create_origin_fn()` for creating a function for finding origin coordinates (like `centroid()`). (Helper)

* Adds `create_n_fn()` for creating a function for finding the number of positions to move, e.g. in `roll_elements()`. (Helper)

* Adds `median_index()` for finding the median index of each supplied vector. (Helper)

* Adds `quantile_index()` for finding the *n*th quantile of the index of each supplied vector. (Helper)


## Welcome message (important!)

* Please welcome `rearrr`! :-)
