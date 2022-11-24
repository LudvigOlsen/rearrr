


#   __________________ #< 837022fb68986f4274072896ad1f8be6 ># __________________
#   Helpers                                                                 ####


##  .................. #< a309bc456a67722d30118ad015672baa ># ..................
##  Is correctly positioned                                                 ####


is_correctly_positioned_ <-
  function(data, col, position, what = "max") {
    target_val <-
      ifelse(what == "max", max(data[[col]]), min(data[[col]]))
    data[[col]][[position]] == target_val
  }


##  .................. #< 8e6f910bb09315ea08d9791e6a30c6a7 ># ..................
##  Swap values                                                             ####


swap_values_ <- function(vec, v1, v2) {
  v1_ind <- vec[vec == v1]
  v2_ind <- vec[vec == v2]
  vec[v1_ind] <- v2
  vec[v2_ind] <- v1
  vec
}


##  .................. #< f5bf1e543ca585628647e0f35453d42f ># ..................
##  Base helpers                                                            ####


base_rename_ <-
  function(data, before, after, warn_at_overwrite = FALSE) {
    #
    # Replaces name of column in data frame
    #

    # Check names
    if (!is.character(before) || !is.character(after)) {
      stop("'before' and 'after' must both be of type character.")
    }
    if (length(before) != 1 || length(before) != 1) {
      stop("'before' and 'after' must both have length 1.")
    }

    if (before == after) {
      message("'before' and 'after' were identical.")
      return(data)
    }
    # If after is already a column in data
    # remove it, so we don't have duplicate column names
    if (after %in% colnames(data)) {
      if (isTRUE(warn_at_overwrite)) {
        warning("'after' already existed in 'data' and will be replaced.")
      }
      data[[after]] <- NULL
    }
    colnames(data)[names(data) == before] <- after
    data
  }

# Cols should be col names
base_select_ <- function(data, cols) {
  tryCatch(
    subset(data, select = cols),
    error = function(e) {
      if (grepl("is missing", e)) {
        stop("base_select_() only work on data.frames.")
      } else {
        stop(paste0("base_select_() got error from subset(): ", e))
      }
    }
  )
}

# Cols should be col names
base_deselect_ <- function(data, cols) {
  if (!is.character(cols)) {
    stop("cols must be names")
  }
  base_select_(data = data, cols = setdiff(names(data), cols))
}


##  .................. #< ee7952f84c8fa1b183ab4a3b923b9903 ># ..................
##  Position first                                                          ####


# Col should be col name
position_first_ <- function(data, col) {
  if (is.numeric(col)) {
    stop("'col' must be a name.")
  }
  # if(is.data.table(data)){
  #   return(data[, c(col, setdiff(names(data), col)), with = FALSE])
  # }

  base_select_(data = data, cols = c(col, setdiff(names(data), col)))
}


##  .................. #< 4cb48e1b875b60049471d53ddceeb9dc ># ..................
##  Insert row                                                              ####


# insertRow2 from https://stackoverflow.com/a/11587051/11832955
# Note: May not work with rownames!
insert_row_ <- function(data, new_row, after) {
  data <- rbind(data, new_row)
  data <- data[order(c(seq_len(nrow(data) - 1), after + 0.5)), ,
    drop = FALSE
  ] # extra comma on purpose
  row.names(data) <- NULL
  data
}


##  .................. #< 551180c531b6373a09823e43527a5c9d ># ..................
##  Create tmp column name                                                  ####


# Add underscore until var name is unique
# arg disallowed can add extra things not to be named as
create_tmp_var <- function(data,
                           tmp_var = ".tmp_index_",
                           disallowed = NULL) {
  # Extract the disallowed names
  disallowed <- c(colnames(data), disallowed)

  while (tmp_var %in% disallowed) {
    tmp_var <- paste0(tmp_var, "_")
  }
  tmp_var
}


##  .................. #< 3abfddeb36aa8dd83aab2d14c948a5fc ># ..................
##  Create tmp unique value                                                 ####


# Add underscore until value is unique in the vector
# arg disallowed can add extra things not to be named as
create_tmp_val <- function(v,
                           tmp_val = ".tmp_val_",
                           disallowed = NULL) {
  # Extract the disallowed names
  disallowed <- c(unique(v), disallowed)

  while (tmp_val %in% disallowed) {
    tmp_val <- paste0(tmp_val, "_")
  }
  tmp_val
}


##  .................. #< e0e05520eb560f3113652056facb7c8f ># ..................
##  Is between                                                              ####


is_between_ <- function(x, a, b) {
  # Checks if x is between a and b
  x > a & x < b
}


##  .................. #< db2cfc96e473219a35e8193b8357e682 ># ..................
##  Not in                                                                  ####


`%ni%` <- function(x, table) {
  !(x %in% table)
}


##  .................. #< 56db9e9358659a166e5bb2c662733e42 ># ..................
##  Is zero vector                                                          ####


is_zero_vector_ <- function(v){
  is.numeric(v) && all(v == 0)
}


##  .................. #< 76a9352ccecc01349336a01b9a870511 ># ..................
##  Greedy windows                                                          ####



greedy_windows_ <- function(data,
                            window_size,
                            factor_name = ".window") {
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(data,
    min.cols = 1,
    min.rows = 1,
    add = assert_collection
  )
  checkmate::assert_number(window_size, lower = 1, add = assert_collection)
  checkmate::assert_string(factor_name, min.chars = 1, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  size <- nrow(data)
  num_windows <- ceiling(size / window_size)
  windows <- rep(seq_len(num_windows), each = window_size)
  data[[factor_name]] <- factor(head(windows, size))
  data
}


##  .................. #< 160fb806ead6df367e8121d143d788b6 ># ..................
##  Windows n-distributed                                                   ####

ndist_windows_ <- function(data,
                           num_windows,
                           factor_name = ".window") {
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(data, add = assert_collection)
  checkmate::assert_number(num_windows, lower = 1, add = assert_collection)
  checkmate::assert_string(factor_name, min.chars = 1, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  if (num_windows > nrow(data)) {
    assert_collection$push("'num_windows' was greater than the number of rows in 'data'.")
  }
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  data[[factor_name]] <-
    n_dist_group_factor_(v_size = nrow(data), n_windows = num_windows)

  data
}


n_dist_group_factor_ <- function(v_size, n_windows) {
  # Simplified version of groupdata2 version

  #
  # Takes a vector and a number of windows to create
  # Distributes excess elements somewhat evenly across windows
  # Returns grouping factor
  #

  # Create grouping factor with distributed excess elements
  grouping_factor <- ceiling(seq_len(v_size) / (v_size / n_windows))

  # Sometimes a value of e.g. 7.0000.. is rounded up to 8
  # in the above ceiling(). This means that we get 8 groups
  # instead of the specified 7. In this case we replace
  # the extra "8" with 7.
  # --> This should be tested! <--

  # If there are too many groups
  if (max(grouping_factor) > n_windows) {
    # Get the largest number in grouping factor
    max_value <- max(grouping_factor)

    # Get the size of the last group
    last_group_size <-
      length(grouping_factor[grouping_factor == max_value])

    # If there is only one group too much and it only contains one element
    # put this element in the second last group instead
    if (max_value - 1 == n_windows && last_group_size == 1) {
      # Replace the level of the factor containing the max_value
      # with the value of the second last group instead (max_value - 1)
      grouping_factor[grouping_factor == max_value] <- max_value - 1

      # Else, stop the script as something has gone wrong
      # and I need to know about it!
    } else {
      stop(
        paste(
          "Grouping factor contains too many groups! ",
          max_value,
          " groups in total with ",
          last_group_size,
          " elements in last group.",
          sep = ""
        )
      )
    }
  }

  factor(grouping_factor)
}


##  .................. #< 60cfc78f594e5611a6eaaf34a2b212ae ># ..................
##  Range outliers                                                          ####

split_range_outliers_ <- function(data,
                                 col,
                                 .min = NULL,
                                 .max = NULL) {
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(data, add = assert_collection)
  checkmate::assert_string(col, min.chars = 1, add = assert_collection)
  checkmate::assert_number(.min, finite = TRUE, null.ok = TRUE, add = assert_collection)
  checkmate::assert_number(.max, finite = TRUE, null.ok = TRUE, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  if (col %ni% colnames(data)){
    assert_collection$push("'col' not found in 'data'.")
  }
  if (!is.null(.min) && !is.null(.max) && .min >= .max){
    assert_collection$push("'.min' must be smaller than .max or 'NULL'.")
  }
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####
  rownames(data) <- NULL

  # Create min/max masks (bools)
  if (!is.null(.min)){
    min_mask <- data[[col]] < .min
  } else {
    min_mask <- rep(0, nrow(data))
  }
  if (!is.null(.max)){
    max_mask <- data[[col]] > .max
  } else {
    max_mask <- rep(0, nrow(data))
  }

  mask <- (min_mask + max_mask) == 1
  outlier_inds <- seq_len(nrow(data))[mask]
  if (length(outlier_inds) == 0){
    outliers <- data[-seq_len(nrow(data)), ]
  } else {
    outliers <- data[outlier_inds, ]
    data <- data[-outlier_inds, ]
  }
  list("data" = data,
       "outliers" = outliers)
}

add_na_column_ <- function(data, col, val = NA_real_, as_list = FALSE, overwrite = FALSE){
  if (isTRUE(as_list))
    to_add <- rep(list(val), nrow(data))
  else
    to_add <- rep(val, nrow(data))

  # Add column
  add_info_col_(
    data = data,
    nm = col,
    content = to_add,
    overwrite = overwrite
  )
}


##  .................. #< 385b0b542a569ee6b885fc0849f644a6 ># ..................
##  Input helpers                                                           ####

check_unique_colnames_ <- function(...) {
  nms <- list(...) %>%
    purrr::compact() %>%
    unlist(recursive = TRUE, use.names = FALSE)

  if (is.null(nms))
    return(invisible(NULL))

  # Get names as string
  first_nms <- deparse(head(nms, 4))
  # Remove "list()"
  nms_str <- paste0(substring(first_nms, first = 3, nchar(first_nms)-1))

  # Add ... if more than 4 elements
  if (length(nms) > 4){
    nms_str <- paste0(nms_str, ", ...")
  }


  # Check that names are unique
  checkmate::assert_character(
    nms,
    unique = TRUE,
    any.missing = FALSE,
    .var.name = paste0(
      "specified column names (",
      nms_str, ")"
    )
  )
}


# check_overwrite(data = data, nm = "?", overwrite = overwrite)
check_overwrite_ <- function(data, nm, overwrite){
  checkmate::assert_flag(overwrite)
  if (!is.data.frame(data) || isTRUE(overwrite)){
    return(invisible(NULL))
  }

  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_string(nm, min.chars = 1, null.ok = TRUE, add = assert_collection)
  # Check if we would overwrite an existing column
  if (!is.null(nm) && nm %in% colnames(data)){
    assert_collection$push(
      paste0("The column '", nm,
             "' already exists and 'overwrite' is disabled.")
    )
  }
  checkmate::reportAssertions(assert_collection)
}


##  .................. #< 4405b38854cdc7cc63ac70477e1c9953 ># ..................
##  Output helpers                                                          ####


# When 1 coordinate but multiple names, it recycles the coordinate
list_coordinates_ <- function(coordinates, names) {
  checkmate::assert(
    checkmate::check_numeric(coordinates),
    checkmate::check_list(coordinates, types = "numeric")
  )
  checkmate::assert_character(names, any.missing = FALSE,
                              min.chars = 1, unique = TRUE)
  if (length(coordinates) == 1 && length(names) > 1) {
    coordinates <- rep(coordinates, length(names))
  }
  if (length(coordinates) > 1 &&
      length(coordinates) != length(names)) {
    stop(
      paste0(
        "When 'coordinates' has length >1, 'coordinates' and 'names'",
        " must have same length."
      )
    )
  }
  list(setNames(coordinates, names))
}

# Paste a list column where each element is c(x = 1, y = d)
paste_coordinates_column_ <- function(data, col, na.rm=FALSE) {
  str_name <- paste0(col, "_str")

  # Remove NAs
  if (isTRUE(na.rm)){
    data[[col]] <- purrr::map(.x = data[[col]], .f = ~{
      #purrr::map(.f = round(.x, digits = digits))
      .x[!is.na(.x)]
      })
  }

  # Paste coordinates
  data[[str_name]] <- paste0(data[[col]])

  # Whether to cut away "list(" or "c("
  start_char <- ifelse(is.list(data[[col]][[1]]), 6, 3)
  data[[str_name]] <- substr(data[[str_name]],
    start = start_char,
    stop = nchar(data[[str_name]]) - 1
  )

  # Remove whitespace
  data[[str_name]] <- gsub("[[:blank:]]+", "", data[[str_name]])

  data
}

paste_ranges_column_ <- function(data,
                                 col,
                                 include_min = TRUE,
                                 include_max = TRUE) {
  str_name <- paste0(col, "_str")
  outer_is_list <- is.list(data[[col]][[1]])
  data[[str_name]] <- data[[col]] %>%
    purrr::map(.f = ~ {
      purrr::map(.x, .f = unname)
    })
  data[[str_name]] <- paste0(data[[str_name]])
  data[[str_name]] <- substr(data[[str_name]],
    start = ifelse(outer_is_list, 6, 3),
    stop = nchar(data[[str_name]]) - 1
  )
  data[[str_name]] <- gsub("[[:blank:]]+", "", data[[str_name]])
  data[[str_name]] <- gsub("c", "", data[[str_name]])
  if (isTRUE(include_min)) {
    data[[str_name]] <- gsub("\\(", "[", data[[str_name]])
  }
  if (isTRUE(include_max)) {
    data[[str_name]] <- gsub("\\)", "]", data[[str_name]])
  }

  data
}

add_dimensions_ <- function(data,
                            new_vectors,
                            suffix = "",
                            overwrite = TRUE) {
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(data, add = assert_collection)
  checkmate::assert_list(new_vectors, names = "unique",
                         types = c("numeric", "character", "factor"),
                         add = assert_collection)
  checkmate::assert_string(suffix, add = assert_collection)
  checkmate::assert_flag(overwrite, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Add suffix
  names(new_vectors) <- paste0(names(new_vectors), suffix)

  # Convert to data frame
  new_data <- data.frame(new_vectors, stringsAsFactors = FALSE)

  if (!isTRUE(overwrite) &&
    length(intersect(colnames(new_data), colnames(data))) > 0) {
    stop(
      paste0(
        "Adding these dimensions would overwrite existing columns: ",
        paste0(intersect(colnames(new_data), colnames(data)), collapse = ", "),
        "."
      )
    )
  }

  # If overwriting columns, delete in 'data' first
  col_intersection <-
    intersect(colnames(new_data), colnames(data))
  if (length(col_intersection) > 0) {
    data <- data[, colnames(data) %ni% col_intersection, drop = FALSE]
  }

  # Add to original dataframe
  data <- dplyr::bind_cols(data, new_data)

  data
}

add_info_col_ <- function(data, nm, content, overwrite = FALSE, check_overwrite = TRUE){
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(data, add = assert_collection)
  checkmate::assert_string(nm, null.ok = TRUE, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # When name is NULL, we don't add the column
  if (is.null(nm)){
    return(data)
  }

  if (isTRUE(check_overwrite)){
    # Check if we will overwrite an existing column
    check_overwrite_(data = data, nm = nm, overwrite = overwrite)
  }

  # Add column
  data[[nm]] <- content

  data

}


##  .................. #< cb9942d54f8fa35406de031d066531dc ># ..................
##  Conversions                                                             ####

calculate_swirl_degrees_ <- function(distances, radius) {
  checkmate::assert_numeric(distances, lower = 0, any.missing = FALSE)
  checkmate::assert_number(radius)
  if (radius == 0) {
    return(distances * 0)
  }
  (distances / (2 * radius) * 360) %% 360
}

##  .................. #< 60cfc78f594e5611a6eaaf34a2b212ae ># ..................
##  Package imports                                                         ####


#' @importFrom dplyr %>%
#' @importFrom rlang .data :=
#' @importFrom utils head tail
#' @importFrom stats quantile runif rnorm median setNames
#' @importFrom lifecycle deprecated deprecate_stop
NULL

# R CMD check NOTE handling
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("."))
}

get_pkg_version <- function(pkg_name){
  vs <- unlist(utils::packageVersion(pkg_name))
  list("major" = vs[[1]],
       "minor" = vs[[2]],
       "patch" = vs[[3]],
       "dev" = ifelse(length(vs) > 3, vs[[4]], integer(0)))
}

is_checkmate_v2_1 <- function(){
  v <- get_pkg_version("checkmate")
  v$major == 2 && v$minor >= 1
}

is_purr_v1_or_pre1_dev <- function(){
  v <- get_pkg_version("purrr")
  #current dev version is called 0.9000.0.9000
  v$major == 1 || v$major == 0 && v$minor == 9000
}

purrr_error <- ifelse(is_purr_v1_or_pre1_dev(), "rlangerror", "simpleError")
