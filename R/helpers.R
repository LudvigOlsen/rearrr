

#   __________________ #< 837022fb68986f4274072896ad1f8be6 ># __________________
#   Helpers                                                                 ####


##  .................. #< a309bc456a67722d30118ad015672baa ># ..................
##  Is correctly positioned                                                 ####


is_correctly_positioned <- function(data, col, position, what = "max"){
  target_val <- ifelse(what == "max", max(data[[col]]), min(data[[col]]))
  data[[col]][[position]] == target_val
}


##  .................. #< 8e6f910bb09315ea08d9791e6a30c6a7 ># ..................
##  Swap values                                                             ####


swap_values <- function(vec, v1, v2){
  v1_ind <- vec[vec == v1]
  v2_ind <- vec[vec == v2]
  vec[v1_ind] <- v2
  vec[v2_ind] <- v1
  vec
}


##  .................. #< f5bf1e543ca585628647e0f35453d42f ># ..................
##  Base helpers                                                            ####


base_rename <- function(data, before, after, warn_at_overwrite = FALSE) {

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
base_select <- function(data, cols) {
  tryCatch(subset(data, select = cols), error = function(e){
    if (grepl("is missing", e)){
      stop("base_select() only work on data frames.")
    } else {
      stop(paste0("base_select() got error from subset(): ", e))
    }
  })
}

# Cols should be col names
base_deselect <- function(data, cols) {
  if (!is.character(cols)) stop("cols must be names")
  base_select(data = data, cols = setdiff(names(data), cols))
}


##  .................. #< ee7952f84c8fa1b183ab4a3b923b9903 ># ..................
##  Position first                                                          ####


# Col should be col name
position_first <- function(data, col) {
  if (is.numeric(col)) stop("col must be name")
  # if(is.data.table(data)){
  #   return(data[, c(col, setdiff(names(data), col)), with = FALSE])
  # }

  base_select(data = data, cols = c(col, setdiff(names(data), col)))
}


##  .................. #< 4cb48e1b875b60049471d53ddceeb9dc ># ..................
##  Insert row                                                              ####


# insertRow2 from https://stackoverflow.com/a/11587051/11832955
# Note: May not work with rownames!
insert_row <- function(data, new_row, after) {
  data <- rbind(data, new_row)
  data <- data[order(c(seq_len(nrow(data) - 1), after + 0.5)),
               , drop = FALSE] # extra comma on purpose
  row.names(data) <- NULL
  data
}


##  .................. #< 551180c531b6373a09823e43527a5c9d ># ..................
##  Create tmp column name                                                  ####


# Add underscore until var name is unique
# arg disallowed can add extra things not to be named as
create_tmp_var <- function(data, tmp_var = ".tmp_index_", disallowed = NULL) {

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
create_tmp_val <- function(v, tmp_val = ".tmp_val_", disallowed = NULL) {

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


##  .................. #< 76a9352ccecc01349336a01b9a870511 ># ..................
##  Greedy windows                                                          ####


greedy_windows <- function(data, window_size, factor_name = ".window") {
  size <- nrow(data)
  num_windows <- ceiling(size / window_size)
  windows <- rep(seq_len(num_windows), each = window_size)
  data[[factor_name]] <- head(windows, size)
  data
}


#   __________________ #< 60cfc78f594e5611a6eaaf34a2b212ae ># __________________
#   ImportFrom                                                              ####

#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @importFrom utils head tail
#' @importFrom stats quantile runif
NULL
