

#   __________________ #< b374fd49354d1eddc7d25f2e43608f7c ># __________________
#   Apply transformation differently per group                              ####


# Idea for example:
# agp <- GroupedPipe$new(3)
# agp$add_transformation(fn=rotate_2d, args=list(x_col="Index", y_col="A", origin=c(0,0),
#                                                suffix="", overwrite=TRUE),
#                        var_args = list(degrees=list(45, 90, 120)), name="rotate")
# agp$add_transformation(fn=cluster_groups, args=list(cols=c("Index","A"),
#                                                     suffix="", overwrite=TRUE),
#                        var_args = list(multiplier=list(0.05, 0.1, 0.2)), name="cluster")
# # agp$transformations
# agp$apply(dplyr::group_by(df, G), verbose=T)


# Reminder: Side-effect R6 methods should always return self invisibly!


# A transformation to be applied with
# different argument values to each group
GroupedTransformation <- R6::R6Class(
  "GroupedTransformation",
  list(
    name = NULL,
    fn = NULL,
    args = NULL,
    var_args = NULL,
    num_groups = NULL,
    initialize = function(fn, args, var_args, name = NULL) {
      # Check arguments ####
      assert_collection <- checkmate::makeAssertCollection()
      checkmate::assert_function(fn, add = assert_collection)
      checkmate::assert_list(args, names = "unique", add = assert_collection)
      checkmate::assert_list(var_args, names = "unique", add = assert_collection)
      checkmate::assert_string(name, null.ok = TRUE, add = assert_collection)
      checkmate::reportAssertions(assert_collection)
      if (length(intersect(names(args), names(var_args))) > 0) {
        assert_collection$push("`args` and `var_args` contained the same names.")
      }
      if (sum(abs(diff(lengths(var_args)))) != 0) {
        assert_collection$push("all lists in `var_args` must have the same length (one arg value per group).")
      }
      checkmate::reportAssertions(assert_collection)
      # End of argument checks ####

      # Assign to object
      self$fn <- fn
      self$args <- args
      self$var_args <- var_args
      self$name <- name
      self$num_groups <- length(var_args[[1]])
    },
    get_group_args = function(group_id) {
      checkmate::assert_integerish(
        group_id,
        lower = 0,
        upper = self$num_groups,
        any.missing = FALSE,
        len = 1
      )
      var_args <- sapply(self$var_args, `[[`, group_id)
      var_args <- setNames(var_args, names(self$var_args))
      c(self$args, var_args)
    },
    apply_to_group = function(data, group_id) {
      if (dplyr::is_grouped_df(data)) {
        stop("`data` was grouped. Pass the group subset instead.")
      }
      args <- self$get_group_args(group_id)
      args <- c(list(data = data), args)
      do.call(self$fn, args, envir = parent.frame())
    },
    apply = function(data) {
      if (dplyr::n_groups(data) != self$num_groups) {
        stop(
          paste0(
            "`data` did not have the right number of groups: ",
            dplyr::n_groups(data),
            " != ",
            self$num_groups,
            "."
          )
        )
      }
      pre_grouping_vars <- dplyr::groups(data)
      data <- run_by_group(data = data, fn = self$apply_to_group) %>%
        dplyr::group_by(!!!pre_grouping_vars)
    }
  )
)

# Applies one transformation at a time
# With different arguments per group
GroupedPipe <- R6::R6Class("GroupedPipe", list(
  transformations = list(),
  names = character(),
  num_groups=NULL,
  initialize = function(num_groups) {
    self$num_groups <- num_groups
  },
  add_transformation = function(fn, args, var_args, name){
    if (name %in% self$names){
      stop(paste0("the `name`, ", name, ", already exists. Names must be unique."))
    }

    self$names <- append(self$names, name)
    transformation <- GroupedTransformation$new(fn=fn, args=args, var_args=var_args, name=name)
    if (transformation$num_groups != self$num_groups){
      stop("the transformation must have the same number of groups (see var_args) as the GroupedPipe.")
    }
    self$transformations <- c(self$transformations, setNames(list(transformation), name))
  },
  apply = function(data, verbose=FALSE){
    # TODO benchmark if another looping function with env assignment
    # is faster with e.g. 5-10 transformations
    if (isTRUE(verbose)){
      cat(paste0("\n",paste0(rep("-", 54), collapse = "")))
      cat("\nApplying transformations.")
      start_time <- proc.time()
    }

    for (name in self$names){
      if (isTRUE(verbose)){
        cat(paste0("\nStarting: ", name))
        transf_start_time <- proc.time()
      }

      # Apply transformation
      data <- self$transformations[[name]]$apply(data = data)

      if (isTRUE(verbose)){
        transf_end_time <- proc.time()
        cat(paste0(
          "\nEnded: ", name,
          " | Took ", format_running_time_(transf_start_time, transf_end_time), "."
        ))
      }
    }
    if (isTRUE(verbose)){
      end_time <- proc.time()
      cat(paste0(
        "\nFinished applying transformations. Total time: ",
        format_running_time_(start_time, end_time),"."
      ))
      cat(paste0("\n",paste0(rep("-", 54), collapse = ""),"\n"))
    }

    data
  }
))

format_running_time_ <- function(t_start, t_end, digits=4, suffix="s"){
  t_total <- t_end[["elapsed"]] - t_start[["elapsed"]]
  t_total <- round(t_total, digits=digits)
  paste0(t_total, suffix)
}
