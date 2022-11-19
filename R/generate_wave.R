

#   __________________ #< 68d7309fea7a007c29abd251c0f33a7c ># __________________
#   Generate wave signal                                                    ####


#' @title Generate simple wave signal
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Generate y-values for the wave signal at the given time points with:
#'
#'  \deqn{amplitude * trig_fn(2 * pi * (1 / fs) * ts + phase)}
#' @param ts Time points.
#' @param fs Sampling frequency (samples per second).
#'
#'  Can be a vector with one frequency per time point.
#' @param amplitude Scaling constant.
#'
#'  Can be a vector with one amplitude per time point.
#' @param phase Offset in radians.
#'
#'  Can be a vector with one phase offset per time point.
#' @param trig_fn Trigonometric function like \code{sin} and \code{cos}.
#' @keywords internal
#' @return y-values for the wave signal.
generate_wave <- function(ts,
                          fs = 44100,
                          amplitude = 1,
                          phase = 0,
                          trig_fn = sin) {
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_numeric(ts, any.missing = FALSE, add = assert_collection)
  checkmate::assert_numeric(
    1 / fs,
    any.missing = FALSE,
    finite = TRUE,
    lower = 0,
    upper = 1,
    add = assert_collection
  )
  checkmate::assert_numeric(amplitude, any.missing = FALSE, add = assert_collection)
  checkmate::assert_numeric(
    phase,
    any.missing = FALSE,
    lower = -2 * pi,
    upper = 2 * pi,
    add = assert_collection
  )
  checkmate::assert_function(trig_fn, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  if (length(fs) != 1 && length(fs) != length(ts)) {
    assert_collection$push("when 'fs' has more than one element, it must have same length as 'ts'.")
  }
  if (length(amplitude) != 1 && length(amplitude) != length(ts)) {
    assert_collection$push("when 'amplitude' has more than one element, it must have same length as 'ts'.")
  }
  if (length(phase) != 1 && length(phase) != length(ts)) {
    assert_collection$push("when 'phase' has more than one element, it must have same length as 'ts'.")
  }
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  amplitude * trig_fn(2 * pi * (1 / fs) * ts + phase)
}

#' @rdname generate_wave
generate_sine_wave <- function(ts,
                               fs = 44100,
                               amplitude = 1,
                               phase = 0) {
  generate_wave(
    ts = ts,
    fs = fs,
    amplitude = amplitude,
    phase = phase,
    trig_fn = sin
  )
}

#' @rdname generate_wave
generate_cosine_wave <- function(ts,
                                 fs = 44100,
                                 amplitude = 1,
                                 phase = 0) {
  generate_wave(
    ts = ts,
    fs = fs,
    amplitude = amplitude,
    phase = phase,
    trig_fn = cos
  )
}


# Future examples (Only add when functions are exported)
# @examples
# \dontrun{
# # Attach packages
# library(rearrr)
# library(dplyr)
# has_ggplot <- require(ggplot2)  # Attach if installed
#
# # Set seed
# set.seed(2)
#
# # Create a data frame
# df <- data.frame(
#   "x" = 1:100
# )
#
# # Generate sine wave
# df$sine <- rearrr:::generate_sine_wave(ts = df$x, freq = 1 / 50)
#
# if (has_ggplot){
#   df %>%
#     ggplot(aes(x = x, y = sine)) +
#     geom_path() +
#     theme_minimal()
# }
#
# # Generate cosine wave
# df$cosine <- rearrr:::generate_cosine_wave(ts = df$x, freq = 1 / 50)
#
# if (has_ggplot){
#   df %>%
#     ggplot(aes(x = x, y = cosine)) +
#     geom_path() +
#     theme_minimal()
# }
# }
