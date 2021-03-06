% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_wave.R
\name{generate_wave}
\alias{generate_wave}
\alias{generate_sine_wave}
\alias{generate_cosine_wave}
\title{Generate simple wave signal}
\usage{
generate_wave(ts, fs = 44100, amplitude = 1, phase = 0, trig_fn = sin)

generate_sine_wave(ts, fs = 44100, amplitude = 1, phase = 0)

generate_cosine_wave(ts, fs = 44100, amplitude = 1, phase = 0)
}
\arguments{
\item{ts}{Time points.}

\item{fs}{Sampling frequency (samples per second).

Can be a vector with one frequency per time point.}

\item{amplitude}{Scaling constant.

Can be a vector with one amplitude per time point.}

\item{phase}{Offset in radians.

Can be a vector with one phase offset per time point.}

\item{trig_fn}{Trigonometric function like \code{sin} and \code{cos}.}
}
\value{
y-values for the wave signal.
}
\description{
\Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}

Generate y-values for the wave signal at the given time points with:

\deqn{amplitude * trig_fn(2 * pi * (1 / fs) * ts + phase)}
}
\keyword{internal}
