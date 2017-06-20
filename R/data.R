#' anchoredDistr example data
#'
#' A dataset containing a MADproject object called 'pumping'
#'
#' @docType data
#'
#' @usage data(pumping)
#'
#' @format A MADproject object with an observed time series, three
#' prior samples, realizations for each sample, and true values for
#' validation where the parameter to be inferred is the global mean
#' of the natural log conductivity of a 2D aquifer.
#' \describe{
#'   \item{@observations}{a time series of drawdown, in meters}
#'   \item{@numSamples}{the value 3, [unitless]}
#'   \item{@realizations}{the ensemble of simulated drawdown time series}
#' }
#' @source NULL
"pumping"

#' anchoredDistr example data
#'
#' A dataset containing a MADproject object called 'tutorial'
#'
#' @docType data
#'
#' @usage data(tutorial)
#'
#' @format A MADproject object with an observed head at three wells, 145
#' prior samples, realizations for each sample, and true values for
#' validation where the parameters to be inferred are four anchors of the
#' local decimal log transmissivity of a 1D aquifer
#' \describe{
#'   \item{@observations}{three head measurements, in meters}
#'   \item{@realizations}{the ensemble of simulated head measurements}
#'   \item{@numAnchors}{the value 4}
#'   \item{@priors}{the samples for each anchor's prior distribution
#'   based on the kriging mean and variance at the anchor locations}
#' }
#' @source \url{http://www.mad.codeplex.com/releases/}
"tutorial"

