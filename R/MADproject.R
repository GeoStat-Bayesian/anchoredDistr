#' @include anchoredDistr.R
NULL

#' An S4 class to represent a MAD project
#'
#' @slot madname Character string, the name of the MAD#
#' project
#' @slot resultname Character string, the name of the MAD#
#' project's result
#' @slot xpath Character string, the path where to find the
#' MAD# project's result
#' @slot numLocations A numeric value for the number of
#' measurement locations
#' @slot numTimesteps A numeric value for the number of
#' time steps in series
#' @slot numSamples A numeric value for the number of
#' samples
#' @slot numAnchors A numeric value for the number of
#' local parameters to be inferred
#' @slot numTheta A numeric value for the number of
#' global parameters to be inferred
#' @slot truevalues A numeric vector of length
#' \code{numAnchors+numTheta} that contains
#' the true values of the parameters being inferred,
#' if known (i.e. for validation)
#' @slot observations A numeric vector containing the
#' observed values of inversion data
#' @slot realizations A data.frame containing the the
#' ensemble of simulated inversion data
#' for each sample
#' @slot priors A data.frame containing the values sampled
#' from the priors for each parameter to be inferred plus
#' estimated marginal density
#' @slot likelihoods A data.frame of likelihood values
#' calculated for each sample
#' @slot posteriors A data.frame containing the posterior
#' values estimated for each sample of each parameter
#'
#'
#' @export
MADproject <- setClass(
  # Set the name for the class
  "MADproject",

  # Define the slots
  slots = c(
    #projectname = "character",
    madname = "character",
    resultname = "character",
    xpath = "character",
    numLocations = "numeric",
    numTimesteps = "numeric",
    numSamples   = "numeric",
    numAnchors = "numeric",
    numTheta = "numeric",
    truevalues = "data.frame",
    observations = "numeric",
    realizations = "data.frame",
    priors = "data.frame",
    likelihoods = "data.frame",
    posteriors = "data.frame"
  )

)
