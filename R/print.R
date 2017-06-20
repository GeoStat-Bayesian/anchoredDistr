#' @include MADproject.R
NULL

#' Print the data contained in MADproject object slots.
#'
#' \code{print.MADproject} plots information on the contents
#' in the slots of the provided MADproject.
#'
#' @param x The MADproject object
#' @param ... Not supported
#' @return NULL
#' @examples
#' data(pumping)
#' print(pumping)
#'
#' @export
print.MADproject <- function(x, ...) {
  cat("NAMES:\n")
  cat("The MAD project name:",x@madname,"\n")
  cat("The MAD result name:",x@resultname,"\n")
  cat("The path for the MAD project:",x@xpath,"\n")
  cat("\n")

  cat("CONFIGURATION VALUES:\n")
  cat("The number of measurement locations:",x@numLocations,"\n")
  cat("The number of time steps:",x@numTimesteps,"\n")
  cat("The number of samples:",x@numSamples,"\n")
  cat("The number of anchors:",x@numAnchors,"\n")
  cat("The number of structural parameters:",x@numTheta,"\n")
  cat("\n")

  cat("DATA DIMENSIONS:\n")
  cat("Size of true values:",dim(x@truevalues),"\n")
  cat("Size of observations:",length(x@observations),"\n")
  cat("Size of realizations:",dim(x@realizations),"\n")
  cat("Size of priors:",dim(x@priors),"\n")
  cat("Size of likelihoods:",dim(x@likelihoods),"\n")
  cat("Size of posteriors:",dim(x@posteriors),"\n")
  cat("\n")

}

