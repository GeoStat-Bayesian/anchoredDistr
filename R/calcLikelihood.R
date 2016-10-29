#' @include MADproject.R
NULL

#' Calculate the likelihood for the samples in a MADproject object.
#'
#' \code{calcLikelihood} returns the likelihood values based on the
#' observation and realization data in MADproject or, optionally,
#' a subset thereof.
#'
#' The likelihood calculation utilizes the \pkg{np} package for non-
#' parametric density estimation with all inversion data as dependent (i.e.
#' multivariate likelihood distributions are estimated).
#'
#' @param proj The MADproject object with data read from the MAD# databases.
#' @param dsubset The subset of inversion data to use for the likelihood
#' calculations.
#' @param num_realz The number of realizations to use in the likelihood
#' calculation (defaults to all in the \code{realizations} slot)
#' @param samples A vector of sample IDs for which to calculate
#' likelihood values (defaults to all available in the
#' \code{realizations} slot)
#' @return proj The updated MADproject object with a filled
#' \code{likelihood} slot.
#'
#' @importFrom plyr daply
#' @importFrom plyr .
#' @import np
#'
#' @export
setGeneric("calcLikelihood", function(proj, dsubset, ...) {
  standardGeneric("calcLikelihood")
})

setMethod("calcLikelihood",
          signature(proj="MADproject", dsubset="numeric"),
          function(proj, dsubset, num_realz=max(proj@realizations$rid), samples=1:proj@numSamples) {
            use <- subset(proj@realizations,
                          sid %in% samples & rid <= num_realz & zid %in% dsubset)
            suppressWarnings(proj@likelihoods <- data.frame(sid=unique(use$sid),
                                           like=daply(use, .(sid),
                                                      npLike,
                                                      obs=proj@observations[dsubset])
            ))
            return(proj)
          }
)

setMethod("calcLikelihood",
          signature(proj="MADproject"),
          function(proj, num_realz=max(proj@realizations$rid), samples=1:proj@numSamples) {
            use <- subset(proj@realizations,
                          sid %in% samples & rid <= num_realz)
            proj@likelihoods <- data.frame(sid=unique(use$sid),
                                           like=daply(use, .(sid),
                                                      npLike,
                                                      obs=proj@observations)
            )
            return(proj)
          }
)

npLike <- function(realz, obs){
  options(np.messages=FALSE)
  return(suppressWarnings(npudens(tdat=reshape2::dcast(realz,rid~zid,sum)[,-1],
                 edat=as.data.frame(t(obs)))$dens))
}
