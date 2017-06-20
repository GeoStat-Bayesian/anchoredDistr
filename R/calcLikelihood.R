#' @include MADproject.R
NULL

#' Calculate the likelihood for the samples in a MADproject object.
#'
#' \code{calcLikelihood} returns an updated MADproject with
#' the likelihood values based on the
#' observation and realization data in the MADproject or, optionally,
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
#' @examples
#' data(pumping)
#' pumping <- calcLikelihood(pumping, 100) #Inversion data as time step 100
#'
#' @importFrom np npudens
#' @import dplyr
#'
#' @export
setGeneric("calcLikelihood", function(proj, dsubset, num_realz=max(proj@realizations$rid), samples=1:proj@numSamples) {
  standardGeneric("calcLikelihood")
})

#' @describeIn calcLikelihood Calculates the likelihood using a subset \code{dsubset}
#'  of inversion data \code{zid}
setMethod("calcLikelihood",
          signature(proj="MADproject", dsubset="numeric"),
          function(proj, dsubset, num_realz, samples) {
            if(length(proj@observations) == 0){
              return("No observations for comparison!")
            }
            if(length(proj@realizations) == 0){
              return("No realizations for comparison!")
            }
            sid <- rid <- zid <- . <- NULL
            proj@likelihoods <- as.data.frame(proj@realizations %>%
                             filter_(~sid %in% samples & rid <= num_realz & zid %in% dsubset) %>%
                             group_by_(~sid) %>%
                             do(data.frame(like=npLike(.,obs=proj@observations[dsubset]))))
            return(proj)
          }
)

#' @describeIn calcLikelihood Calculates the likelihood using all inversion data \code{zid}
setMethod("calcLikelihood",
          signature(proj="MADproject"),
          function(proj, num_realz, samples) {
            if(length(proj@observations) == 0){
              return("No observations for comparison!")
            }
            if(length(proj@realizations) == 0){
              return("No realizations for comparison!")
            }
            sid <- rid <- . <- NULL
            proj@likelihoods <- as.data.frame(proj@realizations %>%
                                                filter_(~sid %in% samples & rid <= num_realz) %>%
                                                group_by_(~sid) %>%
                                                do(data.frame(like=npLike(.,obs=proj@observations))))
            return(proj)
          }
)

npLike <- function(realz, obs){
  options(np.messages=FALSE)
  options(np.tree=FALSE)
  tdat <- reshape2::dcast(realz,rid~zid,sum)[,-1]
  edat <- as.data.frame(t(obs))
  dens <- npudens(tdat,edat)$dens
  return(dens)
}
