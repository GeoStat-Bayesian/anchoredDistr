#' @include MADproject.R
NULL

#' Calculate the posterior for a \code{MADproject} object.
#'
#' \code{calcPosterior} returns the posterior values for each sample
#'
#' @param proj The \code{MADproject} object with the \code{likelihood}
#' slot filled.
#' @return proj An updated \code{MADproject} object with the
#' \code{posterior} slot filled.
#'
#' @export
setGeneric("calcPosterior", function(proj, likes) {
  standardGeneric("calcPosterior")
})

setMethod("calcPosterior",
          signature(proj="MADproject"),
          function(proj) {
            postdata <- merge(proj@priors,
                              proj@likelihoods)
            postdata <- dplyr::mutate(postdata, prod=priordens*like)
            products <- dplyr::summarise(dplyr::group_by(postdata, tid), ptotal=sum(prod))
            postdata <- dplyr::mutate(merge(postdata,products), post=prod/ptotal)
            proj@posteriors <- subset(postdata,select=c(sid,tid,name,priorvalue,post))
            return(proj)
          }
)
