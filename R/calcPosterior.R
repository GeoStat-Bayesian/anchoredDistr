#' @include MADproject.R calcLikelihood.R
NULL

#' Calculate the posterior for a \code{MADproject} object.
#'
#' \code{calcPosterior} returns a MADproject object with the posterior values for each sample
#'
#' @param proj The \code{MADproject} object with the \code{likelihood}
#' slot filled.
#' @return proj An updated \code{MADproject} object with the
#' \code{posterior} slot filled.
#' @examples
#' data(pumping)
#' pumping.100 <- calcLikelihood(pumping, 100)
#' pumping.100 <- calcPosterior(pumping.100)
#'
#' @export
setGeneric("calcPosterior", function(proj) {
  standardGeneric("calcPosterior")
})

#' @describeIn calcPosterior Calculate the posterior value for each sample
setMethod("calcPosterior",
          signature(proj="MADproject"),
          function(proj) {
            if(length(proj@likelihoods) == 0){
              return("No likelihoods for comparison!")
            }
            priordens <- like <- tid <- ptotal <- sid <- name <- priorvalue <- post <- NULL
            postdata <- merge(proj@priors,
                              proj@likelihoods)
            postdata <- dplyr::mutate(postdata, prod=priordens*like)
            products <- dplyr::summarise(dplyr::group_by(postdata, tid), ptotal=sum(prod))
            postdata <- dplyr::mutate(merge(postdata,products), post=prod/ptotal)
            proj@posteriors <- subset(postdata,select=c(sid,tid,name,priorvalue,post))
            return(proj)
          }
)
