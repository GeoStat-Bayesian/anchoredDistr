#' @include MADproject.R calcLikelihood.R
NULL

#' Test (visually) the convergence of a MADproject object.
#'
#' \code{test_convergence} returns a plot to help the user visualize if
#' there are enough realizations in the project for converged likelihood
#' values
#'
#' @param proj The MADproject object to be tested.
#' @param dsubset The subset of inversion data to use for the likelihood
#' calculations.
#' @param samples A vector of sample IDs to sample from to calculate
#' likelihood values (defaults to all available in the \code{MADproject} object)
#' @param NR The number of different realization totals for which to
#' calculate likelihood values (defaults to 10)
#' @param NS The number of randomly selected samples to test (defaults to 7)
#' out of \code{samples}
#' @return NULL
#'
#' @export
setGeneric("test_convergence", function(proj, dsubset, samples=1:proj@numSamples, NR=10, NS=7) {
  standardGeneric("test_convergence")
})

#' @describeIn test_convergence Tests the convergence using a subset \code{dsubset}
#'  of inversion data \code{zid}
setMethod("test_convergence",
          signature(proj="MADproject", dsubset="numeric"),
          function(proj, dsubset, samples, NR, NS) {
            sid <- zid <- like <- NULL
            minr <- min(daply(subset(proj@realizations,sid %in% samples & zid %in% dsubset),
                              .(sid), function(df) max(df$rid)))
            samps <- sample(samples,min(NS,length(samples)))
            nr <- seq(ceiling(.1*minr),minr,length.out=NR)
            likes <- plyr::adply(nr, 1, function(x) calcLikelihood(proj, dsubset=dsubset,
                                                                   num_realz=x,
                                                                   samples=samps)@likelihoods)
            likes$nr <- nr[likes$X1]
            likes$sid <- as.factor(likes$sid)
            ggplot(likes, aes(x=nr, y=like, group=sid, colour=sid))  +
              geom_line() + scale_y_log10() + xlab("Number of Realizations") +
              ylab("Log 10 Likelihood") +  scale_colour_discrete(name = "Sample ID")

          }
)

#' @describeIn test_convergence Tests the convergence using all inversion data \code{zid}
setMethod("test_convergence",
          signature(proj="MADproject"),
          function(proj, samples, NR, NS) {
            sid <- zid <- like <- NULL
            minr <- min(daply(subset(proj@realizations,sid %in% samples),
                              .(sid), function(df) max(df$rid)))
            samps <- sample(samples,min(NS,length(samples)))
            nr <- seq(ceiling(.1*minr),minr,length.out=NR)
            likes <- plyr::adply(nr, 1, function(x) calcLikelihood(proj,
                                                             num_realz=x,
                                                             samples=samps)@likelihoods)
            likes$nr <- nr[likes$X1]
            likes$sid <- as.factor(likes$sid)
           ggplot(likes, aes(x=nr, y=like, group=sid, colour=sid))  +
             geom_line() + scale_y_log10() + xlab("Number of Realizations") +
             ylab("Log 10 Likelihood") +  scale_colour_discrete(name = "Sample ID")

          }
)
