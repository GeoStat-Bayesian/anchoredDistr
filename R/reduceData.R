#' @include MADproject.R
NULL

#' Apply the requested dimension reduction technique to the
#' inversion data for the given \code{MADproject} object
#'
#' \code{reduceData} returns a modifed \code{MADproject}
#' object where the \code{observations} and \code{realizations}
#' slots are replaced
#'
#' @param proj \code{MADproject} object that has been filled by
#' MAD# databases
#' @param method function to apply to data
#' @param params function for estimating the initial values of
#' the parameters of \code{method} based on the data, if
#' \code{method} has parameters
#' @param ... addtional arguments as needed for \code{nls} to
#' fit \code{method} to the data
#'
#' @return proj \code{MADproject} object with reduced dimensions
#' for inversion
#'
#' @importFrom plyr ddply
#'
#' @export
setGeneric("reduceData", function(proj, method, params, ...) {
  standardGeneric("reduceData")
})

setMethod("reduceData",
          signature(proj="MADproject", method="function"),
          function(proj, method) {
            proj@observations <- match.fun(method)(proj@observations)
            reduced <- ddply(proj@realizations, .(sid,rid),
                                       function(df){match.fun(method)(df$value)}
                                       )
            proj@realizations <- data.frame(sid=reduced$sid,
                                            rid=reduced$rid,
                                            zid=1,
                                            value=reduced$V1)
            return(proj)
          }
)

setMethod("reduceData",
          signature(proj="MADproject", method="function", params="function"),
          function(proj, method, params, ...) {
            fit <- nls(y~match.fun(method)(t,init.params),
                data=data.frame(t=1:proj@numTimesteps,y=proj@observations),
                start=list(init.params=match.fun(params)(proj@observations)),
                nls.control(warnOnly=TRUE),
                ...
            )
            proj@observations <- coefficients(fit)
            reduced <- ddply(proj@realizations, .(sid,rid),
                                        function(df){
                                            fit <- nls(y~match.fun(method)(t,init.params),
                                                       data=data.frame(t=df$zid,
                                                                       y=df$value),
                                                       start=list(init.params=
                                                                    match.fun(params)(df$value)),
                                                       nls.control(warnOnly=TRUE),
                                                       ...
                                            )
                                            coeffs <- coefficients(fit)
                                            names(coeffs) <- 1:length(coeffs)
                                            return(coeffs)
                                        }
            )
            reduced <- reshape2::melt(reduced, c("sid","rid"))
            names(reduced)[3] <- "zid"
            proj@realizations <- reduced
            return(proj)
          }
)
