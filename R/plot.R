#' @include MADproject.R
NULL

#' Plot the data contained in MADproject object slots.
#'
#' \code{plot} plots the data contained in various
#' slots of the provided MADproject. If no specific slot is
#' specified, all slots that can be plotted will be.
#'
#' All plots utilize \pkg{ggplot2} plotting functions. The slots
#' that can be plotted are \code{priors}, \code{observations},
#' \code{realizations}, and \code{posteriors}.
#'
#' @param x The MADproject object
#' @param y character string from the following options: "observations",
#' "realizations", "priors", or "posteriors"
#' @return NULL
#' @examples
#' data(pumping)
#' plotMAD(pumping, "realizations")
#' plotMAD(pumping, "priors")
#' plotMAD(pumping, "posteriors")
#' plotMAD(pumping)
#'
#'
#' @import ggplot2
#' @importFrom Rmisc multiplot
#'
#' @export
setGeneric("plotMAD", function(x,y) {
  standardGeneric("plotMAD")
})

#' @describeIn plotMAD Plots all available slots in the MADproject object
setMethod("plotMAD",
          signature(x="MADproject"),   #plot all available plots
          function(x) {
            #Plot observations
            if(length(x@observations) > 0){
              plotMAD(x,"observations")
              #Plot realizations if not too many samples
              if((x@numSamples < 5)) {
                plotMAD(x,"realizations")
              }
            }
            #Plot priors
            if(length(x@priors) > 0) plotMAD(x, "priors")
            #Plot posterior
            if(length(x@posteriors) > 0) plotMAD(x, "posteriors")
          }
)

#' @describeIn plotMAD Plots the \code{y} slot in the MADproject object
setMethod("plotMAD",
          signature(x="MADproject", y="character"),
          function(x,y) {
            time <- sid <- zid <- value <- p25 <- p75 <- priorvalue <- post <- priordens <- ..density.. <- NULL
            switch(y,
                   observations = {
                     if(length(x@observations) == 0){
                       return("No observations to print!")
                     }
                     if(length(x@observations) == x@numTimesteps){
                      df <- data.frame(obs=x@observations,
                                       time=1:length(x@observations))
                      ggplot(df) + geom_line(aes(x=time,y=obs)) +
                        ylab("zb Observation") + xlab("Time Steps")
                     }
                    },
                   realizations = {
                     if(x@numSamples>6){
                       return("Too many samples for plotting realizations!")
                     }
                     if(length(x@observations) == x@numTimesteps){  #Time series
                         diff <- dplyr::summarise(dplyr::group_by(x@realizations, sid, zid),
                                   p25=stats::quantile(value,probs=0.25),
                                   p75=stats::quantile(value,probs=0.75))
                         diff$obs <- x@observations
                         ggplot(diff)  +
                           geom_ribbon(aes(x=zid, ymin=p25,ymax=p75)) +
                           geom_line(aes(x=zid,y=obs)) +
                           guides(color="none") +
                           xlab("Time steps") +
                           ylab(paste("zb, Sample",diff$sid)) +
                           facet_grid(sid ~ .)
                       } else if (x@numTimesteps == 1) { #steady
                         obs <- data.frame(zid=1:length(x@observations), obs=x@observations)
                         ggplot(merge(x@realizations,obs), aes(x=value,fill=sid))  +
                           geom_density(alpha=0.25) +
                           geom_vline(aes(xintercept=obs)) +
                           facet_grid(zid ~ .)
                     } else {  #Reduced
                       obs <- data.frame(zid=1:length(x@observations),
                                          obs=x@observations)
                       ggplot(merge(x@realizations,obs), aes(x=value, group=as.factor(sid),
                                     colour=as.factor(sid)))  +
                         geom_density(alpha=0.25) +
                         scale_colour_discrete(name = "Sample ID") +
                         geom_vline(aes(xintercept=obs)) +
                         xlab(paste("Parameter",zid)) +
                         facet_grid(. ~ zid)
                       }
                    },
                   posteriors = {
                     if(length(x@posteriors) == 0){
                       return("No posteriors to print!")
                     }
                     if(length(x@truevalues) == 0){
                       df <- merge(x@posteriors,x@truevalues)
                       ggplot(df, aes(x=priorvalue, weight=post))  +
                         geom_density(weight=1, fill=NA, colour="red") +
                         geom_density(fill=NA, colour="blue") +
                         geom_vline(aes(xintercept = value), na.rm=TRUE) +
                         xlab(unique(df$name)) +
                         facet_grid(tid ~ .)
                     } else {
                       df <- x@posteriors
                       ggplot(df, aes(x=priorvalue, weight=post))  +
                         geom_density(weight=1, fill=NA, colour="red") +
                         geom_density(fill=NA, colour="blue") +
                         xlab(unique(df$name)) +
                         facet_grid(tid ~ .)
                     }
                    },
                   priors = {
                     if(length(x@priors) == 0){
                       return("No priors to print!")
                     }
                     df <- x@priors
                     ggplot(df, aes(priorvalue,y=priordens))  +
                       geom_bar(stat="identity", width=1/length(df$priorvalue)) +
                       geom_density(aes(priorvalue,..density..),fill=NA, colour="red")+
                       xlab(unique(df$name)) + ylab("density") +
                       facet_grid(tid ~ .)
                   }
                    )
          }
)
