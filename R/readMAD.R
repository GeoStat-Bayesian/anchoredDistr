#' @include MADproject.R
NULL

#' Read the SQLite databases from MAD# into the MADproject object.
#'
#' \code{readMAD} returns an updated MADproject object with data from
#' the MAD# databases.
#'
#'
#' @param proj The MADproject object with the slots \code{madname},
#'   \code{resultname}, and \code{xpath} specified.
#' @param location The measurement ID(s) to be read from the MAD# databases
#' @return proj An updated MADproject object with slots \code{numTimesteps},
#'    \code{numLocations}, \code{numSamples}, \code{numAnchors},
#'    \code{numTheta}, \code{observations}, \code{priors},
#'    \code{truevalues} (if present),
#'    and \code{realizations} filled in from the MAD# databases.
#' @examples
#' \dontrun{
#' example <- new("MADproject", madname="Example", resultname="results",
#' xpath=getwd())
#' example <- readMAD(example, 1:3) #Observations 1-3
#' }
#'
#' @import np
#' @importFrom plyr adply dlply .
#'
#' @export
setGeneric("readMAD", function(proj, location) {
  standardGeneric("readMAD")
})

#' @describeIn readMAD Reads the MAD# databases for information related to \code{location}
setMethod("readMAD",
          signature(proj="MADproject", location="numeric"),
          function(proj, location) {
            variable <- . <- NULL
            #Setup database
            database <- paste(proj@xpath,"/",proj@madname,"_",proj@resultname,".xResult",sep="")
            datasample <- paste(proj@xpath,"/",proj@resultname,"/",proj@madname,"_",proj@resultname,sep="")
            con <- RSQLite::dbConnect(DBI::dbDriver("SQLite"), dbname =database)
            #Read project specifics
            proj@numLocations <- as.numeric(RSQLite::dbGetQuery( con,'select count(*) from selection' ))
            proj@numTimesteps <- as.numeric(RSQLite::dbGetQuery( con,'select count(*) from selectionvalues' ))/proj@numLocations
            proj@numSamples <- length(list.files(paste0(proj@xpath,proj@resultname,"/")))
            proj@numAnchors <- as.numeric(RSQLite::dbGetQuery( con,"SELECT count(typevar) FROM measure WHERE typevar LIKE 'Anchor_Type_A' "))
            proj@numTheta <- abs(proj@numAnchors - as.numeric(RSQLite::dbGetQuery( con,"SELECT count(*) from (SELECT structuralname FROM priordata group by structuralname) ")))
            #Read observations
            sqlvector=paste("select sv.value from  selectionvalues sv, likelihoodselecgroup s where sv.idselectionvalues= s.idselectionvalues   and s.idlikegroup=",1," order by sv.idselectionvalues;",sep='');
            res<- RSQLite::dbSendQuery(con,sqlvector)
            vector <- DBI::fetch(res, n=-1)
            proj@observations <- vector$value[(1:proj@numTimesteps) + (location-1)*proj@numTimesteps]
            RSQLite::dbClearResult(res)

            #Read realizations
            for(sample in 1:proj@numSamples){
              dbs <- paste(datasample,sample,".xdata",sep='')
              if (file.exists(dbs)){
                consa <- RSQLite::dbConnect(DBI::dbDriver("SQLite"), dbname =dbs)

                sql1=paste("select sv.idselectionvalues from  likelihoodselecgroup sv where  sv.idlikegroup=",1," order by sv.idselectionvalues",sep='');
                res<- RSQLite::dbSendQuery(con,sql1)
                zvectorid <- DBI::fetch(res, n=-1)
                zvector=zvectorid$idselectionvalues[(1:proj@numTimesteps) + (location-1)*proj@numTimesteps]
                RSQLite::dbClearResult(res)
                counter=1
                sqlverify=paste("select count(*) as numrealizations from resultselection r, resultid ri where r.idresult=ri.idresult and ri.sample=",sample," and r.idselectionvalues=",zvector[1],";",sep='');
                numrea<- RSQLite::dbGetQuery(consa,sqlverify)[[1]]

                tmp <-  array(0,c(numrea,length(zvector)))

                for(i in zvector){
                  sql2=paste("select r.value  from resultselection r, resultid ri where r.idresult=ri.idresult and ri.sample=",sample," and r.idselectionvalues=",i,"  order by ri.realization limit ", numrea,";",sep='');
                  res<- RSQLite::dbSendQuery(consa,sql2);
                  realizations<- DBI::fetch(res, n=-1);
                  value= realizations$value;
                  tmp[,counter]=value
                  counter=counter+1
                  RSQLite::dbClearResult(res)
                }
                RSQLite::dbDisconnect(consa)

                proj@realizations <- rbind(proj@realizations,
                                           data.frame(sid=rep(sample,nrow(tmp)*ncol(tmp)),
                                                      rid=rep(1:nrow(tmp),times=ncol(tmp)),
                                                      zid=rep(1:ncol(tmp),each=nrow(tmp)),
                                                      value=as.vector(tmp)
                                                      )
                                           )
              }
            }
            #Read priors
            param.names <- as.character(unlist(RSQLite::dbGetQuery( con,"SELECT DISTINCT fieldname FROM priordata")))
            priordata <- as.data.frame(t(adply(param.names, .margins=1, .id=NULL,
                                function(name){
                    return(RSQLite::dbGetQuery( con,paste0("SELECT value FROM priordata WHERE fieldname like '",name,"'"))[1:proj@numSamples,1])
                  }
            )))
            priordata$sid <- 1:proj@numSamples
            priordata <- reshape2::melt(priordata, "sid")
            tmp <- as.data.frame(dlply(priordata, .(variable), function(param){
              npudens(tdat=param$value,
                      edat=param$value)$dens
            }))


            tmp$sid <- 1:proj@numSamples
            tmp2 <- reshape2::melt(tmp,"sid")
            tmp2$tid <- unlist(lapply(strsplit(as.character(tmp2$variable), "V"),
                                      function(x){x[-1]}))
            proj@priors <- cbind(tmp2[,-2],name=param.names[as.numeric(tmp2$tid)],
                                 priorvalue=priordata$value)
            names(proj@priors)[2] <- "priordens"

            #Read true values
            tvals <- as.numeric(unlist(RSQLite::dbGetQuery( con,"SELECT value FROM truevalues")))
            proj@truevalues <- data.frame(tid=1:length(tvals), value=tvals)
            RSQLite::dbDisconnect(con)
            return(proj)
          }
          )

