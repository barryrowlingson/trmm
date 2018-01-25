##' getTRMM function
##'
##' A function to download 3-hourly TRMM data into a directory of your choice.
##'
##' @param outdir the directory in which to save files
##' @param start start date e.g. ymd('20121231')
##' @param end end date e.g. ymd('20130101')
##' @param by argument passed to seq(start,end,by), the sequence of days to download data from, default is all days between start and end
##' @param hours the hours in the day for which to download data, a character string. Default is all hours.
##' @param product the product to download, default is '3B42RT', but other options are '3B41RT' and '3B40RT'
##' @param version version of the data to download, default is 7, but could be either 5 or 6. Note that version 7 has the greatest temporal coverage at present
##' @param website the website from which to do the downloading, default is 'ftp://trmmopen.gsfc.nasa.gov/pub/merged'
##' @return Downloads the files requested into the directory specified.
##' @export

getTRMM <- function(outdir,start=lubridate::ymd("20121231"),end=lubridate::ymd("20130101"),by="days",hours=c("00","03","06","09","12","15","18","21"),product="3B42RT",version=7,website="ftp://trmmopen.gsfc.nasa.gov/pub/merged"){

    if(!any(version==5:7)){
        stop("Version must be a numeric: either 5, 6 or 7.")
    }

    dtseq <- seq(start,end,by=by)
    gr <- expand.grid(hours,dtseq)
    ymdh <- cbind(lubridate::year(gr[,2]),lubridate::month(gr[,2]),lubridate::day(gr[,2]),as.character(gr[,1]))

    fun <- function(x){
        xx <- as.numeric(x)
        xx[xx<10] <- paste("0",xx[xx<10],sep="")
        return(xx)
    }
    ymdh[,2] <- fun(ymdh[,2])
    ymdh[,3] <- fun(ymdh[,3])

    N <- nrow(gr)

    obtained <- rep(0,N)

    old_obtained <- obtained

    while(!all(obtained==1)){
        for(i in 1:N){

            y <- ymdh[i,1]
            m <- ymdh[i,2]
            d <- ymdh[i,3]
            h <- ymdh[i,4]

            if(obtained[i]==1){
                cat("Already obtained data for",y,"-",m,"-",d,"-",h,"\n")
                next
            }

            if(version==7){
                ftpaddr <- file.path(website,product,y,m)
                flname <- paste(product,".",y,m,d,h,".",version,"R2.bin.gz",sep="")
                altflname <- paste(product,".",y,m,d,h,".",version,".bin.gz",sep="")
            }
            else{
                ftpaddr <- file.path(website,product,paste("V",version,sep=""),y)
                if(version==6){
                    flname <- paste(product,".",y,m,d,h,".",version,".bin.gz",sep="")
                    altflname <- flname
                }
                if(version==5){
                    flname <- paste(product,".",y,m,d,h,".bin.gz",sep="")
                    altflname <- flname
                }                               
            }

            inpath <- file.path(ftpaddr,flname)
            altinpath <- file.path(ftpaddr,altflname)
            outpath <- file.path(outdir,flname)
            altoutpath <- file.path(outdir,altflname)

            if(file.exists(outpath) & file.size(outpath)>0){
                cat("Already fetched",inpath,"\n")
                obtained[i] <- 1
                next
            }
            else if(file.exists(altoutpath) & file.size(altoutpath)>0){
                cat("Already fetched",altinpath,"\n")
                obtained[i] <- 1
                next
            }
            else{
                try(utils::download.file(inpath,outpath))
                if(file.exists(outpath) & file.size(outpath)>0){
                    obtained[i] <- 1
                }
                else{
                    cat(inpath,"failed, trying",altinpath,"instead.\n")
                    try(utils::download.file(altinpath,altoutpath))
                    if(file.exists(altoutpath) & file.size(altoutpath)>0){
                        obtained[i] <- 1
                    }   
                }

               
            }
           
        }


        new_obtained <- obtained

        sleeptime <- sample(30:120,1)

        if(all(old_obtained==new_obtained)){ # then the site is barring access, wait a bit
            print(obtained)
            cat("Looks like you're unable to access site at the moment, waiting",sleeptime,"seconds.\n")
            Sys.sleep(sleeptime)
        }

        old_obtained <- new_obtained

    }

    cat("Done.\n")

}

file.size <- function(f){
    file.info(f)$size
}
