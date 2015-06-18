##' trmm2ncdf function
##'
##' A function to convert TRMM rainfall data binaries into a NetCDF file
##'
##' @param directory directory in which the .bin files are stored. Note these must be gunzipped first 
##' @param outfile name and location of the NetCDF file to save results into
##' @param start start date e.g. ymd('20121231') 
##' @param end end date e.g. ymd('20130101')
##' @param by argument passed to seq(start,end,by), the sequence of days to download data from, default is all days between start and end 
##' @param hours the hours in the day for which to download data, a character string. Default is all hours.
##' @param product the product to download, default is '3B42RT', but other options are '3B41RT' and '3B40RT' 
##' @param version version of the data to download, default is 7, but could be either 5 or 6. Note that version 7 has the greatest temporal coverage at present 
##' @param poly a polygonal window bounding a spatial subset of the data to extract. By default, all data over the range covered by TRMM will be extracted
##' @param brick logical. If set to TRUE (the default) then this extracts all TRMM data at each time point
##' @param layer the name of the layer to extract. Options are ''
##' @param quiet logical: show progress?
##' @return an object of class trmmNCDF, for which there is a method extract_ncdf for extracting data from the saved NetCDF file. The returned list object contains an element firstslice which is a raster brick of data from the first time point, this can be used to give the extracted data geographical context.
##' @export

trmm2ncdf <- function(directory,outfile=tempfile(fileext=".nc"),start=ymd("20060101"),end=ymd("20060102"),by="days",hours=c("00","03","06","09","12","15","18","21"),product="3B42RT",version=7,poly=NULL,brick=TRUE,layer=NULL,quiet=FALSE){

    if(brick&!is.null(layer)){
        warning("brick is set to TRUE, extracting all layers. If a single layer is desired, set brick to FALSE and specify layer.",.immediate=TRUE)
    }

    if(!identical(CRS(proj4string(poly)),CRS("+init=epsg:4326"))){
        poly <- spTransform(poly,CRS("+init=epsg:4326"))
    }

    if(is.null(layer) & !brick){
        cat("Extracting layer 'precipitation'.\n")
        layer <- "precipitation"
    }

    dtseq <- seq(start,end,by=by)
    gr <- expand.grid(hours,dtseq)
    ymdh <- cbind(year(gr[,2]),month(gr[,2]),day(gr[,2]),as.character(gr[,1]))

    fun <- function(x){
        xx <- as.numeric(x)
        xx[xx<10] <- paste("0",xx[xx<10],sep="")
        return(xx)
    }
    ymdh[,2] <- fun(ymdh[,2])
    ymdh[,3] <- fun(ymdh[,3])

    N <- nrow(gr)

    initialised <- FALSE

    ncdata <- NULL
    dd <- NULL
    firstslice <- NULL

    getFun <- function(i){

        if(!quiet){
            print(paste(i,"of",N))
        }
        
        y <- ymdh[i,1]
        m <- ymdh[i,2]
        d <- ymdh[i,3]
        h <- ymdh[i,4]
        flname <- paste(product,".",y,m,d,h,".",version,".bin",sep="")
        altflname <- paste(product,".",y,m,d,h,".",version,"R2.bin",sep="")
        
        if(!file.exists(file.path(directory,flname))){
            flname <- altflname
        }

        h <- get_header(file.path(directory,flname))
        if(brick){
            if(is.null(poly)){
                out <- try(get_brick(h))
            }
            else{
                out <- try(crop(get_brick(h),poly))
            }
            
        }
        else{
            if(is.null(poly)){
                out <- try(get_layer(h,layer))
            }
            else{
                out <- try(crop(get_layer(h,layer),poly))
            }            
        }

        if(!initialised){
            initialised <<- TRUE
            tt <- dim.def.ncdf( "T", "time coordinates", 1:N)
            dd <<- dim(out)
            xx <- dim.def.ncdf( "X", "x coordinates", 1:dd[1])
            yy <- dim.def.ncdf( "Y", "y coordinates", 1:dd[2])
            vv <- dim.def.ncdf( "Type", "Variable type", 1:dd[3])
            sout <- var.def.ncdf("TRMM","none", list(tt,xx,yy,vv), missval=1.e30,prec="double")
            ncdata <- create.ncdf(outfile,sout)
            close.ncdf(ncdata)
            ncdata <<- open.ncdf(outfile,write=TRUE)
            firstslice <<- out # all coordinate and projection details available from this slice
        }

        if(!inherits(out,"try-error")){
            put.var.ncdf(ncdata,ncdata$var[[1]],raster::as.array(out),start=c(i,1,1,1),count=c(1,dd[1],dd[2],dd[3]))
        }
    }

    sapply(1:N,getFun)
    sync.ncdf(ncdata)
    close.ncdf(ncdata)

    ans <- list()
    ans$ymdh <- ymdh
    ans$file <- outfile
    ans$poly <- poly
    ans$firstslice <- firstslice
    ans$outfile <- outfile
    ans$directory <- directory
    ans$product <- product
    ans$version <- version

    class(ans) <- c("list","trmmNCDF")

    return(ans)

}


## dim_ncdf function
##
## Generic function for getting the dimension of NetCDF data
##
## @param dat an object
## @param ... additional arguments
## @return method dim_ncdf
## @export

# dim_ncdf <- function(dat,...){
#     UseMethod("dim_ncdf")
# }



##' dim_ncdf function
##'
##' A function to return the dimension of the NetCDF file saved by trmm2ncdf
##'
## @method dim_ncdf trmmNCDF
##' @param dat an object inheriting class trmmNCDF as created by trmm2ncdf 
##' @return the dimension of the NetCDF file
##' @seealso \link{trmm2ncdf}
##' @export

dim_ncdf <- function(dat){
    ncdata <- open.ncdf(dat$outfile)
    datadim <- ncdata$var$TRMM$varsize
    close.ncdf(ncdata)
    return(datadim)
}



## extract_ncdf function
##
## Generic function for the extraction of NetCDF data
##
## @param dat an object
## @param ... additional arguments
## @return method extract_ncdf
## @export

# extract_ncdf <- function(dat,...){
#     UseMethod("extract_ncdf")
# }



##' extract_ncdf function
##'
##' A function to extract data from the NetCDF file saved by trmm2ncdf
##'
## @method extract_ncdf trmmNCDF
##' @param dat an object inheriting class trmmNCDF as created by trmm2ncdf 
##' @param start the start index. A vector of indices indicating where to start reading the passed values (beginning at 1).  The length of this vector must equal the number of dimensions the variable has.  If not specified, reading starts at the beginning of the file (1,1,1,...).
##' @param count A vector of integers indicating the count of values to read along each dimension.  The length of this vector must equal the number of dimensions the variable has. If not specified and the variable does NOT have an unlimited dimension, the entire variable is read.  As a special case, the value '-1' indicates that all entries along that dimension should be read. By default this extracts data for the  first time point.
##' @return an array or matrix with the requested data
##' @seealso \link{trmm2ncdf}
##' @examples
##' \dontrun{extract_ncdf(dat=dat)}
##' \dontrun{extract_ncdf(dat=dat,start=c(1,1,1,1),count=c(1,2,3,1))}
##' \dontrun{extract_ncdf(dat=dat,start=c(1,1,1,1),count=c(-1,1,1,1))}
##' @export


extract_ncdf <- function(dat,start=NULL,count=NULL){

    ncdata <- open.ncdf(dat$outfile)
    datadim <- ncdata$var$TRMM$varsize
    
    if(is.null(start)){
        start <- rep(1,1,1,1)
    }

    if(is.null(count)){
        count <- c(1,datadim[2],datadim[3],1)           
    }
    
    x <- get.var.ncdf(nc=ncdata, varid=ncdata$var[[1]], start=start, count=count)
    close.ncdf(ncdata)

    return(x)
}

