
splut <- function(v,sep=","){
    ## split on comma
    stringr::str_split(v,sep)[[1]]
}

size_map <- function(s){
    ## convert text sizes to bit sizes
    sizes = c("signed_integer1"=8,
        "signed_integer2"=16)
    sizes[s]
}

compute_edge <- function(s){
    ## convert string like "90N" into signed degrees
    parts = stringr::str_match(s,"(\\d+)([^\\d+])")
    v = as.numeric(parts[,2])
    v = v * ifelse(parts[,3]=="S" | parts[,3]=="W",-1,1)
    v
}

##' read header from trmm file
##'
##' This functions read a header from a trmm file so you can
##' then read the data into a raster or raster brick.
##' 
##' @title read trmm header
##' @param f path to trmm file
##' @return a trmmheader object
##' @author Barry Rowlingson
##' @export
get_header <- function(f){
    t = readChar(f,nchar=2880)
    t = stringr::str_trim(t)
    t = stringr::str_split(t[[1]]," ")
    t = stringr::str_split(t[[1]],"=")
    names = sapply(t, function(z){z[1]})
    L = lapply(t, function(z){z[2]})

    names(L)=names
    L$number_of_variables=as.numeric(L$number_of_variables)
    L$number_of_latitude_bins=as.numeric(L$number_of_latitude_bins)
    L$number_of_longitude_bins=as.numeric(L$number_of_longitude_bins)
    L$flag_value = as.numeric(L$flag_value)
    L$variable_type = splut(L$variable_type)
    L$variable_size = size_map(L$variable_type)
    L$variable_name = splut(L$variable_name)
    L$variable_scale = as.numeric(splut(L$variable_scale))
    L$variable_units = splut(L$variable_units)
    L$header_byte_length = as.numeric(L$header_byte_length)
    L$path = f
    class(L)="trmmheader"
    L
}

print.trmmheader <- function(x,...){
    cat("\ntrmm header\n")
    cat("\n")
    cat("Path:",x$path,"\n")
    cat("Size:",x$number_of_longitude_bins,"x",x$number_of_latitude_bins,"\n")
    cat("Variable names:",L$variable_name,"\n")
    cat("\n")
    cat("See names() for all header fields\n")
}
    
##' get a layer from a trmm using the header
##'
##' when you have a trmm header object, use this to get a single
##' layer into a raster object
##' @title read one layer from a trmm file
##' @param L trmmlayer object
##' @param layer layer variable name
##' @return raster of the layer
##' @author Barry Rowlingson
##' @export
get_layer <- function(L, layer){
    w = which(L$variable_name == layer)
    if(length(w)==0){
        stop("Layer ",layer," not found in ",L$path)
    }
    offset = c(0,cumsum(L$variable_size))[w]/8 * L$number_of_latitude_bins *
        L$number_of_longitude_bins + L$header_byte_length
    f = file(L$path,"rb")
    seek(f,where=offset)
    nbytes=L$variable_size[w]/8
    if(stringr::str_sub(L$byte_order,1,3)=="big"){
        endian="big"
    }else{
        endian="little"
    }
    
    z = readBin(f,
        "integer",
        n=L$number_of_latitude_bins * L$number_of_longitude_bins,
        size=nbytes,
        endian=endian,
        signed=TRUE
        )
    close(f)
    z[z==L$flag_value]=NA
    r = raster::raster(matrix(z,ncol=L$number_of_latitude_bins,
                  nrow=L$number_of_longitude_bins),
           xmn=compute_edge(L$west_boundary),
           xmx=compute_edge(L$east_boundary),
           ymn=compute_edge(L$south_boundary),
           ymx=compute_edge(L$north_boundary)
           )
    
    r = r/L$variable_scale[w]
    raster::projection(r)=sp::CRS("+init=epsg:4326")
    r
}

##' get all layers into a brick
##'
##' reads all layers into a brick, with names.
##' @title read all layers into a brick
##' @param L trmmheader object
##' @return raster brick
##' @author Barry Rowlingson
##' @export
get_brick <- function(L){
    b = do.call(
        raster::brick,lapply(L$variable_name,function(n){
                         get_layer(L,n)
                     }
                     )
        )
    names(b)=L$variable_name
    b
}
     
        
readconverted <- function(f,var=1){
    ## this reads and converts a table of i,j,p1,p2,p3...
    ## as output by the fortran code. Only useful for testing.
    m = read.table(f,head=TRUE)
    col=var+2 # skip i,j
    message("reading ",names(m)[col])
    ni = max(m$i)
    nj = max(m$j)
    ymx=nj/8
    ymn=-nj/8
    xmx=360
    xmn=0
    pm = matrix(m[,col], ni,nj)
    r = raster::raster(pm, xmn=xmn, xmx=xmx, ymn=ymn, ymx=ymx)
    projection(r)=CRS("+init=epsg:4326")
    r[r < -90000] <- NA
    r
}
