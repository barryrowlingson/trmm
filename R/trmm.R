##' trmm
##' 
##' Read data from NASA TRMM data files into raster structures. 
##' 
##' \tabular{ll}{
##' Package: \tab trmm\cr
##' Version: \tab 0.1\cr
##' Date: \tab 2015-06-18\cr
##' License: \tab MIT \cr
##' }
##' 
##'  
##' 
##' section{Dependencies}{
##' The package \code{trmm} depends upon some other important contributions to CRAN in order to operate; their uses here are indicated:\cr\cr
##'     sp, raster, stringr, lubridate, ncdf4.
##' }
##' 
##' section{Citation}{
##' X
##' }
##' 
##' references{
##' X
##' }
##' 
##' @docType package
##' @name trmm-package
##' @author Barry Rowlingson, Department of Medicine, Lancaster University,
##'  Benjamin Taylor, Department of Medicine, Lancaster University
##' @keywords package
##'
##'

# note the below set of imports does not get rid of all warnings on R CMD check
## @import sp
## @import raster
## @import stringr
## @import lubridate
## @import ncdf4

# instead import functions individually
##' @importFrom sp bbox proj4string<- proj4string SpatialPixelsDataFrame SpatialGridDataFrame Polygon Polygons SpatialPolygons coordinates CRS geometry GridTopology over proj4string SpatialGrid SpatialPixels SpatialPoints SpatialPolygonsDataFrame split spTransform 
##' @importFrom raster raster crop
##' @importFrom stringr str_split str_match str_trim
##' @importFrom lubridate ymd year month week day
##' @importFrom ncdf4 nc_open nc_close nc_sync ncvar_get ncdim_def ncvar_def nc_create ncvar_put


### @importFrom raster raster crop
### @importFrom sp bbox proj4string<- proj4string SpatialPixelsDataFrame SpatialGridDataFrame Polygon Polygons SpatialPolygons coordinates CRS geometry GridTopology over proj4string SpatialGrid SpatialPixels SpatialPoints SpatialPolygonsDataFrame split spTransform 
### @importFrom RColorBrewer brewer.pal 
### @importFrom stringr str_count str_detect
### @importFrom Matrix Matrix sparseMatrix
### @importFrom rgl abclines3d aspect3d axes3d planes3d points3d segments3d text3d title3d 
### @importFrom fields image.plot  
### @importFrom RandomFields CovarianceFct
### @importFrom rgeos gBuffer
### @importFrom iterators icount iter nextElem
### @importFrom sp bbox proj4string<- proj4string SpatialPixelsDataFrame SpatialGridDataFrame Polygon Polygons SpatialPolygons coordinates CRS geometry GridTopology over proj4string SpatialGrid SpatialPixels SpatialPoints SpatialPolygonsDataFrame split spTransform 
### @importFrom spatstat rpoint progressreport
### @importFrom survival Surv survfit
### @importFrom geostatsp asImRaster
### @importFrom raster crop
### @importFrom stats acf coefficients deriv dexp dist dnorm end fft fitted formula Gamma integrate knots lm model.matrix optim optimise poly quantile rbinom rexp rnorm runif sd start update var 
### @importFrom graphics hist legend lines matplot par plot points title 






`trmm` = NA


