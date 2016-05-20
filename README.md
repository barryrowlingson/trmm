NASA trmm data files are a high spatial and temporal
resolution rainfall data product. Although I think
they may be available as HDF/NetCDF formats from
another source, the original source seems to be 
here:

ftp://trmmopen.gsfc.nasa.gov/pub/merged/

That FTP site also includes some C and Fortran code
that needs a bit of a massage to make work. There's
issues with endianness and compiler limitations and
stack overflows.

So here's an R package. Here's how it works.

 * install the package

 * Read the header info from wherever you saved the file:

     H = get_header("../Data/3B41RT.2014033100.7.bin")

 * Get a layer from the file as a raster object, and plot:

     prec = get_layer(H,"precipitation")
     plot(prec)

 * The metadata is available from `H`, for example:

     > H$variable_name
     [1] "precipitation"       "precipitation_error" "total_pixels"       

 * You can read all layers into a raster brick:

      data = get_brick(H)
      plot(data)


[![Travis-CI Build Status](https://travis-ci.org/barryrowlingson/trmm.svg?branch=master)](https://travis-ci.org/barryrowlingson/trmm)

[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/barryrowlingson/trmm?branch=master&svg=true)](https://ci.appveyor.com/project/barryrowlingson/trmm)
