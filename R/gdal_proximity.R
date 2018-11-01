#' gdal_rasterize
#' 
#' R wrapper for gdal_proximity: produces a raster proximity map.
#' 
#' @param src_datasource Character. The source raster file used to identify target pixels.
#' @param dst_filename Character. The destination raster file to which the proximity map will be written. It may be a pre-existing file of the same size as srcfile. If it does not exist it will be created.
#' @param srcband Numeric.Identifies the band in the source file to use (default is 1).
#' @param dstband Numeric. Identifies the band in the destination file to use (default is 1).
#' @param of Character. Select the output format. Starting with GDAL 2.3, if not specified, the format is guessed from the extension (previously was GTiff). Use the short format name.
#' @param co Passes a creation options to the output format driver. See GDAL's format specific documentation for legal creation options for each format. 
#' @param ot Character. Force the output image bands to have a specific type. Use type names (i.e. Byte, Int16,...). 
#' @param values Numeric. A vector of target pixel values in the source image to be considered target pixels. If not specified, all non-zero pixels will be considered target pixels.  
#' @param distunits Character. "pixel" indicates that distances should be in pixel coordinates. "geo" indicates geographic coordinates.
#' @param maxdist Numeric. The maximum distance to be generated. The nodata value will be used for pixels beyond this distance. If a nodata value is not provided, the output band will be queried for its nodata value. If the output band does not have a nodata value, then the value 65535 will be used. Distance is interpreted in pixels unless -distunits GEO is specified. 
#' @param nodata  Numeric. Specify a nodata value to use for the destination proximity raster.
#' @param use_input_nodata  Logical. (GDAL >= 2.0) Indicate whether nodata pixels in the input raster should be nodata in the output raster (default FALSE).
#' @param fixed_buf_val Numeric. Specify a value to be applied to all pixels that are within the -maxdist of target pixels (including the target pixels) instead of a distance value.
#' @param output_Raster Logical. If TRUE, returns the resulting raster as a RasterBrick.
#' @return NULL or if(output_Raster), a RasterBrick.
#' @author Marcio HR Sales (\email{marciosales@@outlook.com}) (wrapper) and Frank Warmerdam (GDAL lead developer).
#' @details This is an R wrapper for the 'gdal_proximity.py' function that is part of the 
#' Geospatial Data Abstraction Library (GDAL).  It follows the parameter naming
#' conventions of the original function, with some modifications to allow for more R-like
#' parameters.  For all parameters, the user can use a single character string following,
#' precisely, the gdalwarp format (\url{http://www.gdal.org/gdal_rasterize.html}), or,
#' in some cases, can use R vectors to achieve the same end.  
#' 
#' This function assumes the user has a working GDAL on their system.  If the 
#' "gdalUtils_gdalPath" option has been set (usually by gdal_setInstallation),
#' the GDAL found in that path will be used.  If nothing is found, gdal_setInstallation
#' will be executed to attempt to find a working GDAL that has the right drivers 
#' as specified with the "of" (output format) parameter.
#' 
#' The user can choose to (optionally) return a RasterBrick of the output file (assuming
#' raster/rgdal supports the particular output format).
#'
#' @references \url{http://www.gdal.org/gdal_proximity.html}
#' @examples 
#' # We'll pre-check to make sure there is a valid GDAL install
#' # and that raster and rgdal are also installed.
#' # Note this isn't strictly neccessary, as executing the function will
#' # force a search for a valid GDAL install.
#' gdal_setInstallation()
#' valid_install <- !is.null(getOption("gdalUtils_gdalPath"))
#' if(require(raster) && require(rgdal) && valid_install)
#' {
#' src_dataset_original <- system.file("external/tahoe_highrez.tif", package="gdalUtils")
#' src_filename <- paste(tempfile(),".tif",sep="") # will save a modified raster here for later use
#' dst_filename <- paste(tempfile(),".tif",sep="") # this is the destination file

#' # creates a raster file of 0's and 1's.
#' rst = raster(src_dataset_original)
#' writeRaster(rst==0, src_filename)
#' rst = raster(src_filename)
#' plot(rst)
#' # Obtains a raster distances from all 0's to nearest 1 in pixel coordinates 
#' tahoe_distance <- gdal_proximity(src_filename,dst_filename,
#'                                  srcband=1,dstband=1, of = "GTiff", values=1,distunits="pixel",
#'                                  verbose=TRUE,output_Raster=TRUE)
#' # plot results
#' plot(tahoe_distance)
#' # cleanup
#' file.remove(src_filename)
#' file.remove(dst_filename)
#' }
#' @import rgdal
#' @export

gdal_proximity = function(src_datasource,dst_filename, srcband, dstband, of ,co,ot, 
                          values,distunits, maxdist, nodata, use_input_nodata, 
                          fixed_buf_val,
                          # other
                          output_Raster=FALSE,
                          ignore.full_scan=TRUE,
                          verbose=FALSE) {
  if(output_Raster && (!requireNamespace("raster") || !requireNamespace("rgdal")))
  {
    warning("rgdal and/or raster not installed. Please install.packages(c('rgdal','raster')) or set output_Raster=FALSE")
    return(NULL)
  }
  
  parameter_values <- as.list(environment())
  
  if(verbose) message("Checking gdal_installation...")
  gdal_setInstallation(ignore.full_scan=ignore.full_scan)
  if(is.null(getOption("gdalUtils_gdalPath"))) stop("no gdal")
  
  # Place all gdal function variables into these groupings:
  parameter_variables <- list(
    logical = list(
      varnames <- c(
        "use_input_nodata" 
      )),
    vector = list(
      varnames <- c(
        "values"
      )),
    scalar = list(
      varnames <- c(
        "srcband", "dstband", "maxdist", "nodata", "fixed-buf-val"
      )),
    character = list(
      varnames <- c(
        "src_datasource","dst_filename", "of", "ot", "distunits" 
      )),
    repeatable = list(
      varnames <- c(
        "co"
      ))
  )
  
  parameter_order <- c(
    "src_datasource", "dst_filename", "srcband", "dstband", "of", "co", "ot",
    "values","distunits", "maxdist", "nodata", "use_input_nodata",
    "fixed-buf-val"
  )
  
  parameter_noflags <- c("src_datasource","dst_filename")
  
  parameter_noquotes <- unlist(parameter_variables$vector)
  
  executable <- "gdal_proximity.bat"
  
  cmd <- gdal_cmd_builder(
    executable=executable,
    parameter_variables=parameter_variables,
    parameter_values=parameter_values,
    parameter_order=parameter_order,
    parameter_noflags=parameter_noflags,
    parameter_noquotes=parameter_noquotes,
    gdal_installation_id=gdal_chooseInstallation(hasDrivers=of))
  
  if(verbose) message(paste("GDAL command being used:",cmd))
  
  if (Sys.which(c("cmd"))["cmd"]!="") {
    cmd_output <- system("cmd.exe", input = cmd, intern=TRUE) 
  } else {
    cmd_output <- system(cmd, intern=TRUE)   
  }
  
  if(output_Raster)
  {
    return(brick(dst_filename))	
  } else
  {
    return(cmd_output)
  }		
}