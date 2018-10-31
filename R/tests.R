# src_dataset_original <- system.file("external/tahoe_highrez.tif", package="gdalUtils")
# src_filename <- paste(tempfile(),".tif",sep="") # will save a modified raster here for later use
# dst_filename <- paste(tempfile(),".tif",sep="") # this is the destination file
# 
# # creates a raster file of 0's and 1's.
# rst = raster(src_dataset_original)
# writeRaster(rst==0, src_filename)
# rst = raster(src_filename)
# plot(rst)
# 
# # Obtains a raster distances from all 0's to nearest 1 in pixel coordinates 
# tahoe_distance <- gdal_proximity(src_filename,dst_filename,
#                                srcband=1,dstband=1, of = "GTiff", values=1,distunits="pixel",
#                                verbose=TRUE,output_Raster=TRUE)
# # plot results
# plot(raster(dst_filename))
# # cleanup
# file.remove(src_filename)
# file.remove(dst_filename)