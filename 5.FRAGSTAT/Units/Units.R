masks = list.files("./Units/mask",pattern = "tif$",full.names = T)

binary_map = raster("binary_0.39.tif")


for(mask in masks){
  thismask = raster(mask)
  temp = resample(binary_map,thismask,method = "ngb") * (thismask>=0)
  
  
  writeRaster(temp,sub("/mask","",mask),overwrite = T)
  
}
