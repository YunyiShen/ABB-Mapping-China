library("raster")
library (rgdal)
map_binary <- raster("./5.FRAGSTAT/binary_0.39.tif")
IUCN_polygon <- rgdal::readOGR("./8.IUCN/ABB_IUCN.shp")
AOH <- raster("./9.AOH_compare/Ursus_thibetanus-AOH.tif")




AOH_project <- projectRaster(AOH, crs = map_binary@crs, method = "ngb")
AOH_res <- resample(AOH_project, map_binary, method = "ngb")
IUCN_project <- spTransform(IUCN_polygon, map_binary@crs)
IUCN_raster <- rasterize(IUCN_project, AOH_res)
IUCN_res <- resample(IUCN_raster, map_binary, method = "ngb") * (map_binary>=0)
IUCN_res <- mask(IUCN_res,IUCN_project)
writeRaster(IUCN_res,"./9.AOH_compare/IUCN.tif", overwrite = T)

IUCN_AOH <- (IUCN_res==1) * AOH_res
IUCN_AOH <-  mask(IUCN_AOH,IUCN_project)
plot(IUCN_AOH)
writeRaster(IUCN_AOH,"./9.AOH_compare/AOH.tif", overwrite = T)


1-46230/sum(getValues(IUCN_AOH),na.rm = T) # percentage of AOH
1-46230/sum(getValues((IUCN_res==1)),na.rm = T) # percentage of IUCN

1-sum(getValues(IUCN_AOH),na.rm = T)/sum(getValues((IUCN_res==1)),na.rm = T)

