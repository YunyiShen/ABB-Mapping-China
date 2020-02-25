High = "./2. 3.16km/high"
Low = "./1. 30km/low"

Fusion = "./3. fusion/Fusion"


high_res = list.files(High,pattern = ".tif$",full.names = T)

low_res = list.files(Low,pattern = ".tif$",full.names = T)

fold = 10

for(i in 1:fold){
  low_temp = raster(low_res[i])
  high_temp = raster(high_res[i])
  
  low_temp = resample(low_temp,high_temp,method = "ngb")
  
  fusion_temp = ((low_temp/1000)*(high_temp/1000))/(((low_temp/1000)*(high_temp/1000))+((1-low_temp/1000)*(1-high_temp/1000)))
  
  jpeg(filename=paste0(Fusion,"/bear","_fold_",i,"fusion.jpg"))
  plot(fusion_temp)
  dev.off()
  
  writeRaster(fusion_temp,paste0(Fusion,"/bear","_fold_",i,"fusion.tif"),overwrite=T)
  
}


all_temp = stack(list.files(Fusion,pattern=".tif$",full.names = T))

mean_prod = mean(all_temp)

writeRaster(mean_prod,paste0(Fusion,"/bear_mean.tif"),overwrite=T)

jpeg(filename=paste0(Fusion,"/bear_mean_fusion.jpg"))
plot(mean_prod)
dev.off()



