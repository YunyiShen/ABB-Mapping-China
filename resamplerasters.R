workwith = "dis"

require(raster)
ori = "./zz. origin rasters/"

allras = list.files(paste0(ori,workwith),full.names = T)
rasname = list.files(paste0(ori,workwith))

h_root = "./2. 3.16km/rasters/"
l_root = "./1. 30km/rasters/"

teph = raster(paste0(h_root,"cts/tep.tif"))
tepl = raster(paste0(l_root,"cts/tep.tif"))
res(tepl) = 30000
tepl = resample(teph,tepl)

for(i in 1:length(allras)){ # work on high first
  temp = raster(allras[i])
  
  resample(temp,teph,method = "ngb",filename = paste0(h_root,workwith,"/",rasname[i]))
  print(i)
}

for(i in 1:length(allras)){ # work on low first
  temp = raster(allras[i])
  
  resample(temp,tepl,method = "ngb",filename = paste0(l_root,workwith,"/",rasname[i]))
  print(i)
}
