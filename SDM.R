workdir <- "/1. 30km"
setwd(paste0(getwd(),workdir))
allPA <- read.csv(list.files(pattern = ".csv$"))

require(raster)
require(biomod2)

pathfac <- paste0(getwd(),'/rasters/dis')
pathcon <- paste0(getwd(),'/rasters/cts')

rasters_factor <- list.files(path = pathfac, full.names = T,pattern = ".tif$")
rasters_con <- list.files(path = pathcon, full.names = T,pattern = ".tif$")

stack1 <- stack(rasters_factor)
#stack1[[2]] = stack1[[2]] * (stack1[[1]]>-1000) #make 128 NAs
for(i in 1:1){
	stack1[[i]] <- asFactor(stack1[[i]])
	stack1[[i]] <- ratify(stack1[[i]])
}


stack2 <- stack(rasters_con)
my.stack <- stack(stack1,stack2)
#my.stack = my.stack[[-2]]

rm(stack1)
rm(stack2)

distributiondata.species1 <- 
  BIOMOD_FormatingData(
    resp.var = as.numeric(allPA[,5]),
    expl.var = my.stack,
    resp.xy = (allPA[, 3:4]),
    resp.name = 'bear'
  )


SDM.1 <-
  BIOMOD_Modeling(
    data = distributiondata.species1,
    models = 'RF',
    DataSplit=85,
    models.eval.meth = c('TSS', 'ROC') ,
    VarImport = 3,
    SaveObj = T
  )

distribution.est.1.full <- BIOMOD_Projection(SDM.1, my.stack, 'distribution1.est',build.clamping.mask = F,omit.na = F)
plot(distribution.est.1.full)
distribution.est.1 <- distribution.est.1.full@proj@val[[2]]
plot(distribution.est.1)
writeRaster(distribution.est.1,paste0((getwd()),"bear7_19.tif"),overwrite=T)
summary(SDM.1)

stack.all = stack(distribution.est.1,my.stack)

proj_env = data.frame( na.omit(getValues(stack.all)))
rm(stack.all)
env_name = colnames(proj_env)

for(i in 2:13){
  jpeg(filename=paste0(env_name[i],".jpg"))
  plot(proj_env[,i],proj_env[,1])
  dev.off()
}

