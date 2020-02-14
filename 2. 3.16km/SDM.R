#workdir <- "/1. 30km"
#setwd(paste0(getwd(),workdir))
allPA <- read.csv(list.files(pattern = ".csv$"))

require(raster)
require(biomod2)
require(caret)

pathfac <- paste0(getwd(),'/rasters/dis')
pathcon <- paste0(getwd(),'/rasters/cts')

rasters_factor <- list.files(path = pathfac, full.names = T)
rasters_con <- list.files(path = pathcon, full.names = T,pattern = ".tif$")

stack1 <- stack(rasters_factor)
stack1[[2]] = stack1[[2]] * (stack1[[1]]>-1000) #make 128 NAs
for(i in 1:2){
	stack1[[i]] <- asFactor(stack1[[i]])
	stack1[[i]] <- ratify(stack1[[i]])
}


stack2 <- stack(rasters_con)
my.stack <- stack(stack1,stack2)
my.stack = my.stack[[-2]]
#my.stack = my.stack[[-3]]

set.seed(114514)
rm(stack1)
rm(stack2)
fold = 5
folds = createFolds(y=allPA[,5],k=fold) # create a fold
res = list()

for(i in 1:fold){
	distributiondata.species1 <- 
  		BIOMOD_FormatingData(
    		resp.var = as.numeric(allPA[-folds[[i]],5]),
    		expl.var = my.stack,
    		resp.xy = (allPA[-folds[[i]],3:4]),
    		resp.name = paste0("bear",i)
  		)


		SDM.1 <-
  			BIOMOD_Modeling(
    		data = distributiondata.species1,
    		models = 'RF',
    		DataSplit=1,
    		models.eval.meth = c('TSS', 'ROC') ,
    		VarImport = 3,
    		SaveObj = T
  		)

		distribution.est.1.full <- BIOMOD_Projection(SDM.1, my.stack, 'distribution1.est',build.clamping.mask = F,omit.na = F)
		#plot(distribution.est.1.full)
		distribution.est.1 <- distribution.est.1.full@proj@val[[2]]
		#plot(distribution.est.1)
		writeRaster(distribution.est.1,paste0((getwd()),"/high/bear_1.12_fold_",i,".tif"),overwrite=T)
		#distribution.est.1 = raster(file.choose())
		LR = raster(paste0(getwd()),"/low/bear_1.12_fold",i,".tif"))
		L2R = resample(LR,distribution.est.1)

		#writeRaster(L2R,file.choose())

		combined = ((distribution.est.1/1000)*(L2R/1000))/((1-distribution.est.1/1000)*(1-L2R/1000)+(distribution.est.1/1000)*(L2R/1000))
		writeRaster(combined,paste0(getwd()),"/combine/bear_1.12_fold",i,".tif"),overwrite=T)
		write.csv(allPA[folds[[i]],],paste0("./high/val_bear_1.12_fold_",i,".csv"))
		res[[i]] = list(model_high = SDM.1,prediction_high = distribution.est.1,val = allPA[folds[[i]],],fusion = combined)
	
	
}

