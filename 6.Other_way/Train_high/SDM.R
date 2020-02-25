
allPA <- read.csv(list.files(pattern = ".csv$"))



require(raster)
require(biomod2)
require(caret)
require(pROC)
fold = 5
folds = createFolds(y=allPA[,"P.A"]
                       ,k=fold) # create a fold



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


set.seed(42)
fold = 10
folds_low = createFolds(y=allPA[allPA$Res==0,3],k=fold) # create a fold
set.seed(42)
folds_high = createFolds(y=allPA[allPA$Res==1,3],k=fold )

res = list()
folds = list()

for(i in 1:fold){
  folds[[i]] = c(folds_low[[i]],folds_high[[i]])
  
  
	distributiondata.species1 <- 
  		BIOMOD_FormatingData(
    		resp.var = as.numeric(allPA[-folds[[i]],3]),
    		expl.var = my.stack,
    		resp.xy = (allPA[-folds[[i]],1:2]),
    		resp.name = paste0("bear",i)
  		)


		SDM.1 <-
  			BIOMOD_Modeling(
    		data = distributiondata.species1,
    		models = 'RF',
    		DataSplit=100,
    		models.eval.meth = c('TSS', 'ROC') ,
    		VarImport = 3,
    		SaveObj = T
  		)

		distribution.est.1.full <- BIOMOD_Projection(SDM.1, my.stack, paste0( 'distribution',i,'.est'),build.clamping.mask = F,omit.na = F)
		#plot(distribution.est.1.full)
		distribution.est.1 <- distribution.est.1.full@proj@val[[1]]
		#plot(distribution.est.1)
		jpeg(filename=paste0("bear","_fold_",i,".jpg"))
		plot(distribution.est.1)
		dev.off()
		writeRaster(distribution.est.1,paste0((getwd()),"/bear_fold_",i,".tif"),overwrite=T)
		#distribution.est.1 = raster(file.choose())
		
		prediction_on_test = extract(distribution.est.1,allPA[folds[[i]],1:2])/1000
		roc_test = roc(allPA[folds[[i]],"R"],prediction_on_test)
		
		res[[i]] = list(model_high = SDM.1,prediction_low = distribution.est.1,val = allPA[folds[[i]],],roc = roc_test)
		stack.all = stack(distribution.est.1,my.stack)

		proj_env = data.frame( na.omit(getValues(stack.all)))
		rm(stack.all)
		env_name = colnames(proj_env)

	  write.csv(allPA[folds[[i]],],paste0("test_fold_",i,".csv"))
	
}

aucs = sapply(res,function(w){w$roc$auc})

average_pred = lapply(res,function(w){w$prediction_low})

average_pred = Reduce(mean,average_pred)


writeRaster(distribution.est.1,paste0((getwd()),"/bear_average.tif"),overwrite=T)

