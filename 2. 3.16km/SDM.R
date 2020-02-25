workdir <- "/2. 3.16km"
setwd(paste0(getwd(),workdir))
allPA <- read.csv(list.files(pattern = ".csv$")[2])

require(raster)
require(biomod2)
require(caret)

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
#my.stack = my.stack[[-3]]

set.seed(114514)
fold = 10
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
  writeRaster(distribution.est.1,paste0((getwd()),"/high/bear_1.12_fold_",i,".tif"),overwrite=T)
  #distribution.est.1 = raster(file.choose())
  
  prediction_on_test = extract(distribution.est.1,allPA[folds[[i]],3:4])/1000
  roc_test = roc(allPA[folds[[i]],"R"],prediction_on_test)
  
  res[[i]] = list(model_high = SDM.1,prediction_low = distribution.est.1,val = allPA[folds[[i]],],roc = roc_test)
  stack.all = stack(distribution.est.1,my.stack)
  
  proj_env = data.frame( na.omit(getValues(stack.all)))
  rm(stack.all)
  env_name = colnames(proj_env)
  
  for(j in 2:ncol(proj_env)){
    jpeg(filename=paste0(env_name[j],"_fold_",i,".jpg"))
    plot(proj_env[,j],proj_env[,1])
    dev.off()
  }
  write.csv(allPA[folds[[i]],],paste0("test_fold_",i,".csv"))
  
}

aucs = sapply(res,function(w){w$roc$auc})

average_pred = lapply(res,function(w){w$prediction_low})

average_pred = Reduce(mean,average_pred)


writeRaster(distribution.est.1,paste0((getwd()),"/high/bear_average.tif"),overwrite=T)





var_importance = lapply(res,function(w){slot(w$model_high@variables.importances,"val")})


var_importance_matrix = Reduce(rbind,var_importance)

colnames(var_importance_matrix) = c("PRO","ELE","RUG","POP","VEG")
row.names(var_importance_matrix) = paste0("fold_",1:fold)

write.csv(var_importance_matrix,"var_importance.csv")


require(ggplot2)

require(reshape2)

var_im = melt(var_importance_matrix,varnames = c("fold","predictor"))

ggplot(data = var_im,aes(x=predictor,y=value)) + 
  geom_boxplot()+   
  theme(text = element_text(size=14), 
        axis.text.x = element_text(angle=45, hjust=1,size = 12))

ggsave("importance.jpg",dpi=500)


