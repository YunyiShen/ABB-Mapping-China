require(raster)
require(pROC)
Low = "./1. 30km/low"
#Low = "./1. 3.16km/low"

Fusion = "./3. fusion/Fusion"


high_res = list.files(Fusion,pattern = "fold.+?tif$",full.names = T)
high_val = list.files("./2. 3.16km",pattern = "test_fold.+?csv$",full.names = T)

low_res = list.files(Low,pattern = "fold.+?tif$",full.names = T)
low_val = list.files("./1. 30km",pattern = "test_fold.+?csv$",full.names = T)



fold = 10

res = list()
for(i in 1:fold){
  low_temp = raster(low_res[i])
  high_temp = raster(high_res[i])
  
  low_val_temp = read.csv(low_val[i])
  high_val_temp = read.csv(high_val[i])
  
  response_low_val_at_low = extract(low_temp,low_val_temp[,c(4,5)])
  true_low_val_at_low = low_val_temp[,6]
  
  response_high_val_at_low = extract(low_temp,high_val_temp[,c(4,5)])
  true_high_val_at_low = high_val_temp[,6]
  
  
  
  roc_low = roc(c(true_low_val_at_low,true_high_val_at_low),
                c(response_low_val_at_low,response_high_val_at_low))
  
  
  
  response_low_val_at_high = extract(high_temp,low_val_temp[,c(4,5)]
                                     ,buffer = 15000
                                     ,fun=max,na.rn=T)
  true_low_val_at_high = low_val_temp[,6]
  
  response_high_val_at_high = extract(high_temp,high_val_temp[,c(4,5)])
  
  true_high_val_at_high = high_val_temp[,6]
  
  roc_high = roc(c(true_low_val_at_high,true_high_val_at_high),
                c(response_low_val_at_high,response_high_val_at_high))
  res[[i]] = list(roc_high = roc_high,roc_low = roc_low)

}


aucs_high = sapply(res,function(w){w$roc_high$auc})
aucs_low = sapply(res,function(w){w$roc_low$auc})





# high only 
fold = 10

res_high_only = list()
for(i in 1:fold){
  low_temp = raster(low_res[i])
  high_temp = raster(high_res[i])
  
  low_val_temp = read.csv(low_val[i])
  high_val_temp = read.csv(high_val[i])
  
  response_low_val_at_low = extract(low_temp,low_val_temp[,c(4,5)])
  true_low_val_at_low = low_val_temp[,6]
  
  response_high_val_at_low = extract(low_temp,high_val_temp[,c(4,5)])
  true_high_val_at_low = high_val_temp[,6]
  
  
  
  roc_low = roc(c(true_high_val_at_low),
                c(response_high_val_at_low))
  
  
  
  response_low_val_at_high = extract(high_temp,low_val_temp[,c(4,5)]
                                     ,buffer = 15000
                                     ,fun=max,na.rn=T)
  true_low_val_at_high = low_val_temp[,6]
  
  response_high_val_at_high = extract(high_temp,high_val_temp[,c(4,5)])
  
  true_high_val_at_high = high_val_temp[,6]
  
  roc_high = roc(c(true_high_val_at_high),
                 c(response_high_val_at_high))
  res_high_only[[i]] = list(roc_high = roc_high,roc_low = roc_low)
  
}


aucs_high_high_only = sapply(res_high_only,function(w){w$roc_high$auc})
aucs_low_high_only = sapply(res_high_only,function(w){w$roc_low$auc})






