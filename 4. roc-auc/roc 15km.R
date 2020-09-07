require(pROC)
require(raster)


rocdata=read.csv('./data/ROCfinal15km.csv')
rocdata = rocdata[rocdata$Field4!="Sichuan",]


prediction_low=raster("./bear_mean_coarse.tif")
prediction_high=raster("./bear_mean_fusion.tif")

prediction_low_resample = resample(prediction_low/1000,prediction_high,method = "ngb")


roc_data_points = SpatialPointsDataFrame(rocdata[,1:2],data = rocdata,proj4string = crs(prediction_low))

resoponse_low=extract(y=rocdata[,1:2],x=prediction_low_resample,buffer = rocdata[,5]*1000,fun=max)
roc_low=roc(rocdata[,3],resoponse_low)
plot(roc_low)
#points(rocdata[,1:2])

resoponse_high=extract(y=rocdata[,1:2],x=prediction_high,buffer = rocdata[,5]*1000,fun=max)
roc_high=roc(rocdata[,3],resoponse_high)
plot(roc_high)


prediction_train_at_high = raster("bear_mean_trainathigh.tif")
resoponse_train_at_high=extract(y=rocdata[,1:2],x=prediction_train_at_high,buffer = rocdata[,5]*1000,fun=max)
roc_train_at_high=roc(rocdata[,3],resoponse_train_at_high)
plot(roc_train_at_high)

