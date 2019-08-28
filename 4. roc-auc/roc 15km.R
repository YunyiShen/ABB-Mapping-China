require(pROC)
require(raster)


rocdata=read.csv('./data/ROCfinal15km.csv')


prediction=raster(file.choose())

resL=prediction
res(resL)=15000

predictH2L=resample(prediction,resL)


resoponse=extract(y=rocdata[,1:2],x=predictH2L)
rocf=roc(rocdata[,3],resoponse)
plot(rocf)

predictionL=raster(file.choose())
predictionL=resample(predictionL,resL)
resoponseL=extract(y=rocdata[,1:2],x=predictionL)
rocL=roc(rocdata[,3],resoponseL)
plot(rocL)

