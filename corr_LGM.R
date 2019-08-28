rasterdir = 'F:/Pheasant Maxent/2.rasters/LGM only Climate and topo'
setwd(rasterdir)
# rastersfile = list.files(pattern = 'bio')
# cvar = data.frame(matrix(nrow = 19,ncol = 20))
# cvar[,1] = rastersfile
# require(raster)
# 
# for (i in 1:18){
#   temp1 = raster(rastersfile[i])
#   for(j in ((i+1):19)){
#     temp2 = raster(rastersfile[j])
#     cvar[i,j] = cor(na.omit(getValues(temp1)),na.omit(getValues(temp2)))
#     print(paste('calculating correlation of',names(temp1),'and',names(temp2)))
#   }
# }
# 
# write.csv(cvar,'correlation_LGM.csv',row.names = F)
cvar = read.csv('correlation_LGM.csv')
absvar = (cvar)
absvar[,3:20] = abs(absvar[,3:20])
deletevar = absvar
deletevar[,3:20] = absvar[,3:20]>=0.7

corrmatrix = deletevar[,2:20]
layers = deletevar[,1]
still = T

while(still){
  numdel = matrix(1,nrow(corrmatrix))
  for(i in 1:nrow(corrmatrix)){
    numdel[i] = sum(na.omit (as.numeric( (corrmatrix[i,]))))+
      sum(na.omit(as.numeric( (corrmatrix[,i]))))
  }
  if(sum(numdel)==0){break}
  vartodelete = which(numdel == max(numdel))[1]
  print(corrmatrix)
  print(as.character(layers[vartodelete]))
  print(numdel)
  layers = layers[-vartodelete]
  corrmatrix = corrmatrix[-vartodelete,]
  corrmatrix = corrmatrix[,-vartodelete]
  
  
  
}
write.csv(as.character(layers),'layers.notre.csv')







