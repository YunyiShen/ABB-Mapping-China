extract_imp <- function(w){
  w$model_high@variables.importances@val[,,1,1]
}


load("./2. 3.16km/10_fold.RData")

var_imp <- lapply(res,extract_imp)
var_imp <- Reduce(rbind,var_imp)
var_imp <- var_imp[,rev(order(colMeans(var_imp)))]

var_order <- read.csv("./7.make_figs/var_imp/order.csv", row.names = 1, stringsAsFactors = F)
var_imp <- var_imp[,var_order$x[var_order$x %in% colnames(var_imp)]]


#write.csv(colnames(var_imp),"./7.make_figs/var_imp/order.csv")
# for RF30
#colnames(var_imp) <- c("BIO2","RUGG","BIO15","BIO12","BIO3","COVER", "BIO1", "POPU","ELEV", "BIO4","PROT"  )


colnames(var_imp) <- c("RUGG","COVER","POPU","ELEV","PROT")
write.csv(var_imp,"./7.make_figs/var_imp/RF3.csv",row.names = F)
