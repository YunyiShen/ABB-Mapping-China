library(ggplot2)
library(reshape2)


var_imp_30 <- read.csv("./7.make_figs/var_imp/RF30.csv")
var_imp_3 <- read.csv("./7.make_figs/var_imp/RF3.csv")

summary_stat_30 <- data.frame( variable = colnames(var_imp_30),
                               mean =  colMeans(var_imp_30), 
                               sd = apply(var_imp_30, 2, sd),
                               resolution = "Coarse")

summary_stat_3 <- data.frame( variable = colnames(var_imp_3),
                               mean =  colMeans(var_imp_3), 
                               sd = apply(var_imp_3, 2, sd),
                               resolution = "Fine")

plot_data <- rbind(summary_stat_30, summary_stat_3)


# imp_long_30 <- melt(var_imp_30)
# imp_long_30$Resolution <- "Coarse"
# 
# imp_long_3 <- melt(var_imp_3)
# imp_long_3$Resolution <- "Coarse"
# 
# plot_data <- rbind(imp_long_3,imp_long_30)
levels_env <- c("BIO2","RUGG","BIO15","BIO12","BIO3","COVER", "BIO1", "POPU","ELEV", "BIO4","PROT"  )
plot_data$variable <- factor(plot_data$variable, levels = rev(levels_env))

ggplot(data = plot_data, aes(x=variable, y = mean))+
  facet_grid(~ resolution, scales = "free") + 
  geom_bar(stat="identity") + 
  geom_errorbar( aes(ymin=mean-sd, ymax=mean+sd), 
                 width=0.4,  alpha=0.9, size=1.3)+
  ylab("Relative importance") + 
  xlab("Predictor") + 
  theme(text = element_text(size=14,family = "Times New Roman"), 
        plot.margin = margin(.15, .15, .15, .15, "cm"))+
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    legend.background = element_rect(fill = "transparent")#, # get rid of legend bg
    #legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )+
  coord_flip()

ggsave("./7.make_figs/pred_imp.pdf",width = 8, height = 3.5)
ggsave("./7.make_figs/pred_imp.jpg",width = 8, height = 3.5,dpi = 500)

