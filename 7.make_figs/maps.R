library("rasterVis")
library("raster")
library(ggsn)
library(ggplot2)
library (rgdal)
library (rgeos)
library(maptools)
library(tmap)
library(extrafont)
font_import()
loadfonts()

map_30 <- raster("./1. 30km/res_map/bear_average.tif")
map_10 <- raster("./3. fusion/Fusion/bear_mean.tif")
map_30_res <- resample(map_30,map_10,"ngb")

map_stack <- stack(map_30_res,map_10)
names(map_stack) <- c("coarse","fine")

gplot(map_stack) +
  geom_tile(aes(fill = value)) +
  facet_wrap(~ variable) +
  scale_fill_gradient(low = "#0072B2",
                      high =  "#E7B800",
                      na.value="transparent") +
  labs(fill = "Probability", x = "", y="") +
  theme(text = element_text(size=14,family = "Times New Roman"), 
        axis.text.x = element_text(angle=-45,size = 12,vjust=1,hjust = 0),
        axis.text.y = element_text(angle=0,size = 12),
        plot.margin = margin(.15, .15, .15, .15, "cm"))+
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    legend.background = element_rect(fill = "transparent")#, # get rid of legend bg
    #legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )+
  ggsn::scalebar(x.min = -2638293 , x.max = 1906197 , 
                 y.min = 665715.9 , y.max = 5921292, 
                 dist = 1000,dist_unit = "km", transform = F,
                 location = "bottomright",border.size = .1, st.dist = .05,
                 family = "Times New Roman")+
  ggsn::north(x.min = -2638293 , x.max = 2206197 , 
              y.min = 665715.9 , y.max = 5921292,
              location = "topleft",scale = .1, symbol = 15)+
  coord_equal()

  
ggsave("./7.make_figs/prob_maps.tiff",width = 10, height = 5.5,dpi = 500)
ggsave("./7.make_figs/prob_maps.jpg",width = 10, height = 5.5,dpi = 600)



binary_map <- raster("./3. fusion/Fusion/binary_0.39.tif")
binary_map <- ratify( asFactor(binary_map))

management_units <- list.files("./4.5_Management_units",full.names =T,pattern = ".shp$")
all_polygon <- lapply(management_units,rgdal::readOGR)


management_units_df <- lapply(all_polygon, fortify)
management_units_df_comb <- Reduce("rbind",management_units_df)


g <- gplot(binary_map) + 
  geom_tile(aes(fill = value)) + 
  scale_fill_gradient(high = "#0072B2",
                      low =  "lightgrey",
                      na.value="transparent") + 
  guides(fill=FALSE)+
  labs(x = "", y="") +
  theme(text = element_text(size=14,family = "Times New Roman"), 
        axis.text.x = element_text(angle=-45,size = 12,vjust=1,hjust = 0),
        axis.text.y = element_text(angle=0,size = 12),
        plot.margin = margin(.15, .15, .15, .15, "cm"))+
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    legend.background = element_rect(fill = "transparent")#, # get rid of legend bg
    #legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )+
  ggsn::scalebar(x.min = -2638293 , x.max = 1906197 , 
                 y.min = 665715.9 , y.max = 5921292, 
                 dist = 1000,dist_unit = "km", transform = F,
                 location = "bottomright",border.size = .1, st.dist = .05,family = "Times New Roman")+
  ggsn::north(x.min = -2638293 , x.max = 2206197 , 
              y.min = 665715.9 , y.max = 5921292,
              location = "topleft",scale = .1, symbol = 15)

for(i in 1:8){
  g <- g + geom_polygon(data=management_units_df[[i]], aes(long, lat, group=id),color = "black",fill = NA)
}  

g + annotate(geom="text", x=1.78e6, y=2.55e6, label="1",
             color="black",family = "Times New Roman")+
  annotate(geom="text", x=.5e6, y=1.7e6, label="2",
             color="black",family = "Times New Roman") + 
  annotate(geom="text", x=1.65e6, y=4.7e6, label="3",
           color="black",family = "Times New Roman")  +
  annotate(geom="text", x=1.2e6, y=2.75e6, label="4",
           color="black",family = "Times New Roman") +   
  annotate(geom="text", x=.2e6, y=2.75e6, label="5",
           color="black",family = "Times New Roman") +
  annotate(geom="text", x=.25e6, y=3.4e6, label="6",
           color="black",family = "Times New Roman") +
  annotate(geom="text", x=0e6, y=3.25e6, label="7",
           color="black",family = "Times New Roman") +
  annotate(geom="text", x=-.8e6, y=3.2e6, label="8",
           color="black",family = "Times New Roman")

ggsave("./7.make_figs/unit_maps.tiff",width = 6, height = 5,dpi = 500)
ggsave("./7.make_figs/unit_maps.jpg",width = 6, height = 5,dpi = 600)
