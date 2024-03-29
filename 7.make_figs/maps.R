library("rasterVis")
library("raster")
library(ggsn)
library(ggplot2)
library (rgdal)
library (rgeos)
library(maptools)
library(tmap)
library(extrafont)
library(ggnewscale)
font_import()
y
loadfonts()

t_col <- function(color, percent = 0, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent)*255  / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
  t.col
}



map_30 <- raster("./1. 30km/res_map/bear_average.tif")
map_10 <- raster("./3. fusion/Fusion/bear_mean.tif")
map_30_res <- resample(map_30,map_10,"ngb")

namesss <- c("Extend","Possible","")
IUCN_polygon <- rgdal::readOGR("./8.IUCN/ABB_IUCN.shp")
IUCN_polygon_tras <- spTransform(IUCN_polygon,map_10@crs)
IUCN_polygon_fort <- fortify(IUCN_polygon_tras)
IUCN_polygon_fort$id[IUCN_polygon_fort$hole] <- 2
IUCN_polygon_fort$id <- as.numeric(IUCN_polygon_fort$id) + 1
IUCN_polygon_fort$IUCN <- factor(namesss[IUCN_polygon_fort$id],levels = namesss)





map_stack <- stack(map_30_res,map_10)
names(map_stack) <- c("coarse","integrated")

# this will take sichuan out from the polygon
Sichuan <- !(IUCN_polygon_fort$IUCN=="Extend" & !IUCN_polygon_fort$hole) & IUCN_polygon_fort$IUCN!="Possible"

gplot(map_stack) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradient(low = "#0072B2",
                      high =  "#E7B800",
                      #low="gray",
                      na.value="transparent") +
  labs(fill = "Probability", x = "", y="") +
  new_scale("fill") +
  geom_polygon(data=IUCN_polygon_fort[!Sichuan,], 
               aes(long, lat, group=group,subgroup = factor(hole),fill = IUCN)) +
  scale_fill_manual(values = c(#t_col("#E69F00",80),
                                t_col("red", 70),
                               #t_col("#009E73", 80), 
                               t_col("seagreen2", 70),
                               t_col("white", 100)
                               #"white"
                               #,adjustcolor("white",alpha.f = 255)
                               )) + 
  labs(fill = "IUCN", x = "", y="") +
  facet_wrap(~ variable) +
  theme(text = element_text(size=14,family = "Times New Roman"), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
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
ggsave("./7.make_figs/prob_maps.jpg",width = 10, height = 5.5,dpi = 800)

ggsave("./7.make_figs/prob_maps_IUCN.tiff",width = 12, height = 5.5,dpi = 500)
ggsave("./7.make_figs/prob_maps_IUCN.jpg",width = 12, height = 5.5,dpi = 800)


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
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(.15, .15, .15, .15, "cm"))+
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    legend.background = element_rect(fill = "transparent")#, # get rid of legend bg
    #legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )+
  ggsn::scalebar(x.min = -2638293 , x.max = 1906197 , # scalebar
                 y.min = 665715.9 , y.max = 5921292, 
                 dist = 1000,dist_unit = "km", transform = F,
                 location = "bottomright",border.size = .1, st.dist = .05,family = "Times New Roman")+
  ggsn::north(x.min = -2638293 , x.max = 2206197 ,  # north
              y.min = 665715.9 , y.max = 5921292,
              location = "topleft",scale = .1, symbol = 15)

# add polygon
for(i in 1:8){
  g <- g + geom_polygon(data=management_units_df[[i]], aes(long, lat, group=id),color = "black",fill = NA)
}  


# not good, annotate the polygon
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
ggsave("./7.make_figs/unit_maps.jpg",width = 4.5, height = 5,dpi = 1000)
