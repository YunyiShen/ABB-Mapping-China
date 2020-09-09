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
  
  
new_scale <- function(new_aes) {
    structure(ggplot2::standardise_aes_names(new_aes), class = "new_aes")
}  
  

ggplot_add.new_aes <- function(object, plot, object_name) {
  plot$layers <- lapply(plot$layers, bump_aes, new_aes = object)
  plot$scales$scales <- lapply(plot$scales$scales, bump_aes, new_aes = object)
  plot$labels <- bump_aes(plot$labels, new_aes = object)
  plot
}


loadfonts()
map_10 <- raster("./3. fusion/Fusion/bear_mean.tif")

IUCN_polygon <- rgdal::readOGR("./8.IUCN/ABB_IUCN.shp")
IUCN_polygon_tras <- spTransform(IUCN_polygon,map_10@crs)
IUCN_polygon_fort <- fortify(IUCN_polygon_tras)


binary_map <- raster("./3. fusion/Fusion/binary_0.39.tif")
binary_map <- ratify( asFactor(binary_map))

gplot(binary_map) + 
  geom_tile(aes(fill = value)) + 
  scale_fill_gradient(high = "#0072B2",
                      low =  "lightgrey",
                      na.value="transparent") + 
  #scale_fill_gradient(low = "#0072B2",
  #                    high =  "#E7B800",
  #                    na.value="transparent") +
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
  ggsn::scalebar(x.min = -2638293 , x.max = 1906197 , 
                 y.min = 665715.9 , y.max = 5921292, 
                 dist = 1000,dist_unit = "km", transform = F,
                 location = "bottomright",border.size = .1, st.dist = .05,family = "Times New Roman")+
  ggsn::north(x.min = -2638293 , x.max = 2206197 , 
              y.min = 665715.9 , y.max = 5921292,
              location = "topleft",scale = .1, symbol = 15) + 
  new_scale("fill") +
  geom_polygon(data=IUCN_polygon_fort, 
               aes(long, lat, group=group,subgroup = as.factor(hole),fill = as.numeric(id)),alpha = .8)
#  geom_sf(data=IUCN_polygon_fort,aes(long, lat, group=group,subgroup = as.factor(hole)))


