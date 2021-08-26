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
map_crs <- CRS("+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
namesss <- c("Extend","Possible","")
IUCN_polygon <- rgdal::readOGR("./8.IUCN/ABB_IUCN.shp")
IUCN_polygon_tras <- spTransform(IUCN_polygon,map_crs)
IUCN_polygon_fort <- fortify(IUCN_polygon_tras)
IUCN_polygon_fort$id[IUCN_polygon_fort$hole] <- 2
IUCN_polygon_fort$id <- as.numeric(IUCN_polygon_fort$id) + 1
IUCN_polygon_fort$IUCN <- factor(namesss[IUCN_polygon_fort$id],levels = namesss)

IUCN_raster <- rasterize(IUCN_polygon_tras, y)
IUCN_raster <- IUCN_raster * (binary_map>-1)
sum(getValues(IUCN_raster)==1, na.rm = T)
sum(getValues((IUCN_raster==2) * (binary_map)),na.rm = T)

binary_map <- raster("./3. fusion/Fusion/binary_0.39.tif")
binary_map <- ratify( asFactor(binary_map))

PA_name <- c("A","P")

points_coarse <- read.csv("./1. 30km/training data points.csv",row.names = 1)
points_coarse <- points_coarse[,c(2,3,4)]
points_coarse$P.A <- PA_name[points_coarse$P.A+1]
points_coarse$res <- "coarse"
points_coarse$source <- paste(points_coarse$res,points_coarse$P.A,sep = "-")
points_coarse <- points_coarse[,-5]
colnames(points_coarse) <- c("X","Y","P.A.","data.type")

points_fine <- read.csv("./2. 3.16km/afterthinnerandchoosing.csv",row.names = 1)
points_fine$R <- PA_name[points_fine$R+1]
points_fine$res <- "fine"
points_fine$source <- paste(points_fine$res,points_fine$R,sep = "-")
points_fine <- points_fine[,c(2,3,4,7)]
colnames(points_fine) <- colnames(points_coarse)

points_all <- rbind(points_coarse,points_fine)


gplot(binary_map) + 
  geom_tile(aes(fill = value),show.legend = FALSE ) + 
  scale_fill_gradient(high = "#0072B2",
                      low =  "lightgrey",
                      na.value="transparent") + 
  #scale_fill_gradient(low = "#0072B2",
  #                    high =  "#E7B800",
  #                    na.value="transparent") +
  #guides(fill=FALSE)+
  labs(x = "", y="") +
  theme(text = element_text(size=12,family = "Times New Roman"), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(.15, .15, .15, .15, "cm"))+
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    legend.background = element_rect(fill = "transparent",color = NA),#, # get rid of legend bg
    legend.key = element_rect(fill = "transparent")
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
  #ggplot()+geom_polygon(data = datapoly, aes(x= x, y = y, fill = value, group = id, subgroup = subid))
  geom_polygon(data=IUCN_polygon_fort, 
               aes(long, lat, group=group,subgroup = factor(hole),fill = IUCN),alpha = .35) + 
  scale_fill_manual(values = c("#E69F00", "#009E73",adjustcolor("white",alpha.f = 0))) + 
  new_scale("color") +
  geom_point(data = points_all,mapping = aes(x = X, y = Y, color = data.type,shape = data.type),size = 1.5) + 
  scale_color_manual(values = c("#E69F00", "#CC79A7","#009E73" ,"#D55E00"))

ggsave("./7.make_figs/IUCN_points.jpg",width = 7,height = 4,unit = "in",dpi = 600)


