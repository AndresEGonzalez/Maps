# Script.gotadeleche.map.R
# Gota de Leche Maps
# Topographic sudamerica
# And satellite map detail
# Author: @Andriu_Chango 2020

rm(list=ls())
## Packages, Variables & Directories====

# install.packages("ggplot2")
library(ggplot2)
# install.packages("ggmap")
library(ggmap)
# install.packages("ncdf4")#https://www.enmimaquinafunciona.com/pregunta/132178/no-se-puede-instalar-el-paquete-de-r-marmap
library(ncdf4)
# install.packages("marmap")
library(marmap)#for getNOAA.bathy
library(grid)
# install.packages("cowplot")
require(cowplot)
library(ggfortify)
# install.packages("ggfortify")
# install.packages("ggrepel")
# install.packages("units")
# install.packages("sf")
library(sf)
require(ggrepel)

# variable size font plot

#text axis title
txt <- 8
#axis text font size
atsize<-6
#margen del plot en unidades de linea (top,rigth,bottom,left)
margen <-c(0.1, 0.01, 0.1, 0.01)
#titulo de leyendas
# titulo_leyenda <-expression(paste("Dissolved Oxygen\n~(mg~L^-1)"))

#Directory output plot  
campaña.IMG.dir<-("/home/andriu/Documents/GOTA_LECHE/MAPS")

#Maps Bathy & Topo ====

dat <- getNOAA.bathy(-76,-70,-36,-31,res=1, keep=TRUE)
dat2<-fortify.bathy(dat)

# Plot Bathy & Topo object using custom ggplot2 functions autoplot
  map_1 <-  autoplot(dat, geom=c("raster", "contour"), colour="black", size=0.1) +
  #scale_fill_distiller(name="masl",palette ="GnBu")+#"GnBu")+#"YlOrBr")+#"PuBu")+#"Greys")+# "GnBu")+#"BuGn")+#"RdBu")+#"Spectral")+
  scale_fill_etopo()+
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x = "Longitud", y="Latitud", size=txt)+
  ylim(-36,-31)+
  xlim(-76,-70)+
  theme(text=element_text(size=txt),
        legend.title = element_text(size=5),
        legend.justification=c(0,0), 
        legend.position=c(0.1,0.1),
        legend.text = element_text(size = 4),
        legend.key.size = unit(.4,"line"),
        plot.margin = unit(c(0.5,0.01,0.5,0.01), "line"),
        axis.text  = element_text(size=atsize))+
  annotation_custom(textGrob("Oceano Pacífico", x=0.4,  y=0.4, hjust=0, rot = 70,
                             gp=gpar(fontsize=6, fontface="bold")))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 15), limits = c(-36,-31))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6), limits = c(-76,-70))
    
# Let us add the scale bar and the North arrow ====
map_2<-
  map_1+  
  #North "N"
  annotate(geom="text", x=-75.5, y=-35.5, label="N", fontface="bold",color="black", size=4)+
  #Add North arrow
  annotate("point", x = -75.5, y = -35.2, colour="black", size=4, shape=2)+
  #Scale
  annotate("segment", x = -72, xend = -71, y = -35.5, yend = -35.5, arrow=arrow(ends="both", angle=90, length=unit(.1,"cm")))+
  #Km text
  annotate("text", x = -71.6, y = -35.7, label="100 Km", size=1.5)+
  annotate(geom = "rect", ymax = -33.40, ymin = -33.60, xmax = -71.50, xmin = -71.75, 
           colour = "black", size=.3, fill = NA) +
  guides(fill=FALSE)#No topo legend

# South America map ====
# get the NC data:
sud <- map_data("world", ylim=c(-50,25), xlim=c(-100,-50))

#Inset the South America map====
map_3 <- map_2 + inset(
  grob = ggplotGrob(ggplot() + 
          geom_polygon(data = sud, aes(x = long, y = lat, group = group), 
                fill="grey45", color="black", size=0.1)  + # get the state border back on top
                coord_fixed(ratio = 1)+ theme_inset()+
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
            #Square Suda map image
              annotate(geom = "rect", ymax = -30, ymin = -36, xmax = -70, xmin = -76, 
                  colour = "black", size=.3, fill = NA) 
  ),
  #Position South America map in the Bathy & Topo maps
  xmin = -76, xmax = -74, ymin = -33, ymax = -31)

# Satellite Map detail Gota de Leche by google API 2020 ====

# Google Maps API Terms of Service: http://developers.google.com/maps/terms.
# Please cite ggmap if you use it: see citation("ggmap") for details.
#https://console.cloud.google.com/apis/credentials?folder=&organizationId=&project=embebedmap

# save api key
register_google(key = "PUT YOUR API KEY HERE")

# check if key is saved
#has_goog_key()
#> [1] TRUE

gota_basemap<-get_map(location = c(lon = -71.636888, lat = -33.483510), zoom = 13, maptype = "satellite")

localities<-c("Punta de Tralca", "El Tabo", "Las Cruces", "San Sebastián", "Gota de Leche")
lon<-c(-71.697106, -71.666973,-71.623042,-71.599684,-71.636888)
lat<-c(-33.424473,-33.455757,-33.497490,-33.526585,-33.483510)
data_sgl<-data.frame(localities, lon, lat)

playas_pta<-c("Playa Las Cruces", "Punta del Lacho", "Playa Las Salinas", "Punta Tres Cruces",
              "Playa Las Monjas")
lon_p<-c(-71.624766,-71.635497,-71.636868,-71.641839,-71.640599)
lat_p<-c( -33.500351, -33.501685,  -33.495585,  -33.495239, -33.488663)
pyas_sgl<-data.frame(playas_pta, lon_p, lat_p)

matmet_mapa<-ggmap(gota_basemap, legend = "topright") +
  geom_point(aes(x=lon,y=lat), data=data_sgl, color="white", alpha = 1/1)+
  # scale_size(range=c(5,10))+
  geom_text_repel(aes(x=lon,y=lat, label=localities), color="white", 
                  data=data_sgl, 
                  fontface = "bold", 
                  nudge_x      = 0.05,
                  direction    = "y",
                  #angle        = 90,
                  vjust        = 0,
                  segment.size = 0.2)+
  geom_text_repel(aes(x=lon_p,y=lat_p, label=playas_pta), color="white", size=2,
                  data=pyas_sgl, 
                  fontface = "italic",  
                  nudge_x      = -0.05,
                  direction    = "y",
                  #angle        = 90,
                  vjust        = 0,
                  segment.size = 0.2)+
  scale_color_discrete(name = "TYPE") +
  
  labs(x = "", y = "", size=txt)+
  theme(axis.text  = element_text(size=atsize))

# Figure 18x18 cm tiff format(lzw) resolution 300 dpi  ====

# arrange into grid and align
pg<-plot_grid(map_3, matmet_mapa, align = "h", nrow = 1)

#Last figure
tiff(file.path(campaña.IMG.dir,
               paste("Figure_SGL_MAP_5", ".tiff", sep = "")),  
     units= "cm", width = 18, height = 18, res = 300, compression = "lzw")
pg
dev.off()

