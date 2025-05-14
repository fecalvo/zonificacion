library(tidyverse)
library(sf)

misf <- sf::read_sf("ARG/gadm41_ARG_1.shp")

data <- readxl::read_excel("ARG/coordenadas.xlsx")

nombre <- c("Tilimuqui", "Angulos", "Tunuyan" , "Tupungato")
lat <- c(-28.64, -29.16, -33.58, -33.48)
lon <- c(-67.41, -67.66, -69.20, -69.17)

ubi <- tibble(nombre, lat, lon)

numero <- c(1,1,1,1)

cbind(ubi, numero)

jpeg("grafico/mapa_nogal.jpeg", width = 4000, height = 4000, 
     units = "px", res = 800) 

ggplot(ubi) +
  geom_sf(data = misf, fill = "white", color = "black") +
  geom_point(aes(x = lon, y = lat), color = "red", size = 2, shape = 17) +
  geom_point(aes(x = lon, y = lat),color = "black", size = 2, shape = 2) +
  guides(color = guide_legend(title="Aptitud")) +
  theme_bw() +
  ylim(-34, -28) +
  xlim(-70, -63) +
  annotate(geom = "text", x = -66.6, y = -28.64, label = "Angulos", angle = 0, size = 4) +
  annotate(geom = "text", x = -66.8, y = -29.17, label = "Tilimuqui", angle = 0, size = 4) +
  annotate(geom = "text", x = -68.5, y = -33.68, label = "Tunuyán", angle = 0, size = 4) +
  annotate(geom = "text", x = -68.3, y = -33.38, label = "Tupungato", angle = 0, size = 4) +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none")

dev.off()