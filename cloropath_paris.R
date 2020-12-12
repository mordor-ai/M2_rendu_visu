# Let's read the jeoJson file that is stored on the web with the geojsonio library:
install.packages("geojsonio")
install.packages("broom")
install.packages("mapproj")
install.packages("viridis")


library(maps)
library(geojsonio)
library(broom)
library(ggplot2)
library(mapproj)
library(dplyr)
library(stringr)
library(viridis)

spd <- geojson_read("./datas/arrondissements.geojson",  what = "sp")

spdf_fortified <- tidy(spd)

head(spdf_fortified$id)
str(spdf_fortified)
head(spd)

spdf_fortified$id <- as.integer(spdf_fortified$id)
str(spdf_fortified)
# read data
dataParis <- read.csv2("./datas/statistiques_de_creation_d_actes_d_etat_civil_par_arrondissement.csv",encoding = "UTF-8")
head(dataParis)


## get the id or arrdt
dataParis$id <-as.integer(str_extract(dataParis$Arrondissement, "[0-9]+"))
str(dataParis$id )




spdf_fortified = spdf_fortified %>%
  left_join(. , dataParis, by=c("id"="id"))
head(spdf_fortified)

# Plot simple

ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="#69b3a2", color="white") +
  theme_void() +
  coord_map()

# filtre sur les maraiages en 2014
spdf_fortified_filtered <- filter(spdf_fortified, spdf_fortified$Année==2014, spdf_fortified$Type.d.acte == "Mariage")



# plot amélioré
ggplot() +
  geom_polygon(data = spdf_fortified_filtered, aes(fill = Nombres, x = long, y = lat, group = group)) +
  theme_void() +
  coord_map()


# plot final



p <- ggplot() +
  geom_polygon(data = spdf_fortified_filtered, aes(fill = Nombres, x = long, y = lat, group = group) , size=0, alpha=0.9) +
  theme_void() +
  scale_fill_viridis(trans = "log", breaks=c(1,250,500,750,1000), name="Nombre de mariages", 
                     guide = guide_legend( keyheight = unit(3, units = "mm"), 
                                           keywidth=unit(12, units = "mm"), 
                                           label.position = "bottom", 
                                           title.position = 'top', nrow=1) ) +
  labs(
    title = "Ville de Paris - Actes civils",
    subtitle = "Nombres de Mariages en 2014",
    caption = "Data: OPEN DATA PARIS | Creation: Emmanuel Pellegrin,  XXXX, XXXX"
  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    
    legend.position = c(0.7, 0.09)
  ) +
  coord_map()
p
