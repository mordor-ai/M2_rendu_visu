# Let's read the jeoJson file that is stored on the web with the geojsonio library:
install.packages("geojsonio")
install.packages("broom")
install.packages("mapproj")
install.packages("viridis")
install.packages("ggradar")

library(scales)
library(tidyr)
library(maps)
library(geojsonio)
library(broom)
library(ggplot2)
library(mapproj)
library(dplyr)
library(stringr)
library(viridis)

#charge les données des arrondissements pour l'affichage
spd <- geojson_read("./datas/arrondissements.geojson",  what = "sp")

geo_paris <- tidy(spd)

# c'est le numero d'arrondissement mais il est en charactère
geo_paris$id <- as.integer(geo_paris$id)

# Charge les données des actes par arrondissement
dataParis <- read.csv2("./datas/statistiques_de_creation_d_actes_d_etat_civil_par_arrondissement.csv",encoding = "UTF-8")
head(dataParis)


## crée une colonne id avec le N° de l'arrondissement
dataParis$id <-as.integer(str_extract(dataParis$Arrondissement, "[0-9]+"))

# jointure gauche pour lier les données geo et les actes
geo_paris = geo_paris %>%
  left_join(. , dataParis, by=c("id"="id"))

# je regroupe par année et par acte
dataParisAgg <- aggregate(dataParis$Nombres, by=list(dataParis$Année,dataParis$Type.d.acte), FUN=sum)

head(dataParisAgg)




# filtre sur les mariages en 2014
geo_paris_filtered <- filter(geo_paris, geo_paris$Année==2014, geo_paris$Type.d.acte == "Mariage")



# plot final

p <- ggplot() +
  geom_polygon(data = geo_paris_filtered, aes(fill = Nombres, x = long, y = lat, group = group) , size=0, alpha=0.9) +
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








# filtre sur les mariages en 2014
geo_paris_filtered <- filter(geo_paris, geo_paris$Année==2014, 
                             geo_paris$Type.d.acte == "Décès")



# plot final

p <- ggplot() +
  geom_polygon(data = geo_paris_filtered, aes(fill = Nombres, x = long, y = lat, group = group) , size=0, alpha=0.9) +
  theme_void() +
  scale_fill_viridis(trans = "log", breaks=c(1,250,500,750,1000), 
                     name="Nombre de Décès", 
                     guide = guide_legend( keyheight = unit(3, units = "mm"), 
                                           keywidth=unit(12, units = "mm"), 
                                           label.position = "bottom", 
                                           title.position = 'top', nrow=1) ) +
  labs(
    title = "Ville de Paris - Actes civils",
    subtitle = "Nombres de Décès  en 2014",
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






# filtre sur les mariages en 2014
geo_paris_filtered <- filter(geo_paris, geo_paris$Année==2014, 
                             geo_paris$Type.d.acte == "Naissance")



# plot final

p <- ggplot() +
  geom_polygon(data = geo_paris_filtered, aes(fill = Nombres, x = long, y = lat, group = group) , size=0, alpha=0.9) +
  theme_void() +
  scale_fill_viridis(trans = "log", breaks=c(1,250,500,750,1000), 
                     name="Nombre de Naissances", 
                     guide = guide_legend( keyheight = unit(3, units = "mm"), 
                                           keywidth=unit(12, units = "mm"), 
                                           label.position = "bottom", 
                                           title.position = 'top', nrow=1) ) +
  labs(
    title = "Ville de Paris - Actes civils",
    subtitle = "Nombres de Naissances en 2014",
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






# filtre sur les mariages en 2014
geo_paris_filtered <- filter(geo_paris, geo_paris$Année==2014, 
                             geo_paris$Type.d.acte == "Reconnaissances")



# plot final

p <- ggplot() +
  geom_polygon(data = geo_paris_filtered, aes(fill = Nombres, x = long, y = lat, group = group) , size=0, alpha=0.9) +
  theme_void() +
  scale_fill_viridis(trans = "log", breaks=c(1,250,500,750,1000), 
                     name="Nombre de Reconnaissances", 
                     guide = guide_legend( keyheight = unit(3, units = "mm"), 
                                           keywidth=unit(12, units = "mm"), 
                                           label.position = "bottom", 
                                           title.position = 'top', nrow=1) ) +
  labs(
    title = "Ville de Paris - Actes civils",
    subtitle = "Nombres de Reconnaissances en 2014",
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
