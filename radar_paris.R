# from https://gis.stackexchange.com/questions/295227/how-to-make-customized-radar-chart-in-r
#radar 
install.packages("reshape")

install.packages("scales")
install.packages("fmsb")
library(ggplot2)
library(ggradar)
library(dplyr)
library(scales)
library(tibble)
library(reshape)
library(dplyr)
library(fmsb)
library(cast)

# read data
dataParis <- read.csv2("./datas/statistiques_de_creation_d_actes_d_etat_civil_par_arrondissement.csv",encoding = "UTF-8")
head(dataParis)

dataParisAgg <- aggregate(dataParis$Nombres, by=list(dataParis$Année,dataParis$Type.d.acte,dataParis$Arrondissement), FUN=sum)
head(dataParisAgg)
names(dataParisAgg)<-c("ANNEE","TYPE_ACTE",  "ARRONDISSEMENT", "NOMBRES")
head(dataParisAgg)


dataParisAggFiltered <- filter(dataParisAgg, dataParisAgg$ANNEE==2005)

dataParisAggFiltered$NOMBRES <-as.numeric(dataParisAggFiltered$NOMBRES)

dataParisAggFiltered <- subset (dataParisAggFiltered, select = -ANNEE)
dataParisAggFiltered



ggplot(data=dataParisAggFiltered,  aes(x=TYPE_ACTE, y=NOMBRES, group= ARRONDISSEMENT, colour=ARRONDISSEMENT, fill=ARRONDISSEMENT)) + 
  geom_point(size=2) + 
  geom_polygon(size = 1, alpha= 0.2) + 
#  ylim(-2.0, 2.0) + 
  ggtitle("Année 2005")  + 
 # scale_x_discrete() +
  theme_light()+
  coord_polar()





