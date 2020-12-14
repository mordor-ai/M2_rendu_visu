#importation des library
library(ggplot2)
library(dplyr)
library(tidyr)

#chargement de donn??es
data = read.csv(file = "/Users/sariakarandrianjanahary/Desktop/projet_visu/statistiques_de_creation_d_actes_d_etat_civil_par_arrondissement.csv",sep =";",encoding = "UTF-8")

#cr??ation d'une nouvelle donn??e dataParisAgg qui regroupe le nombre selon l'ann??e et le type d'acte
dataParisAgg <- aggregate(data$Nombres, by=list(data$Ann??e,data$Type.d.acte), FUN=sum)
dataParisAgg %>%
  ggplot(aes(x=Group.1, y=x, group=Group.2, fill=Group.2, color=Group.2)) + 
  geom_line()+
  labs(title = "L'??volutions du nombre d'actes civils dans le temps en fonction du type ", 
       x = "Ann??e", 
       y = "Nombres")
