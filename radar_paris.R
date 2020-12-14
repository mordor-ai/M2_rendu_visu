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
##library(cast)

# read data
dataParis <- read.csv2("./datas/statistiques_de_creation_d_actes_d_etat_civil_par_arrondissement.csv",encoding = "UTF-8")
head(dataParis)

dataParisAgg <- aggregate(dataParis$Nombres, by=list(dataParis$Année,dataParis$Type.d.acte,dataParis$Arrondissement), FUN=sum)
head(dataParisAgg)
names(dataParisAgg)<-c("ANNEE","TYPE_ACTE",  "ARRONDISSEMENT", "NOMBRES")
head(dataParisAgg)



## tous les arrondissements

dataParisAggFiltered <- filter(dataParisAgg, dataParisAgg$ANNEE==2014
                               #, dataParisAgg$ARRONDISSEMENT=="15ème arrdt"
                               )
dataParisAggFiltered <- subset (dataParisAggFiltered, select = -ANNEE)
dataParisAggFiltered



ggplot(data=dataParisAggFiltered,  aes(x=TYPE_ACTE, y=NOMBRES, group= ARRONDISSEMENT, colour=ARRONDISSEMENT, fill=ARRONDISSEMENT)) + 
  geom_point(size=2) + 
  geom_polygon(size = 1, alpha= 0.2) + 
  ylim(0, 10000) + 
ggtitle("Année 2014" )  + 
 # scale_x_discrete() +
  theme_light()+
  coord_polar()






# je fait une table pivot
df_scaled <- reshape::cast(dataParisAggFiltered, ARRONDISSEMENT ~ TYPE_ACTE,value="NOMBRES")
# je nomme les lignes avec le nom de l'rrondissement
row.names(df_scaled) <-df_scaled[,1]


# Descriptif des variables
# Obtenir le minimum et le maximum de chaque colonne
col_max <- apply(df_scaled, 2, max)
col_min <- apply(df_scaled, 2, min)
# Calculer le profil moyen 
col_mean <- apply(df_scaled, 2, mean)
# Rassembler le descriptif des colonnes
col_summary <- t(data.frame(Max = col_max, Min = col_min, Average = col_mean))
# je supprime la colonne arrondissment qui ne sert plus à  rien
df_scaled <- subset( df_scaled, select = -ARRONDISSEMENT )
df_scaled
# Rattacher le descriptif des variables aux données
df_scaled2 <- as.data.frame(rbind(col_summary, df_scaled))

oldpar <- par(mar = rep(0.8,4),mfrow = c(5,4)) 
# Définir les paramètres graphiques dans une grille 3x4, avec des marges appropriées:
#par()
#par()
# Produire un graphique radar pour chaque élève
# on commence par 4 car les 3  premier graphiques sont les moyennes , max  et min
for (i in 4:nrow(df_scaled2) ) {
  radarchart(
    df_scaled2[c(1:3, i), ],
    pfcol = c("#99999980",NA),
    pcol= c(NA,2), plty = 1, plwd = 2,
    title = row.names(df_scaled2)[i]
  )
}


# Restaurer les paramètres standard de par()
par(oldpar) 


