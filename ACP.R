#On commence par une ACP


# Définir le dossier de travail (mettre des / )
setwd("C:/Users/antoi/OneDrive/Documents/Data Analyst/Fichiers_Sources_R/DONNEES")

# Importation des données
don<- read.table("wpbc.data", sep=",", dec =".", stringsAsFactors = TRUE, row.names =  1, na.strings = "?")

don <- na.omit(don)

summary(don)
str(don)

###############################################################################################################
#                                         Analyse Multi Variance : ACP
###############################################################################################################

################### 
### ACP
# Sélectionne que les données quantitatives, sans le Y et on les centre-réduit

donq <- don[,-1]
donq <- scale(donq)

## Réalise une ACP

library(FactoMineR)
library(factoextra)

resACP <- PCA(donq,graph=F)

##########################################################
### Résultats de l'ACP 

### Inertie 

fviz_eig(resACP, addlabels = TRUE, ylim = c(0, 50))

resACP$eig[1:2,c("cumulative percentage of variance")]

# L'inertie de la dimension 1 est de XX %, 
# Or si toutes les variables étaient indépendantes, elles apporteraient chacune environ `round(100/(ncol(donq)),2)` % d'information.
# Par conséquent, on a 10 variables corrélés sur l'axe 1

##########################################################
### Le graphique des corrélations des variables 

plot(resACP,invisible=c('quali'),choix="var",title="Graphe des variables de l'ACP")

# Conclusions  : 
# - Forte corrélation entre les variables, danger pour la régression 

# ##L'axe 1  
# Pour l'axe 1, les variables bien projetées sont : 
# 

# ##L'axe 2 
# Pour l'axe 2, les variables bien projetées sont : 

##########################################################
### Le graphique des individus 

plot(resACP,choix="ind",title="Graphe des individus de l'ACP")


# Conclusions  : 
# - Bcp de Points aberrants : "","",""
# - Possible présence de différents groupes, danger sur la répartition des Y ( qui sont répartie entre 25/75)



##########################################################
### Interprétation des axes

dim(donq)

# Regardons l'individu 439
donq[c("865423"),]

# Regardons l'individu 843483
donq[c("843483"),]

# La moyenne par variables

round(colMeans(donq[,]),1)


# Interprétation de l'axe 1  
#Les individus avec une importante valeur de age et de poids (bmi) auront une forte coordonnée sur l'axe 1 

# Interprétation de l'axe 2
#Les patients avec une importante valeur de children auront une forte coordonnée sur l'axe 2


##########################################################
### Conclusion de l'ACP

## Cette première analyse a permis mettre en lumière :
# -  une corrélation évidente entre les variables
# -  l'axe 1 regroupe XX% de l'inertie 
# -  une non corrélation  entre 
# -  une présence de points aberrants
# -  une possible présence de groupes d'individus
# -  une confrontation entre les individus (à gauche) et les individus (à droite)  (sur l'axe 1)
# -  une confrontation entre les individus (à gauche) et les individus (à droite) (l'axe 2)


## Suite :
# - Discriminer les variables : exemple uniquement les means ?
# - Retirer des variables très corrélées entre elles pour faciliter la modélisation ?
# - Réaliser une classification non supervisée, pour mettre en évidence plusieurs groupes d'individus ?
# - Modéliser via une régression ?




names(resACP)




