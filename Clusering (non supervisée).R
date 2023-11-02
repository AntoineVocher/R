##Notes :
# distance de Manhathan sera moins sensible aux outliers 
# l'idee est de minimiser l'inertie intraclasse et de maximiser l'inertie interclasee
#cah marche bien avec peu d'idividus





# Définir le dossier de travail (mettre des / )
setwd("C:/Users/antoi/OneDrive/Documents/Data Analyst/Fichiers_Sources_R/DONNEES")



# Importation des données
don <- read.table("maisons_Boston.csv",header = TRUE, sep=";", dec =".",stringsAsFactors = TRUE)

don <- na.omit(don)

## Prérequis 

doncr <- don

#Enlever le Y
ouY <- grep("medv", names(doncr))
doncr <- doncr [,-ouY]

# Centrer/Réduire les données : les variables ont des unités différentes
doncr <- scale(doncr)

# Enlever les variables quali
doncr <- doncr[,-4]



####### ############################CAHC########################################
# Complete

donM <- dist(doncr)
cahc <- hclust(donM,method = "complete")
plot(cahc)
plot(sort(cahc$height,dec=T)[1:10], type ='h')

# il faut prendre la barre avant le plus grand saut et la première = 2 groupes

# Conclusion :
# Entre le 1er et le 2e bâton se trouve un fort saut, ce qui équivaut à la difficulté de passer de 3 à 2 groupes.
# Cela consiste donc à prendre 2 groupes.
# Cette stratégie est renforcée par le fait:
# - d'obtenir des classes homogènes 
# - d'avoir peu de groupes pour faciliter la modélisation 


# Répartition des individus par groupe
gp <- cutree(cahc , k=6)

# Nombre d'individus par groupe
table(gp)

# Fusion de la matrice don avec le vecteur gp
doncr <- cbind(doncr, gp)

# Moyenne des variables par groupe 
donMe <- aggregate(doncr[], list(gp), mean)
donMe <- abs(donMe)

# Différence

don_dif <- (donMe[1,-1]-donMe[2,-1])

sort(don_dif)



#######################################################################



library(factoextra)

## Données quantitatives et normalisées
summary(doncr)


## CAH single
donM <- dist(doncr)
cahc <- hclust(donM,method = "single")

plot(sort(cahc$height,dec=T)[1:10], type ='h')


## CAH Ward
donM <- dist(doncr)
cahc <- hclust(donM,method = "ward.D2")

plot(sort(cahc$height,dec=T)[1:10], type ='h')




## K means

don_km <- kmeans(doncr,centers=2)
don_km$cluster
table(don_km$cluster)


don_km$totss # Inertie TOTALE
don_km$tot.withinss # Inertie intraclasse
don_km$betweenss  # Inertie interclasse

## Distance manhattan

distances <- dist(doncr, method = "manhattan")
summary(distances)


