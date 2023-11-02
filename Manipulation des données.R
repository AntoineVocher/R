
# Définir le dossier de travail (mettre des / )
setwd("C:/Users/antoi/OneDrive/Documents/Data Analyst/Fichiers_Sources_R/DONNEES")

# Importation des données
don <- read.table("Exo1_manipulation_R.csv",header = TRUE, sep=";", dec =",", stringsAsFactors = TRUE)

row.names =  1 ## On ne veut PAS la première colonne, autrement dit elle n'a pas de nom ou alors c'est une qualitative 



################################################################################################################################
##                                                       1/BASE
################################################################################################################################

str(don)
summary(don)
names(don)
dim(don)
nrow(don)

# Connaitre le type des variables
typevar <- sapply(don,class)
typevar

################################################################################################################################
##                                                       RENOMMER
################################################################################################################################

names(don) <-c("a","b","c","d","e")

# Renommer une colonne "Nombre" en "nb"
don$nb <- don$Nombre
don$Nombre = NULL

################################################################################################################################
##                                                       AJOUTER
################################################################################################################################

# Ajouter une colonne Prix2, le carré de Prix
don$Prix2 <- don$Prix^2


#Ajouter une nouvelle conne Y tel que Y=1 si la e est bonne et 0 sinon
don$Y <- ifelse(don$Qualite=='bon',1,0)

don$Y <- factor(don$Y)


#Ajouter une nouvelle conne W tel que Y=1 si le nombre est > 45 sinon 0
don$W <- ifelse(don$nb>45,1,0)

don$W <- factor(don$W)


################################################################################################################################
##                                                       SUPPRIMER
################################################################################################################################

# Supprimer la colonne W
don$W = NULL

names(don)


# Retirer les colonnes dont le nom contient "dir"
tmp<- names(don)
positionDIR <- grep('dir', tmp)
don <- don [,-positionDIR]




# Supprimer les lignes si la couleur est verte

position_vert <- which(don$Couleur=='vert') # Numéro de ligne avec la condtion couleur = vert
don <- don[-position_vert,]                  # Supprime la les lignes en question ( mettre la virgule !)

# Supprimer les lignes de l'individu 66

position_66 <- which(don$Articles=='ind 66')
don <- don[-position_66,]

# Supprimer les lignes si la couleur est jaune et la qualité est mauvaise

position_jaune_mauvais <- which((don$Couleur=='jaune') & (don$Qualite=='mauvais') )
don <- don[-position_jaune_mauvais,]

# Supprimer l'individu qui le prix le plus important

position_ind_max <- which.max(don$Prix) # Numero de ligne de l'individu ave le prix le plus important
ind_max <- don[position_ind_max,] # Qui est il + garder en mémoire
ind_max
don <- don[-position_ind_max,] # Supprimer la ligne correspondante au numéro de ligne 



################################################################################################################################
##                                                       CALCULER
################################################################################################################################

# Calculer la moyenne des Prix sans les NA

Mna <- mean(don$Prix, na.rm = TRUE)
Mna


# Calculer dans un nouveau dataframe la moyenne des prix par couleurs

donM <- aggregate(don$Prix,list(don$Couleur),mean)
donM

#aggregate(colonne numérique, list de critère, moyenne )

# Calculer dans un nouveau dataframe la somme des prix par couleurs

donM1 <- aggregate(don$Prix,list(don$Couleur),sum)
donM1

# Calculer le nombre d’individus qui ont un prix supérieure à 10 et qui sont jaune ?

qui<- (don$Prix>10)&(don$Couleur == 'jaune')
sum(qui)
don[qui,]




################################################################################################################################
##                                                       Manipuler NA 
################################################################################################################################

#Quels sont les individus qui ont au moins 1 NA ?

don_NA <- is.na(don) # Dataframe de booleen avec TRUE si NA

Nbr_NA_ligne <- apply(don_NA, 1,sum) # Somme le nombre de NA par ligne

Position_ind_na <- which(Nbr_NA_ligne >0) # Numéro de ligne des individu avec au moins 1 NA

Ind_NA <- don[Position_ind_na,]
Ind_NA


# Quelle variable a le plus de NA ?
which.max(apply(is.na(don),2,sum))

# Quel individu a le plus de NA ?
which.max(apply(is.na(don),1,sum))


################################################################################################################################
##                                                       Fusionner 
################################################################################################################################

# Fusionner par colonne
doncr <- cbind(doncr, gp)

# Fusionner par ligne

new_ind <- c(factor("ind 7"), "25" ,    "10"  , "bon" , "vert")
don <- rbind(don, new_ind)





################################################################################################################################
##                                                       ORDONNER 
################################################################################################################################

don$Prix <- sort(don$Prix)

