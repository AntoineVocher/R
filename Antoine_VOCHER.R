# Définir le dossier de travail (mettre des / )

#####################################################################################################################

                                                      # Exercice 1

#####################################################################################################################




## Quelle différence faites vous une erreur d'estimation et une erreur de prévision ?

# L’erreur d'estimation est calculée à partir d’un Y observé et d’un Y estimé. 
#Ce dernier est déterminé à partir de l’estimation des paramètres des variables via une régression linéaire. 
#L’erreur de prévision est calculée à partir d’un Y observé et d’un Y prévu. 
#Ce dernier est déterminé à partir d’un nouveau couple (X, Y) qu’on ne connait pas et qu’on fait passer dans le modèle.
#Par conséquent, l’erreur de prévision est toujours supérieure à l’erreur d’estimation. 





## Est il possible de comparer des modèles en utilisant l'erreur d'estimation ? 

# C'est faisable, en comparant les résidus de nos modèles. Et on peut donc choisir le modèle qui minimisera l'erreur d'estimation.
# C'est pertinent dans le cas, où les modèles ne sont pas imbriqués, sinon celui ayant le plus de variables aura nécessairement une 
# erreur d'estimation minimale.

# On peut aussi utiliser le R2 ou le R2 ajusté, qui utilise l'erreur d'estimation.



# Pourquoi faire une ACP ?

## L'ACP permet d'explorer ces données afin de répondre aux objectifs suivants :

#  **Individus**  
#  - Y a-t-il des individus qui se ressemblent ? Peut-on identifier des groupes d’individus ?
#  **Variables**  
#  - Quelles sont les variables qui sont corrélées entre elles ou non ? 
#  - Quelles pourraient être les indicateurs synthétiques qui résument les variables ?   
  

#####################################################################################################################

                                                      # Exercice 2

#####################################################################################################################

# Importation des données
don <- read.table("medical.csv",header = TRUE, sep="&", dec =",", stringsAsFactors = TRUE)

summary(don)
names(don)

## 0/ Sélection des 6 premières colonnes
don <- don[,1:6]

summary(don)

## 1/ Combien y a-t-il d’individu admettant une valeur de "meanssf0RKid > 110 et dont l’entropyssf0 est inférieure à 5 ?

ind <- which( (don$meanssf0RKid > 110)&(don$entropyssf0RKid <5) )

length(ind)

don[ind,]

# Il y a 21 individus 

## 2/ Quelle est la plus forte corrélation linéaire et entre quelles variables ?

library(corrplot)
matrix <-cor(don)
head(round(matrix,2))

corrplot(matrix, method="number",type="lower", title = "La matrice de corrélation")

# La plus forte corrélation est de 0.95 entre la variable entropyssf0RKid et sdssf0RKid
# La seconde plus forte corrélation est de 0.83 entre la variable kurtosisssf0RKid et skewnessssf0RKid


## 3/ Y a t il des variables non corrélées ?

# Oui, les variables  skewnessssf0RKid et sdssf0RKid ne sont pas corréles, le coefficient de corrélation est pratiquement nul (10^-7)
# kurtosis et mpps
cor(don$skewnessssf0RKid, don$sdssf0RKid)


## 4/ Effectuer une ACP et commenter-la.

################### 
### ACP
# Sélectionne que les données quantitatives, sans le Y et on les centre-réduit
doncr <- scale (don)

## Réalise une ACP

library(FactoMineR)
library(factoextra)

resACP <- PCA(doncr,graph=F)

##########################################################
### Résultats de l'ACP 

### Inertie 

#Pourcentage d'inertie associée à chaque dimension

fviz_eig(resACP, addlabels = TRUE, ylim = c(0, 50))
resACP$eig[1:2,c("cumulative percentage of variance")]

# Si toutes les variables étaient indépendantes, elles apporteraient chacune environ 16.67 % d'information.
# Or, l'inertie de la dimension 1 est de 47% donc on a 3 variables corrélées entre elles sur l'axe 1.
# L’inertie cumulée sur les 2 premières dimensions est de 78%.


##########################################################
### Le graphique des corrélations des variables 
plot(resACP,invisible=c('quali'),choix="var",title="Graphe des variables de l'ACP")

# - Forte corrélation entre les variables, mppssf0RKid et meanssf0RKid ( déja vu dans la matrice de corrélation)
# - Non corrélation entre les variables skewnessssf0RKid et sdssf0RKid ( déja vu dans la matrice de corrélation)


##L'axe 1  
# Pour l'axe 1, les variables bien projetées sont : 
# meanssf0RKid
# mppssf0RKid
# sdssf0RKid
# entropyssf0RKid

##L'axe 2 
# Pour l'axe 2,  les variables bien projetées sont : 
# skewnessssf0RKid
# kurtosisssf0RKid

##########################################################
### Le graphique des individus 

plot(resACP,choix="ind",title="Graphe des individus de l'ACP")

# Conclusions  : 
# - Il y a beaucoup de Points aberrants : "17","8","68", etc..
# - Il y a une possible présence de différents groupes,


##########################################################
### Interprétation des axes

# Interprétation de l'axe 1  
#Les patients avec une importante valeur de meanssf0RKid ,mppssf0RKid, sdssf0RKid, entropyssf0RKid auront une forte coordonnée sur l'axe 1 

# Interprétation de l'axe 2
#Les patients avec une importante valeur de "skewnessssf0RKid" "kurtosisssf0RKid" auront une forte coordonnée sur l'axe 2


##########################################################
### Conclusion de l'ACP

## Cette première analyse a permis mettre en lumière :
# -  78% de l'inertie est comprise dans les dimensions 1 et 2
# -  une bonne projection des variables sur ces dimensions
# -  une corrélation évidente entre les variables meanssf0RKid , mppssf0RKid
# -  une non corrélation  entre skewnessssf0RKid et sdssf0RKid
# -  une présence de points aberrants
# -  une possible présence de groupes d'individus
# -  une confrontation entre les individus ayant de faible  meanssf0RKid ,mppssf0RKid, sdssf0RKid, entropyssf0RKid  (à gauche) 
#    et les individus ayant de forte  meanssf0RKid ,mppssf0RKid, sdssf0RKid, entropyssf0RKid (à droite)  (sur l'axe 1)
# -  une confrontation entre les individus ayant de faible "skewnessssf0RKid" "kurtosisssf0RKid" (en bas) et les individus 
#    ayant de forte "skewnessssf0RKid" "kurtosisssf0RKid" (en haut) (l'axe 2)



## 5 / Dans l’optique d’une classification non-supervisée des patients, combien proposeriez vous de groupes ?

# Complete

donM <- dist(doncr, method="euclidean")
cahc <- hclust(donM,method = "complete")
plot(cahc)
plot(sort(cahc$height,dec=T)[1:10], type ='h')


# Conclusion :
# Entre le 2eme et le 3e bâton se trouve un fort saut, ce qui équivaut à la difficulté de passer de 4 à 3 groupes.
# Cela consiste donc à prendre 3 groupes.
# Cette stratégie est renforcée par le fait:
# - d'obtenir des classes homogènes 

# Répartition des individus par groupe
gp <- cutree(cahc , k=3)

# Fusion de la matrice don avec le vecteur gp
doncr <- cbind(doncr, gp)

# Moyenne des variables par groupe 
donMe <- aggregate(doncr[], list(gp), mean)
donMe <- abs(donMe)

donMe

## 6/ Après avoir fait cette classification, expliciter les groupes.

# Les groupes se démarquent par rapport à l'axe 1 de l'ACP, puis l'axe 2.
# On constate que :

# Groupe 1 : Faible valeurs de meanssf0RKid ,mppssf0RKid, sdssf0RKid, entropyssf0RKid  et faible valeur de "skewnessssf0RKid" "kurtosisssf0RKid"
# Groupe 2 : Forte valeurs de meanssf0RKid ,mppssf0RKid, sdssf0RKid, entropyssf0RKid 
# Groupe 3 : Moyenne valeurs de meanssf0RKid ,mppssf0RKid, sdssf0RKid, entropyssf0RKid et forte valeur de "skewnessssf0RKid" "kurtosisssf0RKid"


#####################################################################################################################

#                                              Exercice 3

#####################################################################################################################

# Objectif : Expliquer la concentration d'ozone par des variables quantitatives et qualitatives 

#Pour cela, nous décidons de suivre cette fiche :
#  1/ Préparation des données 
# - Importation des données
# - Description du jeu de données (quanti vs Quali)
# - Renommer les variables si besoin ( dont le Y)
# - Gestion des NA 
# - Préparer une matrice des données sans le Y et centrée réduite

# 2/ Analyse exploratoire des données
#   >> Description du jeu de donnée
#   >> Analyse descriptive uni variée avec Y 
#   >> Analyse descriptive multivariée :  >> Réaliser une ACP   

# 3/  Modélisation
#   >> Sélection de variable 

# 4/  Qualité du modèle
#   >> Résidus
#   >> Points aberrants
#   >> Points leviers

# 5/ Pistes pour améliorer le modèle 
#   >> Retirer individus ?
#   >> Ajouter des polynômes pour ajuster une relation entre les X et Y

########################################################################################################################
#                                            Préparation des données 
########################################################################################################################

# Importation des données
ozone <- read.table("ozone.txt",header = TRUE, sep=" ", dec =".", row.names = 1, stringsAsFactors = TRUE)


# Description du jeu de données

summary(ozone)
names(ozone)

#Nous allons étudier les données quotidiennes de la concentration d'ozone
#Nous avons 112  individus et 13 variables.

#Les variables Quantitatives sont :

# "T9"
# "T12"
# "T15"
# "Ne9"
# "Ne12"
# "Ne15"
# "Vx9"
# "Vx12"
# "Vx15"
# "maxO3v"


#Les variables Qualitatives sont :

# - "vent" : 4 modalités
# - "pluie": 2 modalités

# La variable à expliquer est "maxO3", que nous décidons de renommer Y

ozone$Y <- ozone$maxO3
ozone$maxO3 = NULL

summary(ozone)

# On décide de renommer la variable "pluie" en "meteo" 

ozone$meteo <- ozone$pluie
ozone$pluie = NULL

summary(ozone)


#Préparer une matrice des données sans le Y et centrée réduite >> ozonecr

ozoneq <- ozone[,-11]
ozoneq <- ozoneq[,-12]

ozoneq <- ozoneq[,-11]

ozonecr <- scale(ozoneq)

#Préparer une matrice des données avec le Y et uniquement les variables quantitatives >> ozoneY

ozoneY <- ozone[,-11]
ozoneY <- ozoneY[,-12]

summary(ozoneY)

###################################################################################################################
#                                            2/ Exploration des données
###################################################################################################################

### Représenter la matrice de corrélation avec le Y 

library(corrplot)

matrix2 <-cor(ozoneY)
head(round(matrix2,2))

corrplot(matrix2, method="number",type="lower", title = "La matrice de corrélation")

# Conclusion 
# Corrélation forte entre les variables de température et forte avec le Y (sans prendre les relations entre les autres variables)
# Corrélation forte entre les variables de nébulosité et moyenne avec le Y (sans prendre les relations entre les autres variables))
# Corrélation forte entre les variables de vent et faible avec le Y (sans prendre les relations entre les autres variables))

# Il serait donc pertinent se détecter si une variable qualitative a un impact sur la relation Y et les températures (prenons T12)

# Représentation graphique entre Y et T12 en fonction de chaque variable qualitative 

library(ggplot2)
# Sec/pluie

ggplot(ozone) +
  aes(x = T12, y = Y, colour = meteo) +
  geom_point(shape = "circle", size = 1.5) +
  scale_color_hue(direction = 1) +
  theme_minimal()

# vent 

ggplot(ozone) +
  aes(x = T12, y = Y, colour = vent) +
  geom_point(shape = "circle", size = 1.5) +
  scale_color_hue(direction = 1) +
  theme_minimal()

# Conclusion
# Seule la variable météo sec/pluie semble avoir un impact sur la relation Y vs T12 


## Analyse descriptive Multivariée : ACP

################### 
### ACP
# Sélectionne que les données quantitatives, sans le Y et on les centre-réduit



## Réalise une ACP

library(FactoMineR)

resACPo <- PCA(ozonecr,graph=F)

##########################################################
### Résultats de l'ACP 

### Inertie 
resACPo$eig[1:2,c("cumulative percentage of variance")]

# L'inertie de la dimension 1 est de 54% , 
# Or si toutes les variables étaient indépendantes, elles apporteraient chacune environ 10% d'information.
# Par conséquent, on a environ 5 variables corrélées sur l'axe 1
# L'inertie de la dimension 1 et 2 est de 73%



##########################################################
### Le graphique des corrélations des variables 

# Affichons le graphe des corrélations
plot(resACPo,invisible=c('quali'),choix="var",title="Graphe des variables de l'ACP")

#Conclusion 
# Les variables Températures sont fortement corrélées entre elles (déja vu dans la matrice de corrélation)
# Les variables Vents sont fortement corrélées entre elles (déja vu dans la matrice de corrélation)
# Les variables Nébulosité sont fortement corrélées entre elles (déja vu dans la matrice de corrélation)



# ##L'axe 1  
# Pour l'axe 1, bien projetées sont : 
# "T9"     "T12"    "T15" : positivement corrélés à l'axe 1
# "Ne9"    "Ne12"   "Ne15" : négativement corrélés à laxe 1

# ##L'axe 2 
# Pour l'axe 2, bien projetées sont : 
#  "Vx9"    "Vx12"   "Vx15" : négativement corrélés à laxe 2

##########################################################
### Le graphique des individus 

plot(resACPo,choix="ind",title="Graphe des individus de l'ACP")


# Conclusions  : 
# - Bcp de Points aberrants : "20010625","20010923"
# - Possible présence de différents groupes

##########################################################
### Interprétation des axes

# Interprétation de l'axe 1  
#Les individus avec une importante valeur de T9, T12, T15, faible valeur de N9,12,Ne15  auront une forte coordonnée sur l'axe 1 
# Les dates chaudes et faiblement nébuleuses auront une forte coordonnée sur l'axe 1 

# Interprétation de l'axe 2
#Les individus  avec une faible valeur de  Vx9, Vx12, Vx15 auront une forte  coordonnée sur l'axe 2
# Les dates avec peu de vents auront une forte  coordonnée sur l'axe 2

##########################################################

##Conclusion de Analyse exploratoire des données

#- La corrélation la plus importante est entre les variables Température  et Y   (sans prendre les relations entre les autres variables))
# Les variables Températures sont fortement corrélées entre elles (déja vu dans la matrice de corrélation)
# Les variables Vents sont fortement corrélées entre elles (déja vu dans la matrice de corrélation)
# Les variables Nébulosité sont fortement corrélées entre elles (déja vu dans la matrice de corrélation)
# La variable meteo semble avoir un impact sur les relation Y vs Température
#- Une présence de points aberrants  
#- Une possible présence de groupes d'individus  
#- Une confrontation entre les dates chaudes et faiblement brumeuses vs froides et très brumeuses (sur l'axe 1)  
#- Une confrontation entre les dates fortement venteuses vs faiblement venteuses  (l'axe 2)  

#Passons à la modélisation de Y avec l'ensemble des variables 


########################################################################################################################
#                                            Modélisation
########################################################################################################################

######## SELECTION DES VARIABLES 

# # Les Modalités de référence sont :
# meteopluie
# ventnord


# Modélisation 
mod <- lm(Y~.,data= ozone)
summary(mod)

# > Peu de variables sont significatives avec le modèle complet.
# > Si je décidais de regrouper dans l'intercept les modalités non significatives avec la modalité de référence, le travail risque d'être fastidieux.
# > Par conséquent, je décide de passer par la fonction step, afin de selectionner les variables en maximisant la vraissemeblance

modAIC2 <- step(mod,trace=0) 
summary(modAIC2)

model_choisi <- modAIC2


########################################################################################################################
#                                            Qualité du modèle
########################################################################################################################

######## Analyse des Résidus 

# On suppose que les résidus suivent un loi normale

residu <-rstudent(model_choisi)
plot(rstudent(model_choisi))
abline(h=c(-2,0,2), col=c(2,1,2))

# Conclusion : Il n'y a pas de structure, les résidus sont bien aléatoires


######## POINTS ABBERANTS

# On ajoute les Y estimés

ozone2 <- ozone
y_estimé <- predict(model_choisi,ozone2)
ozone2$y_estimé <- y_estimé



# Proportion
sum(abs(residu)>2)/nrow(ozone)*100 # Proportion des points dits aberrants 

# > Seule 3.6% des résidus sont aberrants

# Identification du point le plus aberrant 

min(residu) # le point le plus aberrant
pnorm(min(residu)) # Probabilité que le point soit aberrant

point_ab = which.max(abs(residu))
point_ab = ozone2[which.max(abs(residu)),]
point_ab

# > Le point le plus aberrant concerne la date 20010731, et la probabilité pour qu'il le soit est de 10^-5
# > On pourrait l'enlever

# Identification des points aberrants

point <- sort(abs(residu)>2, dec=TRUE)[1:4]
names(point)

ozone2[names(point),] 

# Les 4 points aberrants sont : "20010707" "20010725" "20010731" "20010824"


######## LEVIERS (individus)

# Afficher les points leviers
levier <-hatvalues(model_choisi) 
plot(levier) 
plot(sort((levier), dec=TRUE)[1:20],type = "h") 

# Identification du point levier 

point_levier <-ozone[which.max(levier),]
point_levier

# Comparaison avec la moyenne de chaque colonne numériques

round(colMeans(ozoneq,1))
point_levier

# > Le point levier est l'individu 20010923, qui se démarque sur un fort taux de vent
# > On pourrait l'enlever





########################################################################################################################
#                                            Question : Modalité de référence 
########################################################################################################################


# Le modèle chosi ne dépend pas des modalités de référence située dans l'intercept
# De manière générale, si on change la modalité de référence, cela n'aura pas impacts sur les autres coefficients estimés.

# Par exemple si on une variable qualitative avec 2 modalités A et B, :
# - la modalité A devient la modalité de référence (parce qu'on ne peut pas estimé un beta pour chaque modalité)
# - Un beta est estimé pour une modalité B
# En changeant la modalité de référence, on va modifier le signe du beta afin de garder toujours le même impact de la variable qualitative sur le modèle.
# Mais l'impact des autres variables ne changera pas





#####################################################################################################################

#                                              Exercice 4

#####################################################################################################################

# Fiche 
# Objectif : Expliquer la variable Quality en fonction d'autres variables

## 1/ Préparer les données 
# - Importation des données
# - Description du jeu de données (quanti vs Quali)
# - Renommer les variables si besoin ( dont le Y)
# - Regrouper le Y en 2 classes 
# - Gestion des NA 
# - Préparer une matrice des données sans le Y et centrée réduite

## 2/ Exploration des données
# Matrice de corrélation
# Analyse descriptive  multivariée : ACP

## 3/ Clustering
# Identifier des groupes 
# Décrire les groupes  

## 4/ Modélisation : Régression logistique 
## - Estimation : 
# - Choix du Modèle (AIC le plus faible)
# - Qualité du modèle 

## 5/ Pistes pour aller plus loin 
# - Ajuster le modèle par l'ajout de variables polynomiales
# - Retirer des individus 

## Conclusion 

###################################################################################################################
#                                            1/ Préparer les données
###################################################################################################################

# - Importation des données

vin <- readRDS("vin.RDS")

# - Description du jeu de données

df <- vin
summary(df)
str(df)

# - Renommer les variables si besoin ( dft le Y)

df$Y <- ifelse( df$quality == "bon",1,0)
summary(df)
df$Y <- factor(df$Y)
df$quality = NULL

summary(df)

# - Gestion des NA 
# Pas de NA

# - Préparer une matrice des données sans le Y et centrée réduite

dfq <- df[,-12]
dfcr <- scale(dfq)



###################################################################################################################
#                                            2/ Exploration des données
###################################################################################################################

# Représenter la matrice de corrélation 

library(corrplot)

matrix <-cor(df[,-12])
head(round(matrix,2))

corrplot(matrix, method="number",type="lower", title = "La matrice de corrélation")

# Conclusion des corrélations sont à mettre en lumière  :
# Forte positive entre les variables fixed acidity et citric acidity 
# Forte positive entre les variables fixed acidity et density 
# Forte positive entre les variables total.sulfur.dioxide et free.sulfur.dioxide

# Forte  négative entre les variables fixed acidity et pH 

# D'autres variables semblent ne pas être corrélés entre les variables free.sulfur.dioyde  et total.sulfur.dioxide





# Analyse descriptive  multivariée : ACP


library(FactoMineR)

resACP <- PCA(dfcr,graph=F)

# Résultats de l'ACP 
##########################################################
### Inertie 

resACP$eig[1:2,c("cumulative percentage of variance")]

# L'inertie de la dimension 1 est de 28 %, 
# Or si toutes les variables étaient indépendantes, elles apporteraient chacune environ 9 % d'information.
# Par conséquent, on a environ 3 variables corrélées sur l'axe 1


##########################################################
### Le graphique des corrélations des variables 

plot(resACP,invisible=c('quali'),choix="var",title="Graphe des variables de l'ACP")

# Conclusions  : 
# - Forte corrélation entre les variables

# ##L'axe 1  
# Pour l'axe 1, les variables bien projetées sont : 
# fixed.acidity, citric.acid, density , pH

# ##L'axe 2 
# Pour l'axe 2, les variables bien projetées sont : 
# total.sulfur.dioxide , free.sulfur.dioxide

##########################################################
### Le graphique des individus 

plot(resACP,choix="ind",title="Graphe des individus de l'ACP")


# Conclusions  : 
# - Bcp de Points aberrants : "1436","152","1127"
# - Possible présence de différents groupes



##########################################################
### Interprétation des axes


# Interprétation de l'axe 1  
#Les vins avec une importante valeur de  fixed.acidity, citric.acid et faible  pH auront une forte coordonnée sur l'axe 1 

# Interprétation de l'axe 2
#Les vins avec une importante valeur de total.sulfur.dioxide , free.sulfur.dioxide auront une forte coordonnée sur l'axe 2

##########################################################
### Conclusion de l'exploration des données

## Cette première analyse a permis mettre en lumière :
# -  une corrélation évidente entre les variables
# -  L'axe 1 regroupe 28% de l'inertie et 45% en cumulé avec l'axe 2
# -  une non corrélation  entre pH et sulfite
# -  une présence de points aberrants
# -  une possible présence de groupes d'individus
# -  une confrontation entre les vins peu acide  (à gauche) et les vins très acide  (à droite)  (sur l'axe 1)
# -  une confrontation entre les vins avec peu de sulfure dioxyde  (en bas ) et les vins avec beaucoup de sulfure dioxyde (en haut ) (sur l'axe 2)





###################################################################################################################
#                                            3/ Clustering
###################################################################################################################

# Identifier des groupes avec la méthode CAH : Ward.D2

# Après avoir essayé avec la méthode CAH complete, la répartition dans les groupes était déséquilibrée.
# C'est pourquoi, je décide d'utiliser la méthode CAH Ward

# Clustering

dfM <- dist(dfcr)
cahc <- hclust(dfM,method = "ward.D2")
plot(cahc)
plot(sort(cahc$height,dec=T)[1:10], type ='h')

# Conclusion :
# Entre le 3er et le 4e bâton se trouve un fort saut, ce qui équivaut à la difficulté de passer de 5 à 4 groupes.
# Cela consiste dfc à prendre 4 groupes.
# Cette stratégie est renforcée par le fait:
# - d'obtenir des classes homogènes 
# - d'avoir peu de groupes pour faciliter la modélisation, si on souhaite faire un modèle par groupe


# Répartition des individus par groupe
gp <- cutree(cahc , k=4)

# Nombre d'individus par groupe
table(gp)

# Fusion de la matrice df avec le vecteur gp
dfcr <- cbind(dfcr, gp)

# Moyenne des variables par groupe 
dfMe <- aggregate(dfcr[], list(gp), mean)
dfMe <- abs(dfMe)
dfMe




##########################################################
### Conclusion du Clustering

# On a 4 groupes :

# Groupe 1 : On retrouve les vins avec des valeurs moyennes pour l'ensemble des variables  
# Groupe 2 : On retrouve les vins peu acides mais beaucoup de sulfure de dioxyde  ( axe 2 de l'ACP)
# Groupe 3 : On retrouve les vins fort alcolleux, basiques (fort ph) et beaucoup de sulfates 
# Groupe 4 : On retrouve les vins très acides, peu sucrés et de faibles densité ( axe 1 de l'ACP)


###################################################################################################################
#                                            3/ Modélisation : Régression logistique 
###################################################################################################################

# On pourrait faire 4 modèles, mais commençons par 1

## - Estimation : 
# - Choix du Modèle (AIC le plus faible)

model <- glm(Y~.,data =df, family= "binomial") # Modèle complet
summary(model)

modAIC <- step(model,trace=0) # Modèle AIC
summary(modAIC)

modBIC <- step(model,k=log(nrow(df)),trace=0) # Modèle BIC
summary(modBIC)

## Le meilleur modèle d'estimation est celui qui a l'AIC le plus faible 

modelchoisi <- modAIC

# Il dépend des variables 

names(modelchoisi$coefficients)

# Passons à l'analyse de la qualité de mon modèle choisie

############################### QUALITE DU MODELE (ESTIMATION) #############################

######## Analyse des Résidus 

# On suppose que les résidus suivent un loi normale

residu <-rstudent(modelchoisi)
plot(rstudent(modelchoisi))
abline(h=c(-2,0,2), col=c(2,1,2))

# Conclusion : Il n'y a pas de structure, les résidus sont bien aléatoires


######## POINTS ABBERANTS

# Proportion
sum(abs(residu)>2)/nrow(df)*100 # Proportion des points dits aberrants 

# > Seule 2% des résidus sont aberrants

# Identification du point le plus aberrant 

min(residu) # le point le plus aberrant
pnorm(min(residu)) # Probabilité que le point soit aberrant

point_ab = which.max(abs(residu))
point_ab = df[which.max(abs(residu)),]
point_ab

# > Le point le plus aberrant concerne l'individu 653, et la probabilité pour qu'il le soit est de 0,04%
# > On pourrait l'enlever


######## LEVIERS (individus)

# Afficher les points leviers
levier <-hatvalues(modelchoisi) 
plot(levier) 
plot(sort((levier), dec=TRUE)[1:20],type = "h") 

# Identification du point levier 

point_levier <-df[which.max(levier),]
point_levier

# Comparaison avec la moyenne de chaque colonne numériques

round(colMeans(df[,-12]),1)
point_levier

# > Le point levier est l'individu 152, qui se démarque sur un fort taux d'acid citric et de chlorides
# > On pourrait l'enlever


######## Estimation des Y avec un seuil de 0.5

# Table de confusion

estimation1 <- predict(modelchoisi, type = "response")
yestime1 <- cut(estimation1, breaks=c(0,0.5,1), labels = c(0,1) )
table(yestime1, Yvrai=df$Y)

# Précision

precision_M1 <- 100-sum(diag(table( df$Y, yestime1)))/nrow(df)*100
precision_M1

# > La précision est de 25.5, ce qui veut dire que le modèle estime correctement Y dans 74.5 % des cas. 


##########################################################
### Conclusion de la Modélisation

# - On a choisi un modèle qui minimisait l'AIC
# - les résidus sont aléatoires
# - On a identifié un point aberrant avec une probabilité de 0.04% qu'il le soit : individu 653
# - On a identifié un point levier qu'on peut retirer : individu 152 
# - Avec un seuil de 0.5%, la précision de mon modèle est de 25.5% 


###################################################################################################################
#                                            4/ Pistes pour aller plus loin
###################################################################################################################

# Je décide de retirer l'individu 152, point levier pour voir si cela améliore mon modèle

df1 <- df

df1 <- df1[-152,]

dim(df1)

# Modélisation 

model1 <- glm(Y~.,data =df1, family= "binomial") # Modèle complet
modAIC1 <- step(model1,trace=0) # Modèle AIC
modelchoisi1 <- modAIC1


######## Estimation des Y avec un seuil de 0.5

# Table de confusion

estimation2 <- predict(modelchoisi1, type = "response")
yestime2 <- cut(estimation2, breaks=c(0,0.5,1), labels = c(0,1) )
table(yestime2, Yvrai=df1$Y)

# Précision

precision_M2 <- 100-sum(diag(table( df1$Y, yestime2)))/nrow(df)*100
precision_M2

# > La précision est de 25.39, il a baissé, donc le modèle c'est amélioré 






















