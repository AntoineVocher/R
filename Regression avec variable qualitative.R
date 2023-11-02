
######################## REGRESSION AVEC X Qualitative #####################

#Idées :
## On ajoute autant de variables que de modalités lorsqu'on fait de la modélisation

## La modalité de référence est déterminée comme la 1ere alphabétiquement, elle est dans l'intercept

## Le principe est d'avoir le moins de variables possibles, pourquoi ?
# Économiser dans la mesure (ne prendre que les bonnes variables)
# Mieux prévoir (le modèle est moins “variable”, plus précis)
# Répondre à une question (éliminer des variables = elles ne sont pas utiles pour expliquer)

## Comment ?
## 1/ Méthode COMPLET : Regroupe et on refait etc..
# Regrouper dans l'intercept, les modalités :
# les moins significatives
# les plus corrélés avec la modalité de référence ( elles se ressemblent)

## 2/ Méthode STEP 
#Soit on garde la variable qualitative soit on l'enlève 

## 2/ Méthode LEAPS 
#les modalités de la variable qualitative sont considérées comme des variables  


####################### PREPARATION DES DONNEES ############################

# Définir le dossier de travail (mettre des / )
setwd("C:/Users/antoi/OneDrive/Documents/Data Analyst/Fichiers_Sources_R/DONNEES")

# Importation des données
don <- read.table("ozone.txt",header = TRUE, sep="", dec =".", stringsAsFactors = TRUE) 
summary(don)
names(don)
don <- don[,c("maxO3","T12","vent","pluie")]

# Renomme la variable puie en météo
colnames(don)[4]<-"meteo"


###################### 1/ MODELISATION COMPLET ############################


# Modélisation avec le modèle complet
mod1<- lm(maxO3~T12 + meteo + vent,data= don)         
summary(mod1)

# Changer la modalité de référence 'Intercept)
don$meteo = relevel(don$meteo,"Sec") 

# Regrouper les modalités pour mettre dans l'intercept 

# Afficher les modalités 
levels(don$vent)

# Regrouper "Est" et "Ouest", en position 1 et 3 respectivement 

levels(don$vent)[c(1,3)] <- "Est-Ouest"
levels(don$vent)


###################### 2/ STEP ############################

# Modélisation avec le modèle AIC, avec Step
modAIC<- step(lm(maxO3~T12 + meteo + vent,data= don))        
summary(modAIC)


