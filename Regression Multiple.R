

# Définir le dossier de travail (mettre des / )
setwd("C:/Users/antoi/OneDrive/Documents/Data Analyst/Fichiers_Sources_R/DONNEES")

# Importation des données
ozone <- read.table("ozone_complet.txt",header = TRUE, sep=";", dec =".", row.names = 1) 

tmp<- names(ozone)
tmp
positionDIR <- grep('dir', tmp)
ozone <- ozone [,-positionDIR]  # on enlève les directions

don <- na.omit(ozone) # Jeu de données nettoyés

############################### REGRESSION SIMPLE #############################
 
mod1<- lm(maxO3~T6,data= don) # Modélisation de régression simple de Max03 par T6
summary(mod1) # Afficher les résultats


don2 <- don # Copier le dataframe dans une nouveau jeu de donnée 
don2$T6_carré <- don2$T6^2 # Ajouter le carré d'une variable de T6
mod2 <- lm(maxO3~T6+T6_carré, don2 ) # Modélisation de régression multiple de Max03 par T6 et son carré
summary(mod2)

## ICI les modèles sont imbriqués, on peut faire un test de Student pour sélectionner le meilleur

############################### TEST (Imbrication de modèle) #####################

# H0 : B3 = 0 (la variable ajoutée n'améliore pas le modèle)
# H1 : B3 <> 0 (Oui)
# 
# Rejette H0 si pvalue < 0.05

anova(mod1,mod2)


############################### REGRESSION MUTIPLE #############################

mod<- lm(don$maxO3~.,data= don)                     # Modèle complet
summary(mod)


############################### PREDICTION DE VARIABLE ############################

Dt_new <- data.frame ( circ = c(30,40,100), circ_sqrt = c(30,40,100)) # Création d'un dataframe (X) avec 2 colonnes et 3 lignes
predict(mod2,Dt_new) # Estimation de Y avec des variables définies de X

############################### SELECTION DE VARIABLE #############################


mod<- lm(don$maxO3~.,data= don)                     # Modèle complet
summary(mod)

modAIC <- step(mod)                                       # Modèle AIC
summary(modAIC)

modBIC <- step(mod, k= log(nrow(don)),trace =0)        # Modèle BIC
summary(modBIC)

tmp= summary(modAIC)
tmp$coefficients
row.names(tmp$coefficients)  # Obtenir le nom des variables sélectionnées

############################### QUALITE DU MODELE #############################

######## Analyse des Résidus 

# On suppose que les résidus suivent un loi normale

residu <-rstudent(modAIC)

# Représentation des résidus 
plot(rstudent(modAIC))
abline(h=c(-2,0,2), col=c(2,1,2))

# Conclusion : Il n'y a pas de structure, les résidus sont aléatoires


######## POINTS ABBERANTS : Différence sur le Y 
## Points aberrants =  Résidus situés au dessus de 2 et en dessous de -2

sum(abs(residu)>2)/nrow(don)*100 # Proportion des points dits aberrants 
min(residu) # le point le plus aberrant
pnorm(min(residu)) # Probabilité que le point soit aberrant


y_estimé <- predict(modAIC,don)
don$y_estimé <- y_estimé


point_ab = which.max(abs(residu))
point_ab = don[which.max(abs(residu)),]

# Points aberrants avec Y estimé et Y observé 
point_ab

## Pour aller plus loin 
# Si le point est vraiment aberrant, par rapport à sa probabilité très faible alors 
# On le retire
# On refait tourner le modèle, etc..







######## LEVIERS (individus)
## DEF : Différents par rapport aux variables (X)

# Afficher les points leviers 

levier <-hatvalues(modAIC) 
plot(levier) # Afficher les points leviers
plot(sort((levier), dec=TRUE)[1:20],type = "h") # Afficher les points leviers


## Identification des points leviers 

point_levier <-don[which.max(levier),] #  Qui est il ? 

point_levier <-don[which.max(levier),c("T6","T12","T15","Ne9","Ne12","Vvit6","Vvit9","Vvit15","Vvit18","Vx","maxO3v", "y_estimé", "maxO3")]
point_levier


##Comparaison avec la moyenne des colonnes (des X) 
round(colMeans(don[,c("T6","T12","T15","Ne9","Ne12","Vvit6","Vvit9","Vvit15","Vvit18","Vx","maxO3v")]),1)


## On compare les résultats pour voir si il s'agit bien d'un point levier

############################################# SELECTION DE MODELE POUR LA PREVISION (données vierges) ###################################


################### VALIDATION CROISEE avec BLOC ########################"

# pas faire de modélisation avant (on connaitra les X )

ozone <- read.table("ozone_complet.txt",header = TRUE, sep=";", dec =".", row.names = 1) 
don <- na.omit(ozone)

don <- don
summary(don)
don$Y = don$maxO3
don$maxO3 = NULL
K=10
set.seed(1234)
blocs=sample(rep(1:K,length=nrow(don)))
PREV <- data.frame(Y=don$Y,complet=NA,AIC=NA,BIC=NA)
for(ii in 1:K){
  print(ii)
  donA = don[blocs!=ii,]
  donT = don[blocs==ii,]
  ###################
  mod1 <- lm(Y~.,data=donA)
  PREV[blocs==ii,"complet"]=predict(mod1,donT)
  ###################
  mod2 <- step(lm(Y~.,data=donA),trace=0)
  PREV[blocs==ii,"AIC"]=predict(mod2,donT)
  ###################
  mod3 <- step(lm(Y~.,data=donA),trace=0,k=log(nrow(donA)))
  PREV[blocs==ii,"BIC"]=predict(mod3,donT)
}


erreur <- function(X,Y){mean((X-Y)^2)}
apply(PREV,2,erreur,Y=PREV[,1])

## On choisit le modèle avec la pus faible erreur

# Ajouter un modèle pour la prévision avec des carrés ( à voir)

###################################################################


#Calculer l'erreur d'estimation de BIC avec don, don1 et don2

don <- readRDS("C:/Users/antoi/Downloads/don (1).RDS")
don1 <- readRDS("C:/Users/antoi/Downloads/don1.RDS")
don2 <- readRDS("C:/Users/antoi/Downloads/don2.RDS")

mod<- lm(don$medv~.,data= don) 
moddon <- step(mod, k= log(nrow(don)),trace =0)        # Modèle BIC
residu <- (moddon$residuals)^2
mean(residu)

mod1<- lm(don1$medv~.,data= don1)
moddon1 <- step(mod1, k= log(nrow(don1)),trace =0)        # Modèle BIC
residu1 <- (moddon1$residuals)^2
mean(residu1)

mod2<- lm(don2$medv~.,data= don2)
moddon2 <- step(mod2, k= log(nrow(don2)),trace =0)        # Modèle BIC
residu2 <- (moddon2$residuals)^2
mean(residu2)


####################### SELECTIONNER QUE LES X PROVENANT DU BIC ##############

### Analyse du modèle choisi

#Je pense qu'il vaut mieux travailler avec la base de données restreinte aux variables sélectionnées

var <- names(modBIC$coefficients)[-1] # on enlève l'intercept
don2 <- don[,c("maxO3",var)]

