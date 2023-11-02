#############################" SELECTION DE VARIABLES ################################

################### Nettoyage des données  ##############################################



# Définir le dossier de travail (mettre des / )
setwd("C:/Users/antoi/OneDrive/Documents/Data Analyst/Fichiers_Sources_R/DONNEES")


ozone <- read.table("ozone_complet.txt",header = TRUE, sep=";", dec =".", row.names = 1) 
don <- na.omit(ozone)

don <- don
summary(don)

#################### REGRESSION MULTIPLE ###########################################
################### VALIDATION CROISEE par BLOC descendant ########################"


#les RES récupère les variables sélectionnées pour tous les modèles

don$Y = don$maxO3
don$maxO3 = NULL
RES = NULL
K=10
set.seed(12345)
blocs=sample(rep(1:K,length=nrow(don)))
PREV <- data.frame(Y=don$Y,complet=NA,AIC=NA,BIC=NA)
for(ii in 1:K){
  print(ii)
  donA = don[blocs!=ii,]
  donT = don[blocs==ii,]
  ################### COMPLET
  mod1 <- lm(Y~.,data=donA)                                                     ## Complet
  PREV[blocs==ii,"complet"]=predict(mod1,donT)
  RES <- c(RES,names(mod1$coefficients)[-1])
  ################### AIC
  mod2 <- step(lm(Y~.,data=donA),trace=0)                                       ## AIC descendant
  PREV[blocs==ii,"AIC"]=predict(mod2,donT)
  RES <- c(RES,names(mod2$coefficients)[-1])
  ################### BIC
  mod3 <- step(lm(Y~.,data=donA),trace=0,k=log(nrow(donA)))                     ## BIC descendant
  PREV[blocs==ii,"BIC"]=predict(mod3,donT)
  RES <- c(RES,names(mod3$coefficients)[-1])
  
  } 


#Calcul des erreurs  
erreur <- function(X,Y){mean((X-Y)^2)}
apply(PREV,2,erreur,Y=PREV[,1])


## Tableau représentant l'ensemble des variables les plus utilisées dans tous les modèles 
sort(table(RES))
 

#################### REGRESSION MULTIPLE ###########################################
################### VALIDATION CROISEE par BLOC ascendant ########################"

# Définir le dossier de travail (mettre des / )
setwd("C:/Users/antoi/OneDrive/Documents/Data Analyst/Fichiers_Sources_R/DONNEES")


ozone <- read.table("ozone_complet.txt",header = TRUE, sep=";", dec =".", row.names = 1) 
don <- na.omit(ozone)

don <- don
summary(don)

#RES récupère pour les variables sélectionnées pour tous les modèles 

don$Y = don$maxO3
don$maxO3 = NULL
RES = NULL
K=10
set.seed(12345)
blocs=sample(rep(1:K,length=nrow(don)))
PREV <- data.frame(Y=don$Y,complet=NA,AIC_asc =NA , BIC_asc =NA)
for(ii in 1:K){
  print(ii)
  donA = don[blocs!=ii,]
  donT = don[blocs==ii,]
  ################### COMPLET
  mod1 <- lm(Y~.,data=donA)                                                     ## Complet
  PREV[blocs==ii,"complet"]=predict(mod1,donT)
  RES <- c(RES,names(mod1$coefficients)[-1])
  ################### AIC
  modcst <- lm(Y~1,data=donA)                                                   ## AIC ascendant
  mod4 <- step(modcst, 
               scope = list (lower = modcst , upper =mod1 ),
               direction = "forward", trace =0)
  PREV[blocs==ii,"AIC_asc"]=predict(mod4,donT)
  RES <- c(RES,names(mod4$coefficients)[-1])
  ################### BIC
  mod5 <- step(modcst,                                                          ## BIC ascendant
               scope = list (lower = modcst , upper =mod1 ),
               direction = "forward", trace =0,k=log(nrow(donA)))
  PREV[blocs==ii,"BIC_asc"]=predict(mod5,donT)
  RES <- c(RES,names(mod5$coefficients)[-1])
  
} 


#Calcul des erreurs  
erreur <- function(X,Y){mean((X-Y)^2)}
apply(PREV,2,erreur,Y=PREV[,1])


## Tableau représentant l'ensemble des variables les plus utilisées dans tous les modèles 
sort(table(RES))


#################### REGRESSION MULTIPLE ###########################################
################### VALIDATION CROISEE LEAVE ONE OUT ########################"

# Définir le dossier de travail (mettre des / )
setwd("C:/Users/antoi/OneDrive/Documents/Data Analyst/Fichiers_Sources_R/DONNEES")

don <- read.table("ozone_complet.txt",header = TRUE, sep=";", dec =".", row.names = 1) 
don <- na.omit(don)

summary(don)

#RES récupère pour les variables sélectionnées pour tous les modèles 


don$Y = don$maxO3
don$maxO3 = NULL
RES = NULL

nb <- nrow(don)
set.seed(1234)
PROB <- data.frame(Y=don$Y,complet=NA ,BIC =NA)
for(ii in 1:nb){
  
  donA=don[-ii,]
  donT=don[ii,]
  print(ii)
  ################## Complet
  mod1 <- lm(Y~.,data=donA)                                                     ## Complet
  PROB[blocs==ii,"complet"]=predict(mod1,donT)
  RES <- c(RES,names(mod1$coefficients)[-1])

  ################# BIC
  mod3 <- step(lm(Y~.,data=donA),trace=0,k=log(nrow(donA)))                     ## BIC descendant
  PROB[blocs==ii,"BIC"]=predict(mod3,donT)
  RES <- c(RES,names(mod3$coefficients)[-1])
  
   }


#Calcul des erreurs  
erreur <- function(X,Y){mean((X-Y)^2)}
apply(PROB,2,erreur,Y=PROB[,1])


## Tableau représentant l'ensemble des variables les plus utilisées dans tous les modèles 
sort(table(RES))


















