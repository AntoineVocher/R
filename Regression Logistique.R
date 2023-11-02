# Définir le dossier de travail (mettre des / )
setwd("C:/Users/antoi/OneDrive/Documents/Data Analyst/Fichiers_Sources_R/DONNEES")


######################################### REGRESSION LOGISTIQUE ##################################

# Imporation des données
don<- read.table("heart.csv",header = TRUE, sep=";", dec =".", stringsAsFactors = TRUE)

summary(don)

str(don)

dim(don)

# Transforme en factor la variable à expliquer, puis on créer une nouvelle variable Y égale à celle ci , et on enlève l'ancienne 

don$chd <- factor(don$chd)
don$Y <- don$chd
don$chd <- NULL

# Y est un facteur et est égale à la valeur à expliquer chd ( enlever chd !!!)


###################################### ESTIMATION ######################################
# Modélisation : on cherche à estimer p tel que Y=1

model <- glm(Y~.,data =don, family= "binomial") # Modèle complet
summary(model)

modAIC <- step(model,trace=0) # Modèle AIC
summary(modchoisi)

modBIC <- step(model,k=log(nrow(don)),trace=0) # Modèle BIC
summary(modBIC)

## Le meilleur modèle d'estimation est celui qui a l'AIC le plus faible 


############################### QUALITE DU MODELE (ESTIMATION) #############################

######## Analyse des Résidus 

# On suppose que les résidus suivent un loi normale

residu <-rstudent(modAIC)
plot(rstudent(modAIC))
abline(h=c(-2,0,2), col=c(2,1,2))

# Conclusion : Il n'y a pas de structure, les résidus sont aléatoires


######## POINTS ABBERANTS
## DEF : Residus situés au dessus de 2 et en dessous de -2

sum(abs(residu)>2)/nrow(don)*100 # Proportion des points dits aberrants 
min(residu) # le point le plus aberrant
pnorm(min(residu)) # Probabilité que le point soit aberrant


point_ab = which.max(abs(residu))
point_ab = don[which.max(abs(residu)),]
point_ab

######## LEVIERS (individus)
## DEF : 


levier <-hatvalues(modAIC) 
plot(levier) # Afficher les points leviers

## on a un 

point_levier <-don[which.max(levier),] #  Qui est il ? 
ou 
point_levier <-don[which.max(levier),c("T6","T12","T15","Ne9","Ne12","Vvit6","Vvit9","Vvit15","Vvit18","Vx","maxO3v")]

point_levier
# Comparaison avec la moyenne de chaque colonne numériques

names(don) # nom des colonnes de don
str(don) # les colonnes numériques 

round(colMeans(don[,c("sbp","tobacco","ldl","adiposity","typea","obesity","alcohol","age")]),1)
point_levier



plot(sort((levier), dec=TRUE)[1:20],type = "h") # Afficher les points leviers

######## ERREURS ESTIMATION
# Table de confusion

estimation <- predict(mod1, type = "response")

yestime <- cut(estimation, breaks=c(0,0.5,1), labels = c(0,1) )
table(yestime, Yvrai=don1$Y)


precision_M <- 100-sum(diag(table( don1$Y, yestime)))/nrow(don)*100



################################# VALIDATION CROISEE DESC ################################# 

don<- read.table("heart.csv",header = TRUE, sep=";", dec =".", stringsAsFactors = TRUE)

don$chd <- factor(don$chd)
don$Y <- don$chd
don$chd <- NULL

summary(don)

K=10
set.seed(1234)
blocs=sample(rep(1:K,length=nrow(don)))
PREV <- data.frame(Y=don$Y,complet=NA,AIC=NA,BIC=NA)
for(ii in 1:K){
  print(ii)
  donA = don[blocs!=ii,]
  donT = don[blocs==ii,]
  ###################
  mod1 <- glm(Y~.,data=donA,family= "binomial")
  PREV[blocs==ii,"complet"]=predict(mod1,donT, type = "response")
  ###################
  mod2 <- step(glm(Y~.,data=donA,family= "binomial"),trace=0)
  PREV[blocs==ii,"AIC"]=predict(mod2,donT,type = "response")
  ###################
  mod3 <- step(glm(Y~.,data=donA,family= "binomial"),trace=0,k=log(nrow(donA)))
  PREV[blocs==ii,"BIC"]=predict(mod3,donT,type = "response")
}
summary(PREV)


######## COMPARAISON DE MODELE après les prévisions des probabilités

#### TABLE DE CONFUSION à partir des Y* déterminés à partir d'un seuil à 0.5
library(pROC)

rocglm <- roc(don$Y, PREV$complet)
rocAICf <- roc(don$Y, PREV$AIC)
rocBIC <- roc(don$Y, PREV$BIC)


#Détermination des Y avec un seuil de 0.5
Yesglm <- cut(PREV$complet, breaks = c(0,0.5,1), labels = c("Oest","1est"))
YesAICf <- cut(PREV$AIC, breaks = c(0,0.5,1), labels = c("Oest","1est"))
YesBIC <- cut(PREV$BIC, breaks = c(0,0.5,1), labels = c("Oest","1est"))

#table de confusion
table( don$Y, Yesglm)
table( don$Y, YesAICf)
table( don$Y, YesBIC)


## COMPARAISON DES MODELES 

# Courbe ROC , la mieux est celle proche de l'angle droit

plot(rocglm , legend = "complet")   # Noir
plot(rocAICf, col=2, add=TRUE , legend = "AIC")     #Rouge
plot(plot(rocBIC, col= 3, add=TRUE), col=3, add=TRUE, legend = "BIC") #Verte


# ACCURACY, SPECIFICITY & SENSITIVITY (meilleur taux > Meilleur modèle)

coords(rocglm, x=0.5, ret = c("threshold","accuracy","sensitivity","specificity"))
coords(rocAICf, x=0.5, ret = c("threshold","accuracy","sensitivity","specificity"))
coords(rocBIC, x=0.5, ret = c("threshold","accuracy","sensitivity","specificity"))

## PRECISION  = Nombre de fois où on se trompe (faible précision >>> Meilleur modèle)

precision_complet <- 100-sum(diag(table( don$Y, Yesglm)))/nrow(don)*100
precision_AIC <- 100-sum(diag(table( don$Y, YesAICf)))/nrow(don)*100
precision_BIC <- 100-sum(diag(table( don$Y, YesBIC)))/nrow(don)*100

dm <- data.frame("complet"=precision_complet , "AIC" = precision_AIC , "BIC" = precision_BIC)


names(dm)[apply(dm==min(dm),FUN=sum,MARGIN=2)>0] # le modèle chosi










###########################################################"  
################### LEAVE ONE OUT - LOGISTIQUE #########################
################### #######################################

don<- read.table("heart.csv",header = TRUE, sep=";", dec =".", stringsAsFactors = TRUE)

don$chd <- factor(don$chd)
don$Y <- don$chd
don$chd <- NULL
varsel <- list()

# Leave and out : complet, bic  

nb <- nrow(don)
set.seed(1234)
PREV <- data.frame(Y=don$Y)
for(ii in 1:nb){
  
  ######################## Toutes les variables ##########################
  
  donA=don[-ii,]
  donT=don[ii,]
  print(ii)
  #########################
  modcomplet <- glm(Y~.,data=donA,family="binomial")
  PREV[ii,"complet"] <- predict(modcomplet,donT,type="response")
  ##########################
  modBIC <- step(glm(Y~.,data=donA,family= "binomial"),trace=0,k=log(nrow(donA)))
  PREV[ii,"BIC"] <- predict(modBIC,donT,type="response")
  
  varsel [[ii]] <- names(coefficients(modcomplet))[-1]
}





















