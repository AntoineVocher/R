
# Définir le dossier de travail (mettre des / )
setwd("C:/Users/antoi/OneDrive/Documents/Data Analyst/Fichiers_Sources_R/DONNEES")

beton <- read.table("SlumpTest_NA.txt",header = TRUE, sep="", dec =".",
                    row.names = 1,stringsAsFactors =TRUE )


####################################--------Manipulation des Valeurs Manquantes----------- 

# Repérer la présence de NA


summary(beton) # Repérer au global la présence de données manquantes

sum(apply(is.na(beton),1,sum)) # Nombre de données manquantes

apply(is.na(beton),2,sum) # Nombre de données manquantes par variables 

which.max(apply(is.na(beton),2,sum)) # La variable qui en a le plus 

apply(is.na(beton),1,sum) # Nombre de données manquantes par individus

which.max(apply(is.na(beton),1,sum)) # L'individu en a le plus

indNA <- which(is.na(beton)) # La position des données manquantes dans le dataframe

indNA_Cement <- which(is.na(beton$Cement)) # La position des données manquantes pour une variable

beton_2 <- beton [-indNA_Cement,] # Supprimer les lignes pour lequel on a des valeurs manquantes pour cette variable

beton <- na.omit(beton) # Enleve les lignes avec des na 

###### AVEC Tydiverse
library(tidyr)

drop_na(beton,Slag) #supprime les lignes avec des NA pour les variables mentionnées

replace_na(beton, list(Slag = 0,Cement = 21.46)) # Remplace les NA par la valeur souhaitée ici 

####################################--------Etude des Valeurs Manquantes----------- 

## L'idée :
# 1/ Connaitre la proportion des NA ?
# >> Statistiques Descriptives
 
# 2/ Quelle est la typologie des NA ( aléatoire :) ou non )
# >> Graphiques avec packages naniar et VIM

# 3/ Existe il une dépendance entre les NA ?
# >> ACM : étudier les associations entre NA 

## Avec ses résultats, on peut passer à la suite si la typologie est aléatoire avec peu de dépendance entre elles

#La Suite :4/ Sélectionner la meilleure méthode d'imputation des NA



############################# 1/ Connaitre la proportion des NA ?
# >> Statistiques Descriptives

#!! Enlever les variables qualitatives avant en créant un nouveau data frame 
beton_num <-beton[,-x]

beton_num <- beton

library(naniar)
library(FactoMineR)
library(ggplot2)

dim(na.omit(beton_num)) # taille du jeu de données complet
pct_miss(beton_num) # pourcentage de données manquantes dans le jeu de données
n_miss(beton_num) # nombre de données manquantes
n_complete(beton_num) # données complètes
pct_complete(beton_num) # pourcentage de données complètes   
miss_var_summary(beton_num) # répartition des données manquantes par variables
miss_case_summary(beton_num) # répartition des données manquantes par individus

gg_miss_var(beton_num,show_pct = TRUE) + ylim(0, 100) # graphique de répartition des données manquantes par variables
gg_miss_case(beton_num) # graphique de répartition des données manquantes par individus

miss_case_table(beton_num) # Nombre d'idividus par nombre de valeurs manquantes 

#############################

############################# 2/Quelle est la typologie des NA ( aléatoire :) ou non )

vis_miss(beton_num) # Vue globale de données manquantes (très intéressant pour définir le dispositif de typologie)
#>> Carré noir bien réparti >> Aléatoire

vis_miss(beton_num, cluster=TRUE) # Clusters d'individus par nombre de données manquantes
vis_miss(beton_num[order(beton_num$Compressive_Strength,na.last=TRUE),]) #Tri sur la variable à expliquer (maxO3) 

res <- summary(VIM::aggr(beton_num,sortVar=TRUE))$combinations
#>> Ligne d'en bas, la combinaison où on a pas de NA : Majoritaire :) 

# Est ce que Compressive_Strength a des valeurs manquantes lorsque Slag est manquant?

ggplot(beton_num,aes(x = Slag,y = Compressive_Strength)) +
  geom_miss_point()

ggplot(beton_num, aes(x = Slag, y = Compressive_Strength)) + 
  geom_miss_point()  + 
  theme_dark()

ggplot(bind_shadow(beton_num),aes(x = Compressive_Strength,fill = Slag_NA)) +
  geom_density()
#>> c'est assez proche => aléatoire 

#############################

############################# 3/ Existe il une dépendance entre les NA ?

# Visualiser dans R avec une analyse des correspondances multiples
# création d'une matrice présence-absence pour réaliser l'ACM 
data_miss <- data.frame(is.na(beton_num))
data_miss <- apply(X=data_miss, FUN=function(x) if(x) "m" else "o", MARGIN=c(1,2))
res.mca <- FactoMineR::MCA(data_miss, graph = F)
plot(res.mca, invis = "ind", title = "graphe des modalités", cex  = 0.5)

##>> LEs o au milieu, espace entre les groupes de variables, variables loin de l'origine (situation rare), variable à expliquer m isolée 



#############################

# Imputation simple
###################


# imputation par la moyenne
#Danger 
#créer de la  distorsion et on peut impacter négativement la relation qui existe entre x et y (casser une relation linéaire)


# Imputation multiple
#####################

# On teste différentes méthode d’imputation pour choisir celle qui minimise le fmi et lambda lors de la modélisation

# On fait du multiple => modeliser (inférence)


## imputation multiple par ACP (multiple car objectif de modélisation)
nb <- missMDA::estim_ncpPCA(dta,ncp.max=5) # estimation du nb de composantes
res.mipca <- missMDA::MIPCA(dta, scale = TRUE, ncp=nb$ncp,nboot=100)
plot(res.mipca)
# on observe une certaine variabilité entre les différentes imputations
# côté individus, malgré qq ellispes assez grandes, les points restent dans leur zone sans vraiment changer l'interprétation









####  Plusieurs méthodes : 
#  missMDA (ACM) >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> res.pool
#  missMDA (ACM) avec selection de variable>>>>>>>>> res.pool_AIC
#  mice >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> res.mice
#  mice avec selection de variable >>>>>>>>>>>>>>>>> res.mice_AIC
#  Amelia >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> res.pool.amelia

dta <- beton_num

## Modelisation 

# missMDA (ACM)  >> res.pool

imp <- missMDA::prelim(res.mipca,dta)
res.lm <- with(imp, 
               lm(Compressive_Strength~Cement+Slag+Fly_ash+Water+SP+Coarse_Aggr+Fine_Aggr))
res.pool<-mice::pool(res.lm)
summary(res.pool)
res.pool

#  missMDA (ACM) avec selection de variable >> res.pool_AIC

expr <- expression(f1<-lm(Compressive_Strength~1),
                   f2<- step(f1,scope=list(upper=~Cement+Slag+Fly_ash+Water+SP+Coarse_Aggr+Fine_Aggr),
                             lower=~1))
fit <- with(imp,expr)
formulas <- lapply(fit$analyses,formula)
terms <- lapply(formulas, terms)
vars <- unlist(lapply(terms,labels))
table(vars) 
res.lm <- with(imp, 
               lm(Compressive_Strength~Cement+Fly_ash+Water+Coarse_Aggr))
res.pool_AIC<-mice::pool(res.lm)
summary(res.pool_AIC)
res.pool_AIC


#  mice

dta_impute_mice <- mice(dta,m=100)
res.micelm <- with(dta_impute_mice, 
                   lm(Compressive_Strength~Cement+Slag+Fly_ash+Water+SP+Coarse_Aggr+Fine_Aggr))
res.mice<-mice::pool(res.micelm)
summary(res.mice)
res.mice

# mice avec selection de variable

res.micelm <- with(dta_impute_mice, 
                   lm(Compressive_Strength~Cement+Fly_ash+Water+Coarse_Aggr))
res.mice_AIC<-mice::pool(res.micelm)
summary(res.mice_AIC)
res.mice_AIC


#  Amelia


library(Amelia)
dat.amelia <- Amelia::amelia(dta,m=100)
Amelia::compare.density(dat.amelia,var="Compressive_Strength")
Amelia::compare.density(dat.amelia,var="SP")

res.amelia.DF <- lapply(dat.amelia$imputations,as.data.frame)
res.lm.amelia <- lapply(res.amelia.DF,lm,
                        formula="Compressive_Strength~Cement+Fly_ash+Water+Coarse_Aggr")
res.lm.amelia <- as.mira(res.lm.amelia)
res.pool.amelia<-mice::pool(res.lm.amelia)
summary(res.pool.amelia)
res.pool.amelia


## Compare les 5 méthodes ( on choisit celle avec le plus faible fmi)

res.pool
res.pool_AIC
res.mice
res.mice_AIC
res.pool.amelia

## Le meilleur est la méthode d'imputation par l'ACM avec séléction de variable 
## Enlever des variables peut créer de l'incertitude
