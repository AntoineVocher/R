
# Définir le dossier de travail (mettre des / )
setwd("C:/Users/antoi/OneDrive/Documents/Data Analyst/Fichiers_Sources_R/DONNEES")

# Importation des données
don <- read.table("ozone_complet.txt",header = TRUE, sep=";", dec =".", row.names = 1) 

don <- na.omit(don)

# ggplot2

library(ggplot2)



####################################### GRAPHIQUE #####################################

# NUAGE DE POINTS entre Y = maxo3 et X = T6

ggplot(don) +
  aes(x = T6, y = maxO3) +
  geom_point(shape = "circle", size = 1.5, colour = "#112446") +
  labs(x = "T6", y = "maxo3", title = "Nuage de Points") +
  theme_minimal()

# Faire un HISTOGRAMME  [Variable Quantitative continue]

hist(don$maxO3, breaks = 15,
     freq = FALSE, 
     main = paste("Histogramme"), 
     xlab = "Pic d'ozone",
     ylab = )


# Faire un DIAGRAMME EN BATON  [Variable Quantitative discrète]

plot(proportions(table (don$Ne9))*100, 
     type = 'h',
     ylab = "Fréquence (%)",
     main = "Fréquence de la couverture nuageuse")


# Faire une BOITE A MOUSATCHE de max03
boxplot(don$maxO3)


####################################### CLASSES #########################################

# Créer des classes en incluant les bornes [10:20] et [20:25]

y<- cut(don$T9, breaks= c(10, 20, 25), include.lowest = TRUE )


# Créer des classes en incluant les bornes automatiques pour un tableau ET le nombre 
y<- cut(don$T9, breaks= c(min(don$T9), 20, max(don$T9)), include.lowest = TRUE)

# Compter le nombre par classe
table(y ,useNA = "always")


################################# CORRELATION ###########################################

# Calculer le coef de corrélation entre le pic d'ozone et la temp à 12h
cor(don$maxO3, don$T12)



## Representer la matrice de corrélation 

library(corrplot)

matrix <-cor(don)
head(round(matrix,2))

corrplot(matrix, method="number",type="lower", title = "La matrice de corrélation")

matrix

summary(matrix)
dim(matrix)


