# Charger les bibliothèques nécessaires
library(tidyverse)
#install.packages("palmerpenguins")
library(caret)
library(MASS)
library(ggplot2)
library(GGally)
library(plotly)

# Charger les données penguins
penguins <- read.table(file.choose(), sep=";", header=TRUE)

# Afficher les premières lignes pour vérifier la structure des données
head(penguins)
str(penguins)
summary(penguins)

# Nettoyer les données (supprimer les lignes avec des valeurs manquantes)
penguins_clean <- na.omit(penguins)

# Statistiques descriptives pour les variables 
summary(penguins_clean)

# Répartition des espèces dans l'échantillon
table(penguins_clean$species)

# ANOVA pour comparer les espèces
# ANOVA pour la longueur du bec (bill_length_mm)
summary(aov(bill_length_mm ~ species, data = penguins_clean))

# ANOVA pour la profondeur du bec (bill_depth_mm)
summary(aov(bill_depth_mm ~ species, data = penguins_clean))

# ANOVA pour la longueur des nageoires (flipper_length_mm)
summary(aov(flipper_length_mm ~ species, data = penguins_clean))

# ANOVA pour la masse corporelle (body_mass_g)
summary(aov(body_mass_g ~ species, data = penguins_clean))


# Visualisation des données avec des boxplots
# Boxplot pour la longueur du bec (bill_length_mm)
boxplot(bill_length_mm ~ species, data = penguins_clean,
        main = "Longueur du bec par espèce",
        xlab = "Espèce", ylab = "Longueur du bec (mm)",
        col = c("lightblue", "lightgreen", "lightpink"))

# Boxplot pour la profondeur du bec (bill_depth_mm)
boxplot(bill_depth_mm ~ species, data = penguins_clean,
        main = "Profondeur du bec par espèce",
        xlab = "Espèce", ylab = "Profondeur du bec (mm)",
        col = c("lightblue", "lightgreen", "lightpink"))

# Boxplot pour la longueur des nageoires (flipper_length_mm)
boxplot(flipper_length_mm ~ species, data = penguins_clean,
        main = "Longueur des nageoires par espèce",
        xlab = "Espèce", ylab = "Longueur des nageoires (mm)",
        col = c("lightblue", "lightgreen", "lightpink"))

# Boxplot pour la masse corporelle (body_mass_g)
boxplot(body_mass_g ~ species, data = penguins_clean,
        main = "Masse corporelle par espèce",
        xlab = "Espèce", ylab = "Masse corporelle (g)",
        col = c("lightblue", "lightgreen", "lightpink"))

# Créer un graphique en matrice de dispersion
pairs(penguins_clean[, c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")], 
      main = "Données Penguins",
      col = as.factor(penguins_clean$species),
      pch = 19)


# Tracer la longueur du bec vs la longueur des nageoires
ggplot(penguins_clean, aes(x = bill_length_mm, y = flipper_length_mm, color = species)) +
  geom_point(size = 3) +
  labs(title = "Apprentissage : Longueur du bec vs longueur des nageoires",
       x = "Longueur du bec (mm)",
       y = "longueur des nageoires (mm)",
       color = "Espèce") +
  theme_minimal()


# Analyse Discriminante Linéaire (LDA)
lda_penguins <- lda(species ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g, 
                    data = penguins_clean)

# Afficher les résultats de l'analyse
print(lda_penguins)

# Prédire les scores LDA pour chaque individu
lda_values <- predict(lda_penguins)

# Ajouter les scores aux données
penguins_clean$LD1 <- lda_values$x[, 1]
penguins_clean$LD2 <- lda_values$x[, 2]

# Visualiser les résultats de l'analyse discriminante
ggplot(penguins_clean, aes(x = LD1, y = LD2, label = species, color = species)) +
  geom_text(size = 3) +
  theme_minimal() +
  labs(title = "Analyse Discriminante Linéaire des Manchots",
       x = "LD1",
       y = "LD2")


# Matrice de covariance
matrice_covariance <- cov(penguins_clean[, c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")])
print("Matrice de covariance :")
print(matrice_covariance)

# Calculer la moyenne globale
X_means <- colMeans(penguins_clean[, c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")])
print("Moyenne globale :")
print(X_means)

# Séparer les données par classe (espèce de manchots)
classes <- split(penguins_clean[, c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")], 
                 penguins_clean$species)

# Calculer les moyennes de chaque classe
X_means_h <- lapply(classes, colMeans)
print("Moyennes par classe :")
print(X_means_h)

# Calculer les différences entre les moyennes des classes et la moyenne globale
dif_1 <- X_means_h[[1]] - X_means  # Différence pour Adelie
print("Différence pour Adelie :")
print(dif_1)
dif_2 <- X_means_h[[2]] - X_means  # Différence pour Chinstrap
print("Différence pour Chinstrap :")
print(dif_2)
dif_3 <- X_means_h[[3]] - X_means  # Différence pour Gentoo
print("Différence pour Gentoo :")
print(dif_3)

# Calcul de la matrice de variance-covariance
TOT <- cov(penguins_clean[, 1:4])  # Matrice totale
print(TOT)

# Calculer la matrice de variance interclasse (B)
n <- nrow(penguins_clean)  # Nombre total d'observations
n_classes <- sapply(classes, nrow)  # Nombre d'observations par classe
# Nombre d'observations pour chaque classe 
n1 = n_classes['Adelie']
print(n1)
n2 = n_classes['Chinstrap']
print(n2)
n3 = n_classes['Gentoo']
print(n3)
# Calculer la matrice de variance interclasse (B)
B <- (n1 * (dif_1 %*% t(dif_1)) + 
     (n2 * (dif_2 %*% t(dif_2))) + 
     (n3 * (dif_3 %*% t(dif_3)))) / 
     nrow(donnees)

# Afficher la matrice de variance interclasse
print("Matrice de variance interclasse (B) :")
print(B)



# Calculer la matrice de variance intraclasse (W)
W <- TOT - B
print("Matrice de variance intraclasse (W) :")
print(W)


# Extraction des coefficients des discriminants linéaires
a1 <- matrix(lda_penguins$scaling[,1])
print(a1)

a2 <- matrix(lda_penguins$scaling[,2])
print(a2)

# Calcul de Lambda de Wilks
lambda1 <- (t(a1) %*% B %*% a1) / (t(a1) %*% TOT %*% a1)
print(lambda1)

lambda2 <- (t(a2) %*% B %*% a2) / (t(a2) %*% TOT %*% a2)
print(lambda2)

# Lambda global
lambda <- (1 - lambda1) * (1 - lambda2)
# Affichage des résultats
print(lambda)


# Diviser les données en ensembles d'entraînement et de test
set.seed(123)  # Pour la reproductibilité
train_index <- createDataPartition(penguins_clean$species, p = 0.8, list = FALSE)
train <- penguins_clean[train_index, ]
test <- penguins_clean[-train_index, ]

# Appliquer LDA sur les données d'entraînement
lda_model <- lda(species ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g, 
                 data = train)


# Les données en ensembles d'entraînement
pred <- predict(lda_model, newdata = train )
# Afficher les prédictions
print(pred$class)
# Matrice de confusion
conf_matrix <- table(Prediction = pred$class, Réel = train $species)
print("Matrice de confusion :")
print(conf_matrix)
# Calculer la précision
precision <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Précision :", precision))

# Calculer le taux d'erreur de classification
taux_erreur <- 1 - sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Taux d'erreur de classification :", taux_erreur))



# Prédire sur les données de test
pred1 <- predict(lda_model, newdata = test)

# Afficher les prédictions
print(pred1$class)

# Matrice de confusion
conf_matrix1 <- table(Prediction = pred1$class, Réel = test$species)
print("Matrice de confusion :")
print(conf_matrix1)

# Calculer la précision
precision1 <- sum(diag(conf_matrix1)) / sum(conf_matrix1)
print(paste("Précision :", precision1))

# Calculer le taux d'erreur de classification
taux_erreur1 <- 1 - sum(diag(conf_matrix1)) / sum(conf_matrix1)
print(paste("Taux d'erreur de classification :", taux_erreur1))

# Calculer les distances de Mahalanobis pour chaque espèce
Mahalanobis_distance_adelie <- mahalanobis(test[, c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")], 
                                           X_means_h[[1]], matrice_covariance)
print("Distances de Mahalanobis par rapport à Adelie :")
print(Mahalanobis_distance_adelie)

Mahalanobis_distance_chinstrap <- mahalanobis(test[, c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")], 
                                              X_means_h[[2]], matrice_covariance)
print("Distances de Mahalanobis par rapport à Chinstrap :")
print(Mahalanobis_distance_chinstrap)

Mahalanobis_distance_gentoo <- mahalanobis(test[, c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")], 
                                           X_means_h[[3]], matrice_covariance)
print("Distances de Mahalanobis par rapport à Gentoo :")
print(Mahalanobis_distance_gentoo)





# Créer une nouvelle observation
new_observation <- data.frame(
  bill_length_mm = 45.2,  # Longueur du bec
  bill_depth_mm = 16.8,   # Profondeur du bec
  flipper_length_mm = 195, # Longueur des nageoires
  body_mass_g = 4200      # Masse corporelle
)

# Afficher la nouvelle observation
print(new_observation)

# Prédire l'espèce pour la nouvelle observation
new_prediction <- predict(lda_model, newdata = new_observation)

# Afficher la prédiction
print(new_prediction$class)  # Espèce prédite
print(new_prediction$posterior)  # Probabilités a posteriori pour chaque espèce


