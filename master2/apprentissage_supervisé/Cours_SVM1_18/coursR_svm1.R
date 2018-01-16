# 1. Charger le jeu de données dans R. Afficher les données d'apprentissage.

rm(list = ls())
data <- read.table(file = "C:/Users/pc/Documents/Cours/App-sup/Course/Course_17_afaire/Cours5_afaire_SVM/CoursSVM18/data4/synth_train.txt", header = TRUE)
dim(data)
## [1] 100 3
X <- as.matrix(data[, -1])
Y <- as.factor(data$y)
plot(data[, -1], pch = data$y, col = data$y)
legend("topright", legend = c("classe 1", "classe 2"), pch = 1:2, col = 1:2)

## 2. Charger (ou installer puis charger le package e1071) et consulter l'aide de la fonction svm.
# install.packages('e1071')
library(e1071)
help(svm)
##3. Appliquer la méthode des SVM avec un noyau linéaire (on utilisera la méthode par formule, du type
##                                                       y  ., data=..., pour pouvoir utiliser les fonctions graphiques dans la suite).
data$y <- as.factor(data$y)
svm.lin <- svm(y ~ ., data = data, kernel = "linear")
svm.lin

##4. Calculer l'erreur d'apprentissage du prédicteur obtenu. Charger les données test puis calculer son
## erreur test.
svm.lin.pred <- predict(object = svm.lin, newdata = data)
sum(svm.lin.pred != Y)/length(Y)

data_test <- read.table(file = "C:/Users/pc/Documents/Cours/App-sup/Course/Course_17_afaire/Cours5_afaire_SVM/CoursSVM18/data4/synth_test.txt", header = TRUE)
dim(data_test)

head(data_test)

X_test <- as.matrix(data_test[, -1])
Y_test <- as.factor(data_test$y)
svm.lin.test <- predict(object = svm.lin, newdata = data_test)
sum(svm.lin.test != Y_test)/length(Y_test)

## 5. En utilisant la fonction plot appliquée au résultat du SVM précédent, tracer la frontière de décision
## du SVM linéaire.
plot(svm.lin, data = data, grid = 200)

## 6. Faire la même chose pour des SVM avec noyaux polynomiaux de degrés 2, 3, 4, 5.
svm.deg2 <- svm(y ~ ., data = data, kernel = "polynomial", degree = 2)
svm.deg2

svm.deg2.pred <- predict(svm.deg2, data)
sum(svm.deg2.pred != Y)/length(Y)
## [1] 0.14
svm.deg2.test <- predict(svm.deg2, data_test)
sum(svm.deg2.test != Y_test)/length(Y_test)
## [1] 0.165
plot(svm.deg2, data, grid = 200)

svm.deg3 <- svm(y ~ ., data = data, kernel = "polynomial", degree = 3)
svm.deg3

svm.deg3.pred <- predict(svm.deg3, data)
sum(svm.deg3.pred != Y)/length(Y)
## [1] 0.05
svm.deg3.test <- predict(svm.deg3, data_test)
sum(svm.deg3.test != Y_test)/length(Y_test)
## [1] 0.06
plot(svm.deg3, data, grid = 200)

svm.deg4 <- svm(y ~ ., data = data, kernel = "polynomial", degree = 4)
svm.deg4

svm.deg4.pred <- predict(svm.deg4, data)
sum(svm.deg4.pred != Y)/length(Y)
## [1] 0.13
svm.deg4.test <- predict(svm.deg4, data_test)
sum(svm.deg4.test != Y_test)/length(Y_test)
## [1] 0.19
plot(svm.deg4, data, grid = 200)

svm.deg5 <- svm(y ~ ., data = data, kernel = "polynomial", degree = 5)
svm.deg5

svm.deg5.pred <- predict(svm.deg5, data)
sum(svm.deg5.pred != Y)/length(Y)
## [1] 0.04
svm.deg5.test <- predict(svm.deg5, data_test)
sum(svm.deg5.test != Y_test)/length(Y_test)
## [1] 0.065
plot(svm.deg5, data, grid = 200)

svm.gaus <- svm(y ~ ., data = data)
svm.gaus

svm.gaus.pred <- predict(svm.gaus, data)
sum(svm.gaus.pred != Y)/length(Y)
## [1] 0.04
svm.gaus.test <- predict(svm.gaus, data_test)
sum(svm.gaus.test != Y_test)/length(Y_test)

plot(svm.gaus, data = data, grid = 200)



