# 2.1 Description des données prostate.
# A. Charger le jeu de données dans R, décrire le jeu de données prostate 
# Description de la table prostate 
library(ElemStatLearn)
data(prostate) 

# Information sur la table prostate 
?prostate 

# attacher la table prostate 
attach(prostate) 

# Binariser la réponse lpsa ; high si lpsa > median(lpsa) et 0 sinon 
g <- factor(ifelse(lpsa > median(lpsa), "high", "low")) 

# utiliser les 3 variables explicatives lcavol, lweight and age 
# pour modéliser les classes g et tracer le nuage des points 
# de ces trois variables en indiquant les classes 
library(lattice) 
splom(~prostate[,1:3], groups=g) 

# Ou utiliser la fonction pairs 
pairs(prostate[,1:3], col=as.numeric(g))


# 2.2 Régression linéaire 
# B. Pourquoi la régression linéaire n'est pas adaptée!? 
# régression linéaire sur une variable indicatrice (binaire) 
y <- ifelse(g=="high", 1, 0) 

# Calculer la régression linéaire 
lm.fit <- lm(y~lcavol+lweight+age) 

# coefficients de régression
lm.beta <- lm.fit$coef 

# tracer le modèle estimé lcavol et age pour lweight moyen 
b <- -lm.beta[2]/lm.beta[4] 
a <- (0.5 - lm.beta[1] - lm.beta[3]*mean(lweight))/lm.beta[4] 
plot(lcavol, age, col=g) 
abline(a,b) 

# prédiction de y 
yhat <- predict(lm.fit) 

# prediction de la classe g 
lm.ghat <- factor(ifelse(yhat > 0.5, "high", "low")) 

# nombre d'exemples mal classés 
sum(lm.ghat != g) 

# erreur de classification 
mean(lm.ghat != g) 

# matrice de confusion 
table(lm.ghat, g)


# 2.3 KNN 
# C. On s’intéresse d’abord à la méthodologie du choix de k 
# (1) Créer un jeu de données de données d’apprentissage (75% des données) 
# et un jeu de données test (25% des données) avec le code suivant. 
set.seed(30) 
X=cbind(g, lcavol,lweight,age) 
tr <- sample(1:nrow(X),72) 
Xtrain <- X[tr,]
Xtest <- X[-tr,] 

# (2) Calculer les taux d’erreur sur les données test pour k variant de 1 à 100. 
# Avec la fonction plot, représenter ce taux d’erreur test en fonction de k 
# (contrôler que l’abscisse du graphique partde 0). 
# Avec la fonction which.min, trouver le nombre de voisins qui donne la plus petite erreur test. 
library(class) 
kmax=100 
err_test <- rep(NA,kmax) 
for (k in 1:kmax) { 
  pred <- knn(Xtrain[,-1], Xtest[,-1], Xtrain[,1], k) # knn(train, test, train_label, k)
  err_test[k] <- sum(pred!=Xtest[,1])/length(Xtest[,1]) 
} 
lim <- c(0,max(err_test)) 
plot(err_test,type="l",ylim=lim,col=2,xlab="nombre de voisins", 
     ylab= "taux d'erreur") 
which.min(err_test) 


# (3) Recommencer avec un autre découpage aléatoire apprentissage/test 
# et représenter la courbe d’évolution du taux d’erreur test sur le même graphique 
# qu’à la question précédente.
set.seed(10) 
tr <- sample(1:nrow(X),90) 
Xtrain <- X[tr,] 
Xtest <- X[-tr,] 
for (k in 1:kmax) { 
  pred <- knn(Xtrain[,-1],Xtest[,-1], Xtrain [,1],k) 
  err_test[k] <- sum(pred!=Xtest[,1])/length(Xtest[,1])
} 
points(err_test,type="l",col=4) 
legend("bottomright", legend=c("decoupage 1", "decoupage 2"), lty=1, col=c(2,4))


# (4) Exécuter le code suivant et faire un choix pour k. 

B<- 20 
kmax <- 100 
err_test <- matrix(NA,kmax,B) 
for (b in 1:B) { 
  tr <- sample(1:nrow(X),90) 
  Xtrain <- X[tr,] 
  Xtest <- X[-tr,] 
  for (k in 1:kmax) 
  { 
    pred <- knn(Xtrain[,-1],Xtest[,-1], Xtrain [,1],k) 
    err_test[k,b] <- sum(pred!= Xtest[,1])/length(Xtest[,1]) 
  }
} 
mean_err_test <- apply(err_test,1,mean) 
lim <-c(0,max(err_test)) 
matplot(err_test,type="l",lty=2,col=2,ylim=lim, xlab="nombre de voisins",ylab="taux d'erreur") 
matpoints(mean_err_test,type="l",col=2,lwd=4) 
legend("bottomright", legend=c("Erreur moyenne", "Erreurs conditionnelles"), 
       lty=c(1,3),lwd=c(4,2),col=c(2,2))
which.min(mean_err_test)

# (5) Choisir maintenant le nombre k de voisin en utilisant par 
# validation croisée (cross validation) leave-one-out (LOO) avec la fonction knn.cv. 
?knn.cv  # Default: leave-one-out
err_test <- rep(NA,kmax) 
for (k in 1:kmax) { 
  pred <- knn.cv(X[,-1], X[,1],k) 
  err_test[k] <- sum(pred!= X[,1])/length(X[,1]) 
} 
lim <-c(0,max(err_test)) 
plot(err_test,type="l",col=2,ylim=lim,xlab="nombre de voisins", ylab="taux d'erreur") 
points(mean_err_test,type="l",col=4,lwd=1) 
legend("bottomright", legend=c("Erreur loo", "Erreur moyenne"), col=c(2,4),lty=1)
which.min(err_test)
# (6) Faire un petit bilan méthodologique concernant le choix du paramètre k. 

# D. On veut maintenant non seulement choisir k mais également avoir une idée 
# de l’erreur de prédiction de ce classifieur. Pour cela, il faut utiliser 
# des données n’ayant jamais été utilisées. Les données doivent donc être 
# découpées en trois parties : apprentissage/validation/test .

# (1) Couper aléatoirement les données des deux parties : 
# un ensemble "apprentissage-validation" (75 % des données) 
# et un ensemble test de taille (25% des données). 
set.seed(30) 
tr <- sample(1:nrow(X),72) 
Xtrainval <- X[tr,] 
Xtest <- X[-tr,] 

# (2) Utiliser la première approche pour choisir k sur l’ensemble "apprentissage-validation" : 
# i. Choisir k en découpant les 945 données de l’ensemble "apprentissage-validation" en deux 
# parties : une partie "apprentissage" (50% des données) et une partie "validation" 
# (25 % des données). Choisir k qui minimise le taux d’erreur moyen sur les 
#ensembles de validations de B = 25 découpages. 
B <- 25 
kmax <- 50
err_valid <- matrix(NA,kmax,B) 
for (b in 1:B) { 
  tr <- sample(1:nrow(Xtrainval),36) 
  Xtrain <- Xtrainval[tr,] 
  Xvalid <- Xtrainval[-tr,] 
  for (k in 1:kmax) 
  { 
    pred <- knn(Xtrain[,-1],Xvalid[,-1],Xtrain[,1],k) 
    err_valid[k,b] <- sum(pred!=Xvalid[,1])/length(Xvalid[,1]) 
  } 
} 
mean_err_valid <- apply(err_valid,1,mean) 
plot(mean_err_valid,type="l")

# ii. Constuire le classifieur avec ce nombre de voisins 
# sur l’ensemble "apprentissage-validation" et calculer le taux d’erreur des données test. 
pred <- knn(Xtrainval[,-1],Xtest[,-1],Xtrainval[,1],k=which.min(mean_err_valid)) 
sum(pred!=Xtest[,1])/length(Xtest[,1]) 

# (3) Utiliser la seconde approche pour choisir k par validation croisée LOO 
# sur l’ensemble "apprentissage validation". Calculer ensuite le taux d’erreur des données test. 
err_valid <- rep(NA,kmax) 
for (k in 1:kmax) 
{ 
  pred <- knn.cv(Xtrainval[,-1],Xtrainval[,1],k) 
  err_valid[k] <- sum(pred!=Xtrainval[,1])/length(Xtrainval[,1]) 
} 
which.min(err_valid) 
pred <- knn(Xtrainval[,-1],Xtest[,-1],Xtrainval[,1],k=which.min(err_valid)) 
sum(pred!=Xtest[,1])/length(Xtest[,1])

# E. Pour les courageux, on pourrait recommencer avec plusieurs découpages 
# des données en deux parties "apprentissage-validation" et "test". 
# Cela permettrait d’avoir une erreur test moyenne, et une idée de sa variabilié. 
# C’est assez rapide à faire avec la méthode de LOO pour le choix de k. 
B <- 10 
kmax <- 50 
err_valid <- rep(NA,kmax) 
err_test <- rep(NA,B) 
for (b in 1:B) 
{ 
  tr <- sample(1:nrow(X),72) 
  Xtrainval <- X[tr,] 
  Xtest <- X[-tr,] 
  for (k in 1:kmax) 
  {
    pred <- knn.cv(Xtrainval[,-1],Xtrainval[,1],k) 
    err_valid[k] <- sum(pred!=Xtrainval[,1])/length(Xtrainval[,1]) 
  } 
  pred <- knn(Xtrainval[,-1],Xtest[,-1],Xtrainval[,1],k=which.min(err_valid)) 
  err_test[b] <- sum(pred!=Xtest[,1])/length(Xtest[,1]) 
} 
boxplot(err_test,main="Erreurs test pour 50 decoupages")

# 2.4 Bayésien naïf 

# 1. Appliquer le classifieur byésien naïf sur la table X. 

prostate.d<-prostate[, -c(9,10)] # ensemble apprentissage 
## Utiliser le pakcage e1071 
library(e1071) 
m <- naiveBayes(g ~ ., data = prostate.d) 
## alternativement: 
m <- naiveBayes(prostate.d, g) 
m 
table(predict(m, prostate.d), g) 



