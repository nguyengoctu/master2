# 3. Données Spam 
# Le jeu de données SPAM est une base de données e-mail, 
# avec 4601 observations et 58 variables descriptives. 

# 1. Obtenir une description de la table spam 
# (utiliser le package{kernlab} pour récupérer le jeu de données) 
library(kernlab)
data("spam")
?spam
attach(spam)

# 2. Effectuer une première analyse statistique univariée et bivariée de la table spam 
# statistique descriptive
# univariée
summary(spam)

boxplot(spam)

# bivariée
library(lattice) 
g = spam[, 58]

pairs(spam[, 55:57], col = as.numeric(g))
pairs(spam[,49:54], col=as.numeric(g))
# 3. Réaliser une étude comparative des méthodes de classification suivantes: 
# régression linéaire, k plus proches voisins et le classifieur bayésien naïf 
# sur le jeu de données spam. 

# Case 1
X = spam[, -58]

# Case 2
X = spam[, -(55:58)]

# Case 3
X = spam_normalized[, -58]

g = factor(spam[, 58])

#spam_normalized = scale(spam[, -58], center = T, scale = T)
#X = cbind(spam_normalized, spam[, 58])
# X = as.matrix(X)

# Régression linéaire 
# B. Pourquoi la régression linéaire n'est pas adaptée!? 
# régression linéaire sur une variable indicatrice (binaire) 


y <- ifelse(g=="spam", 1, 0)

# Calculer la régression linéaire 
lm.fit <- lm(y~., data = X)

# coefficients de régression
lm.beta <- lm.fit$coef 

# prédiction de y 
yhat <- predict(lm.fit) 

# prediction de la classe g 
lm.ghat <- factor(ifelse(yhat > 0.5, "spam", "nonspam")) 

# nombre d'exemples mal classés 
sum(lm.ghat != g) 

# erreur de classification 
mean(lm.ghat != g) 

# matrice de confusion 
table(lm.ghat, g)

# spam_normalized = ?

# KNN 
# On s’intéresse d’abord à la méthodologie du choix de k 
# (1) Créer un jeu de données de données d’apprentissage (75% des données) 
# et un jeu de données test (25% des données) avec le code suivant. 
set.seed(30) 
num_train = as.integer(nrow(X) * 0.75)
tr <- sample(1:nrow(X), num_train) 

Xtrain <- X[tr,]
ytrain = g[tr]
Xtest <- X[-tr,]
ytest = g[-tr]

# (2) Calculer les taux d’erreur sur les données test pour k variant de 1 à 100. 
# Avec la fonction plot, représenter ce taux d’erreur test en fonction de k 
# (contrôler que l’abscisse du graphique partde 0). 
# Avec la fonction which.min, trouver le nombre de voisins qui donne la plus petite erreur test. 
library(class) 
kmax=100 
err_test <- rep(NA,kmax) 
for (k in 1:kmax) { 
  pred <- knn(Xtrain, Xtest, ytrain, k) # knn(train, test, train_label, k)
  err_test[k] <- sum(pred!=ytest)/length(ytest) 
} 
lim <- c(0,max(err_test)) 
plot(err_test,type="l",ylim=lim,col=2,xlab="nombre de voisins", 
     ylab= "taux d'erreur") 
which.min(err_test) 
min(err_test)

# (3) Recommencer avec un autre découpage aléatoire apprentissage/test 
# et représenter la courbe d’évolution du taux d’erreur test sur le même graphique 
# qu’à la question précédente.
set.seed(10) 
tr <- sample(1:nrow(X), num_train) 
Xtrain <- X[tr,]
ytrain = g[tr]
Xtest <- X[-tr,]
ytest = g[-tr]
for (k in 1:kmax) { 
  pred <- knn(Xtrain,Xtest,ytrain,k) 
  err_test[k] <- sum(pred!=ytest)/length(ytest)
} 
points(err_test,type="l",col=4) 
legend("bottomright", legend=c("decoupage 1", "decoupage 2"), lty=1, col=c(2,4))
min(err_test)
which.min(err_test)

# (4) Exécuter le code suivant et faire un choix pour k. 

B<- 20 
kmax <- 100 
err_test <- matrix(NA,kmax,B) 
for (b in 1:B) { 
  tr <- sample(1:nrow(X),num_train) 
  Xtrain <- X[tr,]
  ytrain = g[tr]
  Xtest <- X[-tr,]
  ytest = g[-tr]
  for (k in 1:kmax) 
  { 
    pred <- knn(Xtrain,Xtest, ytrain,k) 
    err_test[k,b] <- sum(pred!= ytest)/length(ytest) 
  }
} 
mean_err_test <- apply(err_test,1,mean) 
lim <-c(0,max(err_test)) 
matplot(err_test,type="l",lty=2,col=2,ylim=lim, xlab="nombre de voisins",ylab="taux d'erreur") 
matpoints(mean_err_test,type="l",col=2,lwd=4) 
legend("bottomright", legend=c("Erreur moyenne", "Erreurs conditionnelles"), 
       lty=c(1,3),lwd=c(4,2),col=c(2,2))
min(mean_err_test)
which.min(mean_err_test)

# (5) Choisir maintenant le nombre k de voisin en utilisant par 
# validation croisée (cross validation) leave-one-out (LOO) avec la fonction knn.cv. 
?knn.cv  # Default: leave-one-out
err_test <- rep(NA,kmax) 
for (k in 1:kmax) { 
  pred <- knn.cv(X, g, k) 
  err_test[k] <- sum(pred!= g)/length(g) 
} 
lim <-c(0,max(err_test)) 
plot(err_test,type="l",col=2,ylim=lim,xlab="nombre de voisins", ylab="taux d'erreur") 
points(mean_err_test,type="l",col=4,lwd=1) 
legend("bottomright", legend=c("Erreur loo", "Erreur moyenne"), col=c(2,4),lty=1)
min(mean_err_test)
which.min(mean_err_test)
# (6) Faire un petit bilan méthodologique concernant le choix du paramètre k. 

# D. On veut maintenant non seulement choisir k mais également avoir une idée 
# de l’erreur de prédiction de ce classifieur. Pour cela, il faut utiliser 
# des données n’ayant jamais été utilisées. Les données doivent donc être 
# découpées en trois parties : apprentissage/validation/test .

# (1) Couper aléatoirement les données des deux parties : 
# un ensemble "apprentissage-validation" (75 % des données) 
# et un ensemble test de taille (25% des données). 
set.seed(30) 
num_train_val = as.integer(nrow(X) * 0.75)
trvl <- sample(1:nrow(X), num_train_val) 
Xtrainval <- X[trvl,]
ytrainval <- g[trvl]
Xtest <- X[-trvl,] 
ytest <- g[-trvl]
# (2) Utiliser la première approche pour choisir k sur l’ensemble "apprentissage-validation" : 
# i. Choisir k en découpant les 945 données de l’ensemble "apprentissage-validation" en deux 
# parties : une partie "apprentissage" (50% des données) et une partie "validation" 
# (25 % des données). Choisir k qui minimise le taux d’erreur moyen sur les 
#ensembles de validations de B = 25 découpages. 
B <- 25 
kmax <- 50
err_valid <- matrix(NA,kmax,B)
num_train = as.integer(nrow(X) * 0.5)
num_val = num_train_val - num_train
for (b in 1:B) { 
  tr <- sample(1:nrow(Xtrainval), num_train_val - num_val) 
  Xtrain <- Xtrainval[tr,] 
  ytrain = ytrainval[tr]
  Xval <- Xtrainval[-tr,]
  yval = ytrainval[-tr]
  for (k in 1:kmax) 
  { 
    pred <- knn(Xtrain,Xval,ytrain,k) 
    err_valid[k,b] <- sum(pred!=yval)/length(yval) 
  } 
} 
mean_err_valid <- apply(err_valid,1,mean) 
plot(mean_err_valid,type="l")
min(mean_err_valid)
which.min(mean_err_valid)

# ii. Constuire le classifieur avec ce nombre de voisins 
# sur l’ensemble "apprentissage-validation" et calculer le taux d’erreur des données test. 
pred <- knn(Xtrainval,Xtest,ytrainval,k=which.min(mean_err_valid)) 
sum(pred!=ytest)/length(ytest) 

# (3) Utiliser la seconde approche pour choisir k par validation croisée LOO 
# sur l’ensemble "apprentissage validation". Calculer ensuite le taux d’erreur des données test. 
err_valid <- rep(NA,kmax) 
for (k in 1:kmax) 
{ 
  pred <- knn.cv(Xtrainval,ytrainval,k) 
  err_valid[k] <- sum(pred!=ytrainval)/length(ytrainval) 
} 
which.min(err_valid) 
min(err_valid)
pred <- knn(Xtrainval,Xtest,ytrainval,k=which.min(err_valid)) 
sum(pred!=ytest)/length(ytest)


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
  tr <- sample(1:nrow(X),num_train_val) 
  Xtrainval <- X[tr,] 
  ytrainval <- g[tr]
  Xtest <- X[-tr,]
  ytest <- g[-tr]
  for (k in 1:kmax) 
  {
    pred <- knn.cv(Xtrainval,ytrainval,k) 
    err_valid[k] <- sum(pred!=ytrainval)/length(ytrainval) 
  } 
  pred <- knn(Xtrainval,Xtest,ytrainval,k=which.min(err_valid)) 
  err_test[b] <- sum(pred!=ytest)/length(ytest) 
} 
boxplot(err_test,main="Erreurs test pour 50 decoupages")
mean(err_test)

# 2.4 Bayésien naïf 

# 1. Appliquer le classifieur byésien naïf sur la table X. 

# ensemble apprentissage 
## Utiliser le pakcage e1071 
library(e1071) 
m <- naiveBayes(g ~ ., data = X) 
## alternativement: 
table(predict(m, X), g) 
