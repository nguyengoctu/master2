library(MASS)
install.packages('RColorBrewer')
library(RColorBrewer)
library(class)

mycols = brewer.pal(8, 'Dark2')[c(3, 2)]

# Générer des données selon une loi normale multivarié
s = sqrt(1/5)
set.seed(30)

makeX = function(M, n = 100, sigma = diag(2) * s){
  z = sample(1:nrow(M), n, replace = TRUE)
  m = M[z, ]
  
  # t(x): x transpose
  return(t(apply(m, 1, function(mu) mvrnorm(1, mu, sigma)))) 
}

M0 = mvrnorm(10, c(1, 0), diag(2)) # Gerer 10 moyennes
x0 = makeX(M0)

M1 = mvrnorm(10, c(0, 1), diag(2))
x1 = makeX(M1)

x = rbind(x0, x1)
y = c(rep(0, 100), rep(1, 100))
cols = mycols[y + 1]



# afficher les donnees d´apprentissage x
plot(x[, 1], x[, 2], col = y)
plot(x, pch = y, col = y)
qplot(x[, 1], x[, 2], col = Y)

X=x ;Y=y+1
plot(X, pch=Y, col=Y)
legend("topright", legend=c("classe 1", "classe 2"), pch=1:2, col=1:2)

# tracer la frontière de décision dans une grille
# mettre les données dans une grille de taille Gs x Gs
GS <- 75
XLIM <- range(x[,1])
tmpx <- seq(XLIM[1], XLIM[2], len=GS)

YLIM <- range(x[,2])
tmpy <- seq(YLIM[1], YLIM[2], len=GS)

# Créer une grille de points
newx <- expand.grid(tmpx, tmpy)


# Appliquer la fonction  lm pour  modéliser y en fonction de X1 et X2

X1 <- x[,1]

X2 <- x[,2]
linear.fit <- lm(y~x)

m <- -linear.fit$coef[2]/linear.fit$coef[3]

b <- (0.5 - linear.fit$coef[1])/linear.fit$coef[3]

# Prédire de nouvelles données  sur la grille

yhat <- predict(linear.fit, newdata=data.frame(X1=newx[,1],X2=newx[,2]))
yhat <- as.numeric(yhat>0.5)
colshat <- mycols[as.numeric(yhat>0.5)+1]

# Afficher la frontière de décision

# tracer la frontière de décision 
plot(x,col=cols,xlab="X1",ylab="X2",xlim=XLIM,ylim=YLIM,type="n") 
abline(b,m)
points(newx,col=colshat,pch=".")

points(x,col=cols) 
title("Régression linéair")


# 1. Appliquer la fonction knn du package class avec k = 15 
# voisins pour prédire les points de coordonnées (0,0) et (-2,2).
library(class)
Xtest<- matrix(c(0,0,-2,2), nrow=2, byrow=TRUE) 
pred <- knn(X,Xtest, Y,15)
pred

# avec k=15 voisins 
pred_train <- knn(X, X,Y,15) 
pred_train != Y

sum(pred_train!=Y)/length(Y)

# avec k=1 voisin 
pred_train <- knn(X, X,Y,1)
sum(pred_train!=Y)/length(Y)


# KNN (1), avec 1 voisin

yhat <- knn(x, newx, y, k=1) 
colshat <- mycols[as.numeric(yhat)]

plot(x, xlab="X1", ylab="X2", xlim=XLIM, ylim=YLIM, type="n") 
points(newx, col=colshat, pch=".")

contour(tmpx, tmpy, matrix(as.numeric(yhat),GS,GS), levels=c(1,2), add=TRUE, drawlabels=FALSE)

points(x, col=cols) 
title("KNN (1)")

# KNN (15)
yhat <- knn(x, newx, y, k=15) 
colshat <- mycols[as.numeric(yhat)]

plot(x, xlab="X1", ylab="X2", xlim=XLIM, ylim=YLIM, type="n") 
points(newx, col=colshat, pch=".")

contour(tmpx, tmpy, matrix(as.numeric(yhat),GS,GS), levels=c(1,2), add=TRUE, drawlabels=FALSE)

points(x, col=cols) 
title("KNN (15)")


# 4. A partir de la matrice de données x. 
# Construire un ensemble d’apprentissage Xtrain (75% des observations) et un ensemble de test Xtest (25% d’observations). 
# Prédire les données de l’ensemble test avec k = 15 voisins puis avec k = 1 voisin. 
# Calculer le taux d’erreur empirique dans les deux cas. 
# Comparer avec les taux d’erreur des prédictions de l’ensemble d’apprentissage.
set.seed(30)

tr <- sample(1:nrow(X),100) 
Xtrain <- X[tr,]
Xtest <- X[-tr,]
Ytrain = y[tr]
Ytest <- y[-tr]
# 15 voisins

pred_test <- knn(Xtrain,Xtest,Ytrain,15) 
sum(pred_test!=Ytest)/length(Ytest)
# 1 voisin

pred_test <- knn(Xtrain,Xtest,Ytrain,1) 
sum(pred_test!=Ytest)/length(Ytest)


