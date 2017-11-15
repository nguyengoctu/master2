# changer le répertoire de travail

setwd('D:/MLDS17/Cours3_app_afaire_LDA')

##  Charger les données

load("insectes.rda")
head(insectes)
X <- insectes[,1:6]
y <- insectes[,7]
class(y)

View(X)
str(insectes)
dim(X)
 #X=scale(X, center = TRUE)
 #XX=data.frame(X,y)
#------- Appliquer LDA -------------------
require(MASS)
?lda

## res.lda <- lda(y~.,XX)

res.lda <- lda(type~.,insectes)

names(res.lda)

res.lda

res.lda$scaling #axes discriminants


#----Question 1--------------
levels(y) #3 groupes donc au plus deux axes discriminants

#----Question 2--------------
n <-nrow(X) #nombre total d'insectes
table(y) #nombre d'insectes dans chaque groupes


#----Question 3----------------
colMeans(res.lda$means) #centre de gravite de l'ensemble des donnees
res.lda$means

#----Question 4----------------
var(as.matrix(X)) ## Matrice variance-covariance
cor(as.matrix(X)) ## Matrice de corrélation 
boxplot(X)

pairs(as.matrix(X), col=as.numeric(y)) ## Nuage de points


# Corrélation variables facteur discriminants
F=as.matrix(X) %*% res.lda$scaling
cc=cor(as.matrix(X),F)


## faire une ACP

#----Question 5----------------
require(FactoMineR)

pca<-PCA(insectes,quali.sup=7,graph=FALSE) #ACP sur matrice des correlations
plot(pca,habillage=7)
plot(pca,choix="var")
#plot(pca,invisible="ind")


#----Question 6----------------

names(res.lda)

res.lda$svd # Valeurs singulières 

res.lda$svd ^2
sum(res.lda$svd ^2)
res.lda$svd ^2/sum(res.lda$svd ^2)

0.8206673/(1- 0.8206673)

#----Question 7 et 8----------------


plot(F, col=as.numeric(y))


a <- seq(0,2*pi,length=100)
plot(cos(a), sin(a), type='l', lty=3, xlab='LDA1', ylab='LD2')
title(main="Cercle des corrélations")
# On trace à présent les flèches
v <- t(cc)[1:2,]
arrows(0,0, v[1,], v[2,], col='red')
text(v[1,], v[2,],colnames(v))
##


#----Question 9----------------
#centre de gravite des donnees centrees

gg=colMeans(res.lda$means)

gA=res.lda$means[1,]-gg
gB=res.lda$means[2,]-gg
gC=res.lda$means[3,]-gg



res.lda$scaling
v1<-res.lda$scaling[,1] #1er axe discriminant
t(gA)%*%v1
t(gB)%*%v1
t(gC)%*%v1


S1 <- F[,1] #1ere variable discriminante
Sk <- split(S1,y)
lapply(Sk,mean)



plot(F[,1],rep(0,74), type = "p",col=as.numeric(y), main = "1ere variable dicriminante ")


#----Question 10----------------

#
obs <- c(193,131,55,160,16,102)
gg=colMeans(res.lda$means)
obs2 <- obs-gg
obs2%*%v1



#-------Question 11 -------------------
S1 <- F[,1] #1ere variable discriminante
Sk <- split(S1,y)
smoy <- lapply(Sk,mean)
seuil1 <- (smoy$C+smoy$A)/2 #seuil entre C et A
seuil2 <- (smoy$A+smoy$B)/2 #seuil entre A et B

predict <- cut(S1,breaks=c(-15,seuil2,seuil1,1),labels=c("B","A","C")) # bien définir les limits -15e t 1
table(y,predict)
sum(y!=predict)/nrow(X)




#-------Question 12 -------------------
n<-nrow(X)
index <- sample.int(n,50)
Xapp <- X[index,]
yapp <- y[index]
Xtest <- X[-index,]
ytest <- y[-index]


App=data.frame(Xapp,yapp)


#construction de la règle sur l'ensemble d'apprentissage
res.lda <- lda(yapp~.,App)

Fapp=as.matrix(Xapp) %*% res.lda$scaling
S1 <- Fapp[,1] #1ere variable discriminante
Sk <- split(S1,yapp)
smoy <- lapply(Sk,mean)
seuil1 <- (smoy$C+smoy$A)/2 #seuil entre C et A
seuil2 <- (smoy$A+smoy$B)/2 #seuil entre A et B

#application de la règle sur l'ensemble test
ggapp=colMeans(res.lda$means)

Xtest_centre <- sweep(Xtest,2,STATS=ggapp,FUN="-") #centrage des données

Ftest <- as.matrix(Xtest_centre)%*% res.lda$scaling

predict <- cut(Ftest[,1],breaks=c(-6,seuil1,seuil2,8),labels=c("C","A","B")) #c("C","A","B")
table(ytest,predict)
sum(ytest!=predict)/n

#Appliquer LDA

res.lda <- lda(type~.,insectes)

names(res.lda)

res.lda

res.lda$scaling #facteurs discriminants


pred <- predict(res.lda) #idem fonction predict.lda
?predict.lda
pred$x[1:5,] #variable discriminante (score de l'AFD)
#res.lda$S[1:5,] #identique celui de l'approche GB

# predict classes
lda.ghat <- predict(res.lda)$class

# number of errors
sum(lda.ghat != y)

# error rate
mean(lda.ghat != y)

# confusion table
table(lda.ghat, y)

#Appliquer QLDA


####
# qda sur la table insectes
res.qda <- lda(type~.,insectes)

names(res.qda)

# predict classes
qda.ghat <- predict(res.qda)$class

# number of errors
sum(qda.ghat != y)

# error rate
mean(qda.ghat != y)

# confusion table
table(qda.ghat, y)


####

############################
#Données synthétique LDA vs QDA
###########################

# charger les données
train <- read.table(file="synth_train.txt", header=TRUE)
X <- train[,-1]
Y <- train$y


plot(X, pch=Y, col=Y)
legend("topleft", legend=c("classe1", "classe2"), pch=1:2, col=1:2)

#

res.lda <- lda(y~.,train)
data=train;
attach(data)

a <- seq(from=min(data$x1), to=max(data$x1), length.out=100)
b <- seq(from=min(data$x2), to=max(data$x2), length.out=100)

grille <- NULL
for (i in a){
  grille <- rbind(grille, cbind(i,b))
}
pred_grille <- predict(res.lda)$class

# x11(bg="white")
plot(X, pch=data$y, col=data$y)
points(grille, pch=20, col=pred_grille, cex=0.5)
legend("topright", legend=c("classe 1", "classe 2"), pch=1:2, col=1:2, bg="white")


##

res.qda <- qda(y~.,train)

a <- seq(from=min(data$x1), to=max(data$x1), length.out=100)
b <- seq(from=min(data$x2), to=max(data$x2), length.out=100)

grille <- NULL
for (i in a){
  grille <- rbind(grille, cbind(i,b))
}
pred_grille <- predict(res.qda)$class

# x11(bg="white")
plot(X, pch=data$y, col=data$y)
points(grille, pch=20, col=pred_grille, cex=0.5)
legend("topright", legend=c("classe 1", "classe 2"), pch=1:2, col=1:2, bg="white")

############################
#Données Prostate
###########################

rm(list=ls())
# we'll use the prostate data set from "the book"
library(ElemStatLearn)
data(prostate)

# get information on dataset
?prostate

# attach dataset to refer to names
attach(prostate)

# let's dicotomize the response lpsa for this example
# we'll call it high if greater than median,
# and low otherwise
g <- factor(ifelse(lpsa > median(lpsa), "high", "low"))

library(MASS)
# fit lda
lda.fit <- lda(g~lcavol+lweight+age)

# predict classes
lda.ghat <- predict(lda.fit)$class

# number of errors
sum(lda.ghat != g)

# error rate
mean(lda.ghat != g)

# confusion table
table(lda.ghat, g)


####
# qda
qda.fit <- qda(g~lcavol+lweight+age)

# predict classes
qda.ghat <- predict(qda.fit)$class

# number of errors
sum(qda.ghat != g)

# error rate
mean(qda.ghat != g)

# confusion table
table(qda.ghat, g)



