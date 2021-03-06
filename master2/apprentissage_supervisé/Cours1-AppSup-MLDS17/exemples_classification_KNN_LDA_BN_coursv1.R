##
##########################################################################
# Exercice 1
###########################################################################
# exemples de classification - donn�es iris
###########################################################################



###########################################################################
# ACP sur la table iris
###########################################################################

# Donn�es

data(iris)
X=iris[,1:4]
labels=iris[,5]
#R�sum� statistique
summary(X)
table(labels)

#exploration graphique des donn�es

#Boite � mostaches
boxplot(X)

#Histogramme 

layout(matrix(c(1:4),2,2))
for(i in 1:4) {hist(X[,i],main=names(iris)[i],xlab="")}
layout(1)



#Nuage de points

pairs(X,main="Donn�es IRIS")

#Nuage de points en foction des labels

pairs(iris[,1:4],col = as.numeric(iris[,5]))

##ACP

p <- prcomp(iris[,1:4])
groupe = gl(3,50)
pairs(p$x,col = as.numeric(labels))
cor(iris[,1:4],p$x)


###########################################################################
# LDA sur la table swiss
###########################################################################



##
 z <- lda(as.matrix(iris[,1:4]),labels)
 zp <- predict(z,iris[,1:4])
 plot(zp$x,col = as.numeric(labels))
table(iris[,5],zp$class)

## Matrice de confusion

zc <- lda(iris[,1:4],labels,CV=TRUE)
table(iris[,5],zc$class)


###########################################################################
# KNN sur la table iris
###########################################################################

library(class)
iris.d<-iris[, 1:4] # ensemble apprentissage
iris.cl <- iris[,5] # labels de classes
iris.1nn <- knn.cv(iris.d, iris.cl, k=20) # 1-NN avec validation crois�e (CV)
table(iris.1nn, iris.cl) #matrice de confusion

%txerreu=sum()/length(iris.cl)
txerreur=sum(iris.1nn!=iris.cl)/length(iris.cl)

###############################
Le Classifieur Bayesien naif sur la table iris
###############################



# Echantillon d'apprentissage & de test
library(e1071)
set.seed(1)
n = dim(iris)[1]
index = sample(n, 0.7 * n)
Appren = iris[index, ]
Test = iris[-index, ]
# Mod�lisation
nb.model <- naiveBayes(Species~., data = Appren)

#l'impl�mentation du mod�le est simple en utilisant la fonction. 
#Les r�sultats en sortie sont les suivants 


##La qualit� du mod�le d�pend de sa capacit� � bien classer 
##dans le jeu de donn�es test

SpeciesTH <- predict(object = nb.model, newdata = Test)
Test.mod <- cbind(Test, SpeciesTH)
head(Test.mod, 5)

tail(Test.mod, 5)

# Taux de bien class�
(Confusion = table(Test.mod$Species, Test.mod$SpeciesTH))

# En pourcetages:
round(prop.table(Confusion), 2)




