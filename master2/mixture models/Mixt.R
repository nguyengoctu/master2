X=matrix(nrow=10000,ncol=2)
for (i in 1:10000) {  
  Z=rbinom(1,1,2/3)
  if (Z==1) {
    X[i,1]=rnorm(1,2,1)
    X[i,2]=rnorm(1,1,1)
  }
  else{
    X[i,1]=rnorm(1,7,1)
    X[i,2]=rnorm(1,5,1)
  }
}
plot(X)
summary(X)
plot(density(X))
wait <- normalmixEM(X, lambda = .5, mu = c(50, 60), sigma = 5)
plot(wait, 
     density = TRUE, 
     cex.axis = 1.4, 
     cex.lab = 1.4, 
     cex.main = 1.8, 
     main2 = "Time between Old Faithful eruptions", 
     xlab2 = "Minutes")
library(mclust)
res.mclust = Mclust(X, 2)
summary(res.mclust)

library(mixtools)
attach(faithful)
dim(faithful)
waiting
hist(waiting)
d=density(waiting)
plot(d)
wait1 <- normalmixEM(waiting, lambda = .5, mu = c(50, 60), sigma = 5)
plot(wait1, density = TRUE, cex.axis = 1.4, cex.lab = 1.4, cex.main = 1.8,main2 = "Time between Old Faithful eruptions", xlab2 = "Minutes")

library(FactoMineR)
library(Rmixmod)
data(birds)
dim(birds)
birds
xem.birds <- mixmodCluster(birds, 2)
summary(xem.birds)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(mclust)
data(diabetes)
class <- diabetes$class
table(class)
## class
## Chemical   Normal    Overt 
##       36       76       33
X <- diabetes[,-1]
head(X)
res.pca=PCA(X)
clPairs(X, class)
res.mclust <- Mclust(X,3)
summary(res.mclust)
table(res.mclust$class,diabetes$class)
res.kmeans=kmeans(X,3,nstart=100)
table(res.kmeans$cluster,diabetes$class)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

