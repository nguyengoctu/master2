#=====================================================================
#Exemple d'ACP avec l'essentiel des sorties de PCA
#On s'int?resse aux profils de temp?ratures des capitales europ?ennes
#====================================================================

library(FactoMineR)
temperature <- read.table("http://factominer.free.fr/livre/temperat.csv",header=TRUE, sep=";", dec=".", row.names=1, fileEncoding="latin1")

#Raliser une ACP sur les capitales eurp?ennes en choisissant judicieusement les varaibles actives
res <- PCA(temperature, ind.sup=24:35, quanti.sup=13:16, quali.sup=17,scale=TRUE)
res <- PCA(temperature, quanti.sup=13:16, quali.sup=17,scale=TRUE)


#Pr?senter le premier plan factoriel en habillant les individus avec en fonction de la variables R?gion
plot.PCA(res, choix="ind", habillage=17)

#m?thode du coude consulter ce lien pour certaines options https://www.statmethods.net/graphs/bar.html
val.propres <- res$eig[,1]
plot(1:12,val.propres,type="b",ylab="Valeurs propres",xlab="Composantes",main="M?thode du coude")
#ou
barplot(val.propres,ylab="Valeurs propres",xlab="Composantes",main="M?thode du coude",names.arg=1:nrow(res$eig))
#ou ? l'aide du package factoextra qui requiert celui de munsell
library(factoextra)
fviz_eig(res, addlabels = TRUE, ylim = c(0, 90)) 

#Faire la selection des variables les plus corr?l?es avec les composantes principales
dimdesc(res)
select  <- dimdesc(res, axes=1:2, proba=0.05)

#Afficher les valeurs propres
res$eig

#Afficher la description des individus: coordonn?es, contribution, qualit? de repr?sentation et distance par rapport ? l'origine
res$ind

#Afficher la description des individus suppl?mentaires : coordonn?es, qualit? de repr?sentation et distance par rapport ? l'origine
res$ind.sup

#Afficher la description des variables actives : coordonn?es (corr?lations), qualit? de repr?sentation (cos2) et contribution (contrib)
res$var

#idem pour les variables quantitatives suppl?mentaires (pas de contribution)
res$quanti.sup

#idem pour les variables qualitatives suppl?mentaires (pas de contribution)
res$quali.sup

#Selection des variables
library("corrplot")
corrplot(res$var$cos2, is.corr=FALSE)

#Selection des individus selon contribution ou cos2
fviz_pca_ind(res, col.ind = "contrib",
             gradient.cols = c("yellow", "green", "red"),
             repel = TRUE)
fviz_pca_ind(res, col.ind = "cos2",
             gradient.cols = c("yellow", "green", "red"),
             repel = TRUE)

#Centrer et r?duire, un mot sur le deuxi?me terme (Que dire de Helsinki et Ath?nes)
scale(temperature[1:23,1:16])*sqrt(22/23)
#Matrice de corr?lation
cor(temperature[1:23,1:16])

#Ellipses de confiance
concat.data <- cbind.data.frame(temperature[1:35,17],res$ind$coord)
ellipse.coord <- coord.ellipse(concat.data,bary=TRUE)
plot.PCA(res, habillage=17, ellipse=ellipse.coord, cex.names=0.8)

#===========================================================
#Reduced K-means with 3 clusters in 2 dimensions after 10 random starts
library(clustrd)
help(clustrd)
macro=temperature[1:23,1:16]
data(macro)
dim(macro)
outRKM = cluspca(macro, 3, 2, method = "RKM", rotation = "varimax", scale = FALSE, nstart = 10)
summary(outRKM)
#Scatterplot (dimensions 1 and 2) and cluster description plot
plot(outRKM, cludesc = TRUE)

#Factorial K-means with 3 clusters in 2 dimensions 
#with a Reduced K-means starting solution
data(macro)
outFKM = cluspca(macro, 3, 2, method = "FKM", rotation = "varimax", 
                 scale = FALSE, smartStart = outRKM$cluster)
outFKM
#Scatterplot (dimensions 1 and 2) and cluster description plot
plot(outFKM, cludesc = TRUE)

#To get the Tandem approach (PCA(SVD) + K-means)
outTandem = cluspca(macro, 3, 2, alpha = 1)
plot(outTandem)
