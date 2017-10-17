library(class)
library(MASS)
library(kohonen)

X = scale(iris[, -5], center = TRUE, scale = TRUE)
data = read.table('workspace/master2/apprentissage_non_supervise/Velib.txt', header = T)
X = as.matrix(data[, -(1:3)])

SOM.IRIS = som(X, grid = somgrid(20, 20, topo = 'rectangular'), rlen = 200)
SOM.IRIS$codes      # Coordonnees des centres
SOM.IRIS$distances  # Distances entres neuronnes adjacents
SOM.IRIS$changes    # Erreur entre les donnees et les centres
plot(SOM.IRIS$changes, type = 'l')

couleur1 = tricolor(SOM.IRIS$grid)
plot(SOM.IRIS, type = 'mapping', bgcol = rgb(couleur1))
plot(SOM.IRIS, type = 'mapping', labels = iris[, 5], pchs = 2)
plot(SOM.IRIS, type = 'quality')
plot(SOM.IRIS, type = 'counts')
plot(SOM.IRIS, type = 'codes')

# Modification de la palette de couleurs (bleu -> rouge)
coolBlueHotRed = function(n, alpha = 1){
  rainbow(n, end = 4/6, alpha = alpha)
}

# U-matrix (matrix de voisinage)
plot(SOM.IRIS, type = 'dist.neighbours', palette.name = coolBlueHotRed)

# Component planes
par(mfrow = c(2, 2))
for (i in 1:4){
  plot(SOM.IRIS, type = 'property', palette.name = coolBlueHotRed, property = SOM.IRIS$codes[[1]][, i], main = colnames(SOM.IRIS$codes[[1]])[i])
}

