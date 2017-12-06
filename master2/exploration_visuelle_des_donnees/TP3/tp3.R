# 1) Charger et analyser les packages MASS, klaR, mda
library(MASS)
library(klaR)
library(mda)

# 2) Réaliser une ACP sur le jeu de données Iris en coloriant les individus selon leur classes. 
# Sauvegardez le graphique sous forme d’un fichier de type .pdf
data = iris
X = data[, -5]
y = data[, 5]
N = nrow(X)

X_pca = princomp(X)
plot(X_pca$scores[, 1], 
     X_pca$scores[, 2], 
     col = y, 
     xlab = 'axe principale 1', 
     lab = 'axe principale 2', 
     main = 'Iris')

# 3) Créer à partir de jeu de données Iris, par tirage aléatoire, un échantillon de données utilisé pour
# l’apprentissage (80% des données) et un échantillon de test.
set.seed(10)
num_train = as.integer(nrow(X) * 0.8)
num_test = N - num_train

tr = sample(1:N, num_train)
X_train = iris[tr, ]

X_test = iris[-tr, ]

# 4) Déterminer pour l’échantillon d’apprentissage l’effectif de chaque classe.
summary(y_train)

# 5) Renommer les classes du jeu de données Iris par : ‘a’ pour ‘virginica’, 
# par ‘e’ pour ‘versicolor’ et par ‘s’ pour ‘species’.
renommer = function(a){
  if (a == 'virginica')
    return('a')
  if (a == 'versicolor')
    return('e')
  if (a == 'setosa')
    return('s')
}

X_train$Species = sapply(X_train$Species, renommer)
X_test$Species = sapply(X_test$Species, renommer)

# 6) Réaliser une description graphique des groupe à l’aide de la fonction partimat().
?partimat
partimat(X_train[, 5] ~ ., data = X_train[, -5], method = 'lda')

# 7) Calculer un ADL à l’aide de la fonction fda().
irisfit = fda(y_train~., data = X_train[, -5])
irisfit

# 8) Calculer les observations mal classées.
confusion(predict(irisfit, X_test[, -5]), X_test[, 5])

# 9) Représenter les résultats de l’ADL sous forme d’un graphique. 
# Comparer avec le résultat obtenu à l’aide de l’ACP. 
# Qu’est-ce que vous observez? Réalisez le même scenario sur le jeu de données (Waveform).
plot(irisfit)


# Exercice 2
# 1) Analysez la fonction dist() avec toutes les options proposées.
# Réaliser l’exemple présenté dans la documentation.
x <- matrix(rnorm(100), nrow = 5)
dist(x)
dist(x, diag = TRUE)
dist(x, upper = TRUE)
m <- as.matrix(dist(x))
d <- as.dist(m)
stopifnot(d == dist(x))

## Use correlations between variables "as distance"
dd <- as.dist((1 - cor(USJudgeRatings))/2)
round(1000 * dd) # (prints more nicely)
plot(hclust(dd)) # to see a dendrogram of clustered variables

## example of binary and canberra distances.
x <- c(0, 0, 1, 1, 1, 1)
y <- c(1, 0, 1, 1, 0, 1)
dist(rbind(x, y), method = "binary")
## answer 0.4 = 2/5
dist(rbind(x, y), method = "canberra")
## answer 2 * (6/5)

## To find the names
labels(eurodist)

## Examples involving "Inf" :
## 1)
x[6] <- Inf
(m2 <- rbind(x, y))
dist(m2, method = "binary")   # warning, answer 0.5 = 2/4
## These all give "Inf":
stopifnot(Inf == dist(m2, method =  "euclidean"),
          Inf == dist(m2, method =  "maximum"),
          Inf == dist(m2, method =  "manhattan"))
##  "Inf" is same as very large number:
x1 <- x; x1[6] <- 1e100
stopifnot(dist(cbind(x, y), method = "canberra") ==
            print(dist(cbind(x1, y), method = "canberra")))

## 2)
y[6] <- Inf #-> 6-th pair is excluded
dist(rbind(x, y), method = "binary"  )   # warning; 0.5
dist(rbind(x, y), method = "canberra"  ) # 3
dist(rbind(x, y), method = "maximum")    # 1
dist(rbind(x, y), method = "manhattan")  # 2.4

# 2) Analyser la fonction cmdscale() du package MASS. 
# Appliquer cette fonction sur le jeu de données «eurodist».
library(MASS)
?cmdscale
loc = cmdscale(eurodist)
x <- loc[, 1]
y <- -loc[, 2] # reflect so North is at the top
## note asp = 1, to ensure Euclidean distances are represented correctly
plot(x, y, type = "n", xlab = "", ylab = "", asp = 1, axes = FALSE,
     main = "cmdscale(eurodist)")
text(x, y, rownames(loc), cex = 0.6)

# 3) Installer les package MASS, RDRToolbox, golubEsets. Analyser la documentation de ces packages.
library(golubEsets)
library(RDRToolbox)


golub = read.table('~/master2/master2/exploration_visuelle_des_donnees/TP3/golub_data.csv', row.names = 1, header = T, sep = ',')
golub[1:5,]
golub1 = t(golub)
golub[1:5, 1:5]
golub_label = read.table('~/master2/master2/exploration_visuelle_des_donnees/TP3/golub_class2.csv', sep = ',', row.names = 1)
dim(golub_label)
library(FactoMineR)
pca.golub = PCA(golub1, scale.unit = F)
pca.golubCoord = cbind(pca.golub$ind$coord[, 1], pca.golub$ind$coord[, 2], golub_label)
# plot(pca.golubCoord[, 1], pca.golubCoord[, 2], pch = 21, bg = c('blue', 'yellow'), )

