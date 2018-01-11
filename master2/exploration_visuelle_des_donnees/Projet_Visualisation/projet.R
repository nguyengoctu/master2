# Lecture des donnees

# gordon
data = read.table('/media/ngoctu/769829E69829A599/workspace/master2/master2/exploration_visuelle_des_donnees/Projet_Visualisation/gordon-2002_database.txt', 
                  sep = '\t')

X = t(data[-(1:2), ])[-1, ]
shape = dim(X)
y = t(data[2, ])
y = y[-1]
y = as.factor(y)

X = mapply(X, FUN = as.numeric)
X = matrix(data = X, nrow = shape[1], ncol = shape[2])
summary(X)
summary(y)

X = scale(X, center = T, scale = T)

gordon = data.frame(X, y)
set.seed(2018)
tr = sample(1:nrow(X), as.integer(0.8 * nrow(X)))
train = gordon[tr, ]
test = gordon[-tr, ]

# Methodes
# 1. ACP
library(FactoMineR)
res.pca = PCA(gordon, scale.unit = T, ncp = 2, quali.sup = 1627)
plot.PCA(res.pca, choix = 'ind', habillage = 1627)

# 2. ADL
library(MASS)
# res.adl = fda(y ~ ., data = gordon[, -1627], dimension = 2)
res.adl = lda(y ~., data = train)
predict.adl = predict(res.adl, test[, -1627])
table(predict.adl$class, test[, 1627])

projected_data = as.matrix(gordon[, -1627]) %*% res.adl$scaling
plot(projected_data, col = gordon[, 1627], pch = 19)

# 3. MDS
D = dist(gordon[, -1627])
res.mds = cmdscale(D)
plot(res.mds, col = gordon[, 1627], pch = 19)

# 4. Isomap
library(RDRToolbox)
par(mfrow=c(3, 3))
K = c(2, 3, 5, 10, 15, 20, 30, 50, 100)
for (k in K){
  res.isomap = Isomap(as.matrix(gordon[, -1627]), k = k)
  plot(res.isomap$dim2, col = gordon[, 1627], pch = 19, main = paste('K =',k), xlab = 'MDS dim1', ylab = 'MDS dim2')
}

# 5. LLE
par(mfrow=c(3, 3))
for (k in K){
  res.lle = LLE(as.matrix(gordon[, -1627]), dim = 2, k = k)
  plot(res.lle, col = gordon[, 1627], pch = 19, main = paste('K =', k), xlab = 'MDS dim1', ylab = 'MDS dim2')
}


# 6. SOM
library(kohonen)
res.som = som(as.matrix(gordon[, -1627]), grid = somgrid(10, 10, topo = 'hexagonal'))
res.som$codes
res.som$distances
res.som$changes
color1 = tricolor(res.som$grid)
plot(res.som, type = 'mapping', bgcol = rgb(color1))
plot(res.som, type = 'mapping', col = gordon[, 1627], pch = 19)
plot(res.som, type = 'quality')

# Node counts
plot(res.som, type = 'counts')
plot(res.som, type = 'codes')

coolBlueHotRed = function(n, alpha = 1)
{
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}

# U-matrix or Neighbour distance
plot(res.som, type = 'dist.neighbours')

mydata <- res.som$codes[[1]]
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var)) 
for (i in 2:15) {
  wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
}
plot(wss)
## use hierarchical clustering to cluster the codebook vectors
som_cluster <- cutree(hclust(dist(res.som$codes[[1]])), 2)
# plot these results:
plot(res.som, type="mapping", bgcol = som_cluster, main = "Clusters") 
add.cluster.boundaries(som_model, som_cluster)