# Lecture des donnees

# gordon
data1 = read.table('/media/ngoctu/769829E69829A599/workspace/master2/master2/exploration_visuelle_des_donnees/Projet_Visualisation/gordon-2002_database.txt', 
                  sep = '\t')
setwd('/media/ngoctu/769829E69829A599/workspace/master2/master2/exploration_visuelle_des_donnees/Projet_Visualisation/gordon/')
X = t(data1[-(1:2), ])[-1, ]
shape = dim(X)
y = t(data1[2, ])
y = y[-1]
y = as.factor(y)

X = mapply(X, FUN = as.numeric)
X = matrix(data = X, nrow = shape[1], ncol = shape[2])
summary(X)
summary(y)


# Normalization
X = scale(X, center = T, scale = T)
gordon = data.frame(X, y)
set.seed(2018)
tr = sample(1:nrow(X), as.integer(0.8 * nrow(X)))
train = gordon[tr, ]
test = gordon[-tr, ]

# Methodes
# 1. ACP
library(FactoMineR)
res.pca = PCA(gordon, scale.unit = T, ncp = 2, quali.sup = 1627, graph = F)
png(filename = '1. ACP1.png', width = 800, height = 600, units = 'px')
plot.PCA(res.pca, choix = 'ind')
dev.off()
png(filename = '1. ACP2.png', width = 800, height = 600, units = 'px')
plot.PCA(res.pca, choix = 'ind', habillage = 1627)
dev.off()

# 2. ADL
library(MASS)
# res.adl = fda(y ~ ., data = gordon[, -1627], dimension = 2)
res.adl = lda(y ~., data = train)
predict.adl = predict(res.adl, test[, -1627])
table(predict.adl$class, test[, 1627])

projected_data = as.matrix(gordon[, -1627]) %*% res.adl$scaling
png(filename = '2. ADL.png', width = 800, height = 600, units = 'px')
plot(projected_data, col = gordon[, 1627], pch = 19)
dev.off()

# 3. MDS
D = dist(gordon[, -1627])
res.mds = cmdscale(D)
png(filename = '3. MDS1.png', width = 800, height = 600, units = 'px')
plot(res.mds, pch = 19)
dev.off()
png(filename = '3. MDS2.png', width = 800, height = 600, units = 'px')
plot(res.mds, col = gordon[, 1627], pch = 19)
dev.off()

# 4. Isomap
library(RDRToolbox)
png(filename = '4. Isomap1.png', width = 800, height = 600, units = 'px')
par(mfrow=c(3, 3))
K = c(2, 3, 5, 10, 15, 20, 30, 50, 100)
for (k in K){
  res.isomap = Isomap(as.matrix(gordon[, -1627]), k = k)
  plot(res.isomap$dim2, pch = 19, main = paste('K =',k), xlab = 'MDS dim1', ylab = 'MDS dim2')
}
dev.off()

png(filename = '4. Isomap2.png', width = 800, height = 600, units = 'px')
par(mfrow=c(3, 3))
for (k in K){
  res.isomap = Isomap(as.matrix(gordon[, -1627]), k = k)
  plot(res.isomap$dim2, col = gordon[, 1627], pch = 19, main = paste('K =',k), xlab = 'MDS dim1', ylab = 'MDS dim2')
}
dev.off()

# 5. LLE
png(filename = '5. LLE1.png', width = 800, height = 600, units = 'px')
par(mfrow=c(3, 3))
for (k in K){
  res.lle = LLE(as.matrix(gordon[, -1627]), dim = 2, k = k)
  plot(res.lle, pch = 19, main = paste('K =', k), xlab = 'MDS dim1', ylab = 'MDS dim2')
}
dev.off()

png(filename = '5. LLE2.png', width = 800, height = 600, units = 'px')
par(mfrow=c(3, 3))
for (k in K){
  res.lle = LLE(as.matrix(gordon[, -1627]), dim = 2, k = k)
  plot(res.lle, col = gordon[, 1627], pch = 19, main = paste('K =', k), xlab = 'MDS dim1', ylab = 'MDS dim2')
}
dev.off()

# 6. SOM
library(kohonen)
res.som = som(as.matrix(gordon[, -1627]), grid = somgrid(10, 10, topo = 'rectangular'))
png(filename = '6. SOM.png', width = 800, height = 600, units = 'px')
par(mfrow = c(1, 2))
plot(res.som, type = 'mapping', col = gordon[, 1627], pch = 19)

# U-matrix or Neighbour distance
plot(res.som, type = 'dist.neighbours')
dev.off()



# pomeroy
setwd('/media/ngoctu/769829E69829A599/workspace/master2/master2/exploration_visuelle_des_donnees/Projet_Visualisation/pomeroy/')
data2 = read.table('/media/ngoctu/769829E69829A599/workspace/master2/master2/exploration_visuelle_des_donnees/Projet_Visualisation/pomeroy-2002-v2_database.txt',
                   sep = '\t')
X = t(data2[-(1:2), ])[-1, ]
shape = dim(X)
y = t(data2[2, ])
y = y[-1]
y = as.factor(y)

X = mapply(X, FUN = as.numeric)
X = matrix(data = X, nrow = shape[1], ncol = shape[2])
summary(X)
summary(y)

# Normalization
X = scale(X, center = T, scale = T)
pomeroy = data.frame(X, y)
set.seed(2018)
tr = sample(1:nrow(X), as.integer(0.8 * nrow(X)))
train = pomeroy[tr, ]
test = pomeroy[-tr, ]

# Methodes
# 1. ACP
library(FactoMineR)
res.pca = PCA(pomeroy, scale.unit = T, ncp = 2, quali.sup = 1380, graph = F)
png(filename = '1. ACP1.png', width = 800, height = 600, units = 'px')
plot.PCA(res.pca, choix = 'ind')
dev.off()
png(filename = '1. ACP2.png', width = 800, height = 600, units = 'px')
plot.PCA(res.pca, choix = 'ind', habillage = 1380)
dev.off()

# 2. ADL
library(MASS)
# res.adl = fda(y ~ ., data = gordon[, -1627], dimension = 2)
res.adl = lda(y ~., data = train)
predict.adl = predict(res.adl, test[, -1380])
table(predict.adl$class, test[, 1380])

# 2D plot
png(filename = '2. ADL1.png', width = 800, height = 600, units = 'px')
projected_data_2D = as.matrix(pomeroy[, -1380]) %*% res.adl$scaling[, 1:2]
plot(projected_data_2D, col = pomeroy[, 1380], pch = 19)
dev.off()

# 3D plot
library(plot3D)
projected_data_3D = as.matrix(pomeroy[, -1380]) %*% res.adl$scaling[, 1:3]
png(filename = '2. ADL2.png', width = 800, height = 600, units = 'px')
scatter3D(x = projected_data_3D[, 1], y = projected_data_3D[, 2], z = projected_data_3D[, 3], col = pomeroy[, 1380], xlab = 'LDA1', ylab = 'LDA2', zlab = 'LDA3')
dev.off()

# 3. MDS
D = dist(pomeroy[, -1380])
res.mds = cmdscale(D)
png(filename = '3. MDS1.png', width = 800, height = 600, units = 'px')
plot(res.mds, pch = 19)
dev.off()

png(filename = '3. MDS2.png', width = 800, height = 600, units = 'px')
plot(res.mds, col = pomeroy[, 1380], pch = 19)
dev.off()

# 4. Isomap
library(RDRToolbox)
png(filename = '4. Isomap1.png', width = 800, height = 600, units = 'px')
par(mfrow=c(2, 3))
K = c(2, 3, 5, 10, 15, 20)
for (k in K){
  res.isomap = Isomap(as.matrix(pomeroy[, -1380]), k = k)
  plot(res.isomap$dim2, pch = 19, main = paste('K =',k), xlab = 'MDS dim1', ylab = 'MDS dim2')
}
dev.off()

png(filename = '4. Isomap2.png', width = 800, height = 600, units = 'px')
par(mfrow=c(2, 3))
for (k in K){
  res.isomap = Isomap(as.matrix(pomeroy[, -1380]), k = k)
  plot(res.isomap$dim2, col = pomeroy[, 1380], pch = 19, main = paste('K =',k), xlab = 'MDS dim1', ylab = 'MDS dim2')
}
dev.off()

# 5. LLE
png(filename = '5. LLE1.png', width = 800, height = 600, units = 'px')
par(mfrow=c(2, 3))
for (k in K){
  res.lle = LLE(as.matrix(pomeroy[, -1380]), dim = 2, k = k)
  plot(res.lle, pch = 19, main = paste('K =', k), xlab = 'MDS dim1', ylab = 'MDS dim2')
}
dev.off()

png(filename = '5. LLE2.png', width = 800, height = 600, units = 'px')
par(mfrow=c(2, 3))
for (k in K){
  res.lle = LLE(as.matrix(pomeroy[, -1380]), dim = 2, k = k)
  plot(res.lle, col = pomeroy[, 1380], pch = 19, main = paste('K =', k), xlab = 'MDS dim1', ylab = 'MDS dim2')
}
dev.off()

# 6. SOM
library(kohonen)
res.som = som(as.matrix(pomeroy[, -1380]), grid = somgrid(6, 6, topo = 'rectangular'))
png(filename = '6. SOM.png', width = 800, height = 600, units = 'px')
par(mfrow = c(1, 2))
plot(res.som, type = 'mapping', col = pomeroy[, 1380], pch = 19)

# U-matrix or Neighbour distance
plot(res.som, type = 'dist.neighbours')
dev.off()
