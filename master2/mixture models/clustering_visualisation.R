# 1. et 2.
library(gdata)
data = read.xls('~/master2/master2/mixture models/Chang_15.xlsx', 1)
label = read.xls('~/master2/master2/mixture models/Chang_True.xlsx', 1)
summary(data)
pairs(data[, 1:15])
plot(data[, 1], data[, 3])
plot(data[, 2], data[, 3])
dim(data)

# 3. Visualiser l’ensembe des observations (individus) sur votre premier plan factoriel 
# en utilsant une analyse en composantes principales, que peut-on dire ?
library(FactoMineR)
res = PCA(data, ncp = 15)
plot.PCA(res, axes = c(1, 2))

# 4. Visualiser les plans 1 × 3, 2 × 3, que peut-on dire ?


# 5. On chercher à partitionner l’ensemble des observations
library(NbClust)
res.NbClust.kmeans = NbClust(data, method = 'kmeans', min.nc = 2, max.nc = 8, index = 'all')
res.NbClust.kmeans$Best.partition

# CAH - single-link
res.NbClust.single = NbClust(data, distance = 'euclidean', method = 'single', min.nc = 2, max.nc = 8, index = 'all')


# 6. Quel nombre de classes peut-on proposer? ---- 2 classes

# 7. On décide d’utiliser les algorithmes issus de l’approche mélange
# mclust
library(mclust)
res.mclust = Mclust(data, 2)
summary(res.mclust)

# Rmixmod
library(Rmixmod)
res.Rmixmod = mixmodCluster(data, 2)
class.mixmod = slot(slot(res.Rmixmod, "bestResult"), "partition") 
summary(res.Rmixmod)


# 8. On décide de visualiser les classes de l’ensemble des observations 
# avec la fonction MclustDR du package mclust.
dr = MclustDR(res.mclust)
plot(dr, what = 'scatterplot')
plot(dr, what = 'evalues')


plot(data[, 1], data[, 15], col = res.mclust$classification)
plot(data[, 1], data[, 15], col = label$True + 1)
plot(data[, 1], data[, 15], col = class.mixmod)

table(label$True, res.mclust$classification)
table(label$True, class.mixmod)

adjustedRandIndex(label$True, res.mclust$classification)
adjustedRandIndex(label$True, class.mixmod)


library(R.matlab)
jaffe = readMat('~/master2/master2/mixture models/jaffe_213n_676d_10c.mat')
summary(jaffe$y)
dim(jaffe$X)
jaffe_data = scale(jaffe$X, scale = F)
jaffe_label = as.vector(jaffe$y)
jaffe.NbClust.kmeans = NbClust(jaffe_data, method = 'kmeans', min.nc = 2, max.nc = 10, index = 'all')
