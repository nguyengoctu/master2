# 2. Importer ces tables en utilisant la librairie R.matlab.
library(R.matlab)
setwd('/media/ngoctu/769829E69829A599/workspace/master2/master2/mixture models/projet/DATA_MATLAB - Projet-master-MLDS/')

jaffe = readMat('jaffe.mat')
jaffe$y = as.factor(jaffe$y)
jaffe = data.frame(jaffe$X, jaffe$y)

umist = readMat('UMIST1.mat')
umist$y = as.factor(umist$y)
umist = data.frame(umist$X, umist$y)

mnist = readMat('MNIST5.mat')
mnist$y = as.factor(mnist$y)
mnist = data.frame(mnist$X, mnist$y)

mfea = readMat('MFEAT1.mat')
mfea$y = as.factor(mfea$y)
mfea = data.frame(mfea$X, mfea$y)

coil20 = readMat('COIL20_1440n_1024d_20c.mat')
coil20$y = as.factor(coil20$y)
coil20 = data.frame(coil20$X, coil20$y)

usps = readMat('USPS.mat')
usps$y = as.factor(usps$y)
usps = data.frame(usps$X, usps$y)

optidigits = readMat('Optdigits.mat')
optidigits$y = as.factor(optidigits$y)
optidigits = data.frame(optidigits$X, optidigits$y)


# 3. Visualiser les nuages des points en croisant les variables deux à deux.
pairs(jaffe[, 1:5], col = jaffe$jaffe.y)

# 4. Visualiser l’ensemble des observations (individus) 
# sur votre premier plan factoriel en utilisant une analyse en composantes principales
library(FactoMineR)
library(Rtsne)
# jaffe
jaffe.acp = PCA(jaffe, ncp = 10, scale.unit = F, graph = F, quali.sup = 677)
plot(jaffe.acp, choix = 'ind')
plot(jaffe.acp, choix = 'ind', habillage = 677)

jaffe.tsne = Rtsne(jaffe[, -677], dims = 2, perplexity = 30, max_iter = 500, check_duplicate = F)
plot(jaffe.tsne$Y)
plot(jaffe.tsne$Y, col = jaffe[, 677])

# umist
umist.acp = PCA(umist, ncp = 10, scale.unit = F, graph = F, quali.sup = 645)
plot(umist.acp, choix = 'ind')
plot(umist.acp, choix = 'ind', habillage = 645)

umist.tsne = Rtsne(umist[, -645], dims = 2, perplexity = 30, max_iter = 500, check_duplicate = F)
plot(umist.tsne$Y)
plot(umist.tsne$Y, col = umist[, 645])

# mnist
mnist.acp = PCA(mnist, ncp = 10, scale.unit = F, graph = F, quali.sup = 785)
plot(mnist.acp, choix = 'ind')
plot(mnist.acp, choix = 'ind', habillage = 785)

mnist.tsne = Rtsne(mnist[, -785], dims = 2, perplexity = 50, max_iter = 1000, check_duplicate = F)
plot(mnist.tsne$Y)
plot(mnist.tsne$Y, col = mnist[, 785])

# mfea
mfea.acp = PCA(mfea, ncp = 10, scale.unit = F, graph = F, quali.sup = 241)
plot(mfea.acp, choix = 'ind')
plot(mfea.acp, choix = 'ind', habillage = 241)

mfea.tsne = Rtsne(mfea[, -241], dims = 2, perplexity = 50, max_iter = 1000, check_duplicate = F)
plot(mfea.tsne$Y)
plot(mfea.tsne$Y, col = mfea[, 241])

# coil20
coil20.acp = PCA(coil20, ncp = 10, scale.unit = F, graph = F, quali.sup = 1025)
plot(coil20.acp, choix = 'ind')
plot(coil20.acp, choix = 'ind', habillage = 1025)

coil20.tsne = Rtsne(coil20[, -1025], dims = 2, perplexity = 50, max_iter = 1000, check_duplicate = F)
plot(coil20.tsne$Y)
plot(coil20.tsne$Y, col = coil20[, 1025])

# usps
usps.acp = PCA(usps, ncp = 10, scale.unit = F, graph = F, quali.sup = 257)
plot(usps.acp, choix = 'ind')
plot(usps.acp, choix = 'ind', habillage = 257)

usps.tsne = Rtsne(usps[, -257], dims = 2, perplexity = 50, max_iter = 1000, check_duplicate = F)
plot(usps.tsne$Y)
plot(usps.tsne$Y, col = usps[, 257])

# optidigits
optidigits.acp = PCA(optidigits, ncp = 10, scale.unit = F, graph = F, quali.sup = 65)
plot(optidigits.acp, choix = 'ind')
plot(optidigits.acp, choix = 'ind', habillage = 65)

optidigits.tsne = Rtsne(optidigits[, -65], dims = 2, perplexity = 50, max_iter = 1000, check_duplicate = F)
plot(optidigits.tsne$Y)
plot(optidigits.tsne$Y, col = optidigits[, 65])


# 5. On cherchera à partitionner l’ensemble des observations, utiliser le package Nbclust
library(NbClust)

# jaffe
jaffe.nbclust.kmeans = NbClust(jaffe.acp$ind$coord, method = 'kmeans', min.nc = 2, max.nc = 20, index = 'all')
jaffe.nbclust.single = NbClust(jaffe.acp$ind$coord, method = 'single', min.nc = 2, max.nc = 20, index = 'all')
jaffe.nbclust.average = NbClust(jaffe.acp$ind$coord, method = 'average', min.nc = 2, max.nc = 20, index = 'all')
jaffe.nbclust.complete = NbClust(jaffe.acp$ind$coord, method = 'complete', min.nc = 2, max.nc = 20, index = 'all')
jaffe.nbclust.ward = NbClust(jaffe.acp$ind$coord, method = 'ward.D', min.nc = 2, max.nc = 20, index = 'all')

# umist
umist.nbclust.kmeans = NbClust(umistacp$ind$coord, method = 'kmeans', min.nc = 2, max.nc = 20, index = 'all')
umist.nbclust.single = NbClust(umist.acp$ind$coord, method = 'single', min.nc = 2, max.nc = 20, index = 'all')
umist.nbclust.average = NbClust(umist.acp$ind$coord, method = 'average', min.nc = 2, max.nc = 20, index = 'all')
umist.nbclust.complete = NbClust(umist.acp$ind$coord, method = 'complete', min.nc = 2, max.nc = 20, index = 'all')
umist.nbclust.ward = NbClust(umist.acp$ind$coord, method = 'ward.D', min.nc = 2, max.nc = 20, index = 'all')

# mnist
mnist.nbclust.kmeans = NbClust(mnist.acp$ind$coord, method = 'kmeans', min.nc = 2, max.nc = 20, index = 'all')
mnist.nbclust.single = NbClust(mnist.acp$ind$coord.acp$ind$coord, method = 'single', min.nc = 2, max.nc = 20, index = 'all')
mnist.nbclust.average = NbClust(mnist.acp$ind$coord, method = 'average', min.nc = 2, max.nc = 20, index = 'all')
mnist.nbclust.complete = NbClust(mnist.acp$ind$coord, method = 'complete', min.nc = 2, max.nc = 20, index = 'all')
mnist.nbclust.ward = NbClust(mnist.acp$ind$coord, method = 'ward.D', min.nc = 2, max.nc = 20, index = 'all')

# mfea
mfea.nbclust.kmeans = NbClust(mfea.acp$ind$coord.acp$ind$coord, method = 'kmeans', min.nc = 2, max.nc = 20, index = 'all')
mfea.nbclust.single = NbClust(mfea.acp$ind$coord.acp$ind$coord, method = 'single', min.nc = 2, max.nc = 20, index = 'all')
mfea.nbclust.average = NbClust(mfea.acp$ind$coord, method = 'average', min.nc = 2, max.nc = 20, index = 'all')
mfea.nbclust.complete = NbClust(mfea.acp$ind$coord, method = 'complete', min.nc = 2, max.nc = 20, index = 'all')
mfea.nbclust.ward = NbClust(mfea.acp$ind$coord, method = 'ward.D', min.nc = 2, max.nc = 20, index = 'all')

# coil20
coil20.nbclust.kmeans = NbClust(coil20.acp$ind$coord.acp$ind$coord, method = 'kmeans', min.nc = 2, max.nc = 20, index = 'all')
coil20.nbclust.single = NbClust(coil20.acp$ind$coord.acp$ind$coord, method = 'single', min.nc = 2, max.nc = 20, index = 'all')
coil20.nbclust.average = NbClust(coil20.acp$ind$coord, method = 'average', min.nc = 2, max.nc = 20, index = 'all')
coil20.nbclust.complete = NbClust(coil20.acp$ind$coord, method = 'complete', min.nc = 2, max.nc = 20, index = 'all')
coil20.nbclust.ward = NbClust(coil20.acp$ind$coord, method = 'ward.D', min.nc = 2, max.nc = 20, index = 'all')

# usps
usps.nbclust.kmeans = NbClust(usps.acp$ind$coord.acp$ind$coord, method = 'kmeans', min.nc = 2, max.nc = 20, index = 'all')
usps.nbclust.single = NbClust(usps.acp$ind$coord.acp$ind$coord, method = 'single', min.nc = 2, max.nc = 20, index = 'all')
usps.nbclust.average = NbClust(usps.acp$ind$coord, method = 'average', min.nc = 2, max.nc = 20, index = 'all')
usps.nbclust.complete = NbClust(usps.acp$ind$coord, method = 'complete', min.nc = 2, max.nc = 20, index = 'all')
usps.nbclust.ward = NbClust(usps.acp$ind$coord, method = 'ward.D', min.nc = 2, max.nc = 20, index = 'all')

# optidigits
optidigits.nbclust.kmeans = NbClust(optidigits.acp$ind$coord.acp$ind$coord, method = 'kmeans', min.nc = 2, max.nc = 20, index = 'all')
optidigits.nbclust.single = NbClust(optidigits.acp$ind$coord.acp$ind$coord, method = 'single', min.nc = 2, max.nc = 20, index = 'all')
optidigits.nbclust.average = NbClust(optidigits.acp$ind$coord, method = 'average', min.nc = 2, max.nc = 20, index = 'all')
optidigits.nbclust.complete = NbClust(optidigits.acp$ind$coord, method = 'complete', min.nc = 2, max.nc = 20, index = 'all')
optidigits.nbclust.ward = NbClust(optidigits.acp$ind$coord, method = 'ward.D', min.nc = 2, max.nc = 20, index = 'all')


# 6. Réaliser un spectral clustring en utilisant un package approprié.
library(kernlab)

# jaffe
jaffe.sc = specc(jaffe[, -677], kernel = "rbfdot", centers = 2)

# umist
umist.sc = specc(umist[, -645], kernel = "rbfdot", centers = 2)

# mnist
mnist.sc = specc(mnist[, -785], kernel = "rbfdot", centers = 2)

# mfea
mfeasc = specc(mfea[, -241], kernel = "rbfdot", centers = 2)

# coil20
coil20.sc = specc(coil20[, -1025], kernel = "rbfdot", centers = 2)

# usps
usps.sc = specc(usps[, -257], kernel = "rbfdot", centers = 2)

# optidigits
optidigits.sc = specc(optidigits[, -65], kernel = "rbfdot", centers = 2)


# 7. Quel nombre de classes peut-on proposer ?

# 8. On décide d’utiliser les algorithmes issus de l’approche mélange
library(Rmixmod)
library(mclust)

jaffe.mclust = Mclust(jaffe[, -677], 2)
jaffe.rmixmod = mixmodCluster(jaffe[, -677], 2)

# 9. visualiser les classes de l’ensemble des observations avec la fonction MclustDR
dr = MclustDR(jaffe.mclust)
plot(dr, what = 'scatterplot')
plot(dr, what = 'evalues')

# 10. On utilsera dans un premier temps, le taux de mal classés 
# issu de la table de confusion puis on évaluera cette qualité à l’aide la NMI et l’ARI.
# NMI
# ARI

# 11.
library(clustrd)