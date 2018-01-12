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
jaffe.acp = PCA(jaffe, ncp = 2, scale.unit = F, graph = F, quali.sup = 677)
plot(jaffe.acp, choix = 'ind')
plot(jaffe.acp, choix = 'ind', habillage = 677)

jaffe.tsne = Rtsne(jaffe[, -677], dims = 2, perplexity = 30, max_iter = 500, check_duplicate = F)
plot(jaffe.tsne$Y)
plot(jaffe.tsne$Y, col = jaffe[, 677])

# umist
umist.acp = PCA(umist, ncp = 2, scale.unit = F, graph = F, quali.sup = 645)
plot(umist.acp, choix = 'ind')
plot(umist.acp, choix = 'ind', habillage = 645)

umist.tsne = Rtsne(umist[, -645], dims = 2, perplexity = 30, max_iter = 500, check_duplicate = F)
plot(umist.tsne$Y)
plot(umist.tsne$Y, col = umist[, 645])

# mnist
mnist.acp = PCA(mnist, ncp = 2, scale.unit = F, graph = F, quali.sup = 785)
plot(mnist.acp, choix = 'ind')
plot(mnist.acp, choix = 'ind', habillage = 785)

mnist.tsne = Rtsne(mnist[, -785], dims = 2, perplexity = 50, max_iter = 1000, check_duplicate = F)
plot(mnist.tsne$Y)
plot(mnist.tsne$Y, col = mnist[, 785])

# mfea
mfea.acp = PCA(mfea, ncp = 2, scale.unit = F, graph = F, quali.sup = 241)
plot(mfea.acp, choix = 'ind')
plot(mfea.acp, choix = 'ind', habillage = 241)

mfea.tsne = Rtsne(mfea[, -241], dims = 2, perplexity = 50, max_iter = 1000, check_duplicate = F)
plot(mfea.tsne$Y)
plot(mfea.tsne$Y, col = mfea[, 241])

# coil20
coil20.acp = PCA(coil20, ncp = 2, scale.unit = F, graph = F, quali.sup = 1025)
plot(coil20.acp, choix = 'ind')
plot(coil20.acp, choix = 'ind', habillage = 1025)

coil20.tsne = Rtsne(coil20[, -1025], dims = 2, perplexity = 50, max_iter = 1000, check_duplicate = F)
plot(coil20.tsne$Y)
plot(coil20.tsne$Y, col = coil20[, 1025])

# usps
usps.acp = PCA(usps, ncp = 2, scale.unit = F, graph = F, quali.sup = 257)
plot(usps.acp, choix = 'ind')
plot(usps.acp, choix = 'ind', habillage = 257)

usps.tsne = Rtsne(usps[, -257], dims = 2, perplexity = 50, max_iter = 1000, check_duplicate = F)
plot(usps.tsne$Y)
plot(usps.tsne$Y, col = usps[, 257])

# optidigits
optidigits.acp = PCA(optidigits, ncp = 2, scale.unit = F, graph = F, quali.sup = 65)
plot(optidigits.acp, choix = 'ind')
plot(optidigits.acp, choix = 'ind', habillage = 65)

optidigits.tsne = Rtsne(optidigits[, -65], dims = 2, perplexity = 50, max_iter = 1000, check_duplicate = F)
plot(optidigits.tsne$Y)
plot(optidigits.tsne$Y, col = optidigits[, 65])


# On cherchera à partitionner l’ensemble des observations, utiliser le package Nbclust
library(NbClust)
