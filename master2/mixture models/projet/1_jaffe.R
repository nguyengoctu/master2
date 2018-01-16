# 2. Importer ces tables en utilisant la librairie R.matlab.
library(R.matlab)
setwd('/media/ngoctu/769829E69829A599/workspace/master2/master2/mixture models/projet/DATA_MATLAB - Projet-master-MLDS/')

donnees = readMat('jaffe.mat')
donnees$y = as.factor(donnees$y)
donnees = data.frame(donnees$X, donnees$y)
habillage = dim(donnees)[2]

# Normalisation
donnees[, -habillage] = scale(donnees[, -habillage], center = T, scale = T)


# 3. Visualiser les nuages des points en croisant les variables deux à deux.
pairs(donnees[, 1:5], col = donnees$donnees.y)



# 4. Visualiser l’ensemble des observations (individus) 
# sur votre premier plan factoriel en utilisant une analyse en composantes principales
library(FactoMineR)
library(Rtsne)
donnees.acp = PCA(donnees, ncp = 30, scale.unit = T, graph = F, quali.sup = habillage)
plot(donnees.acp, choix = 'ind', main = 'ACP')
plot(donnees.acp, choix = 'ind', habillage = habillage, main = 'ACP')

donnees.tsne = Rtsne(donnees[, -habillage], dims = 2, perplexity = 30, max_iter = 1000, check_duplicate = F)
plot(donnees.tsne$Y, xlab = 't-SNE dim 1', ylab = 't-SNE dim 2', main = 't-SNE')
plot(donnees.tsne$Y, col = donnees[, habillage], xlab = 't-SNE dim 1', ylab = 't-SNE dim 2', main = 't-SNE')


# 5. On cherchera à partitionner l’ensemble des observations, utiliser le package Nbclust
library(NbClust)
donnees.nbclust.kmeans = NbClust(donnees.acp$ind$coord, method = 'kmeans', min.nc = 2, max.nc = 20, index = 'all') 
# According to the majority rule, the best number of clusters is  3 

donnees.nbclust.single = NbClust(donnees.acp$ind$coord, method = 'single', min.nc = 2, max.nc = 20, index = 'all')
# According to the majority rule, the best number of clusters is  20 

donnees.nbclust.average = NbClust(donnees.acp$ind$coord, method = 'average', min.nc = 2, max.nc = 20, index = 'all')
# According to the majority rule, the best number of clusters is  20 

donnees.nbclust.complete = NbClust(donnees.acp$ind$coord, method = 'complete', min.nc = 2, max.nc = 20, index = 'all')
# According to the majority rule, the best number of clusters is  2 

donnees.nbclust.ward = NbClust(donnees.acp$ind$coord, method = 'ward.D', min.nc = 2, max.nc = 20, index = 'all')
# According to the majority rule, the best number of clusters is  20 


# 6. Réaliser un spectral clustring en utilisant un package approprié.
library(kernlab)
donnees.sc = specc(donnees[, -habillage], centers = 20)


# 7. Quel nombre de classes peut-on proposer ? 20

# 8. On décide d’utiliser les algorithmes issus de l’approche mélange
library(Rmixmod)
library(mclust)

donnees.mclust = Mclust(donnees[, -habillage], 20)
donnees.rmixmod = mixmodCluster(donnees[, -habillage], 20)


# 9. visualiser les classes de l’ensemble des observations avec la fonction MclustDR
dr = MclustDR(donnees.mclust)
plot(dr, what = 'scatterplot')
plot(dr, what = 'evalues')


# 10. On utilsera dans un premier temps, le taux de mal classés 
# issu de la table de confusion puis on évaluera cette qualité à l’aide la NMI et l’ARI.
# ARI
adjustedRandIndex(donnees$donnees.y, donnees.mclust$classification) # 0.8056816

# 11.
library(clustrd)