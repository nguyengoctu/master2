# 1. Lire les donnees
spam = read.table('~/master2/master2/coclustering/spam.dat', header = T)
spam[, 1] = as.factor(spam[, 1])

# 2. Description elementaire
Lspam = data.frame('spam' = spam[, 1], log(1 + spam[, 2:58]))

# 3. Approche quantitative
# 3.1. ACP
library(FactoMineR)
res.pca1 = PCA(spam, scale.unit = F, quali.sup = 1)
res.pca2 = PCA(spam, scale.unit = T, quali.sup = 1)
res.pca3 = PCA(Lspam, scale.unit = F, quali.sup = 1) 
res.pca4 = PCA(Lspam, scale.unit = T, quali.sup = 1)

plot.pca.result = function(res.pca){
  barplot(res.pca$eig[, 1], main = 'Eigenvalues', names.arg = 1:nrow(res.pca$eig))
  plot(res.pca, choix = 'ind', habillage = 1, cex = 0.5)
  plot(res.pca, choix = 'var')
  dimdesc(res.pca, axes =  c(1, 2))
}

plot.pca.result(res.pca4)


# 3.2. Classification des variables
dist.var = as.dist(1 - cor(Lspam[2:58]) ** 2)
clas.var = hclust(dist.var, method = 'ward.D2')
plot(clas.var)
plot(clas.var$height[56:40])

rS = cor(Lspam[2:58])
dS2 = sqrt(1 - rS ** 2)
dN = dimnames(Lspam[2:58])[[2]]
mdspam = cmdscale(dS2, k = 2)
plot(mdspam, type = 'n')
text(mdspam, dN)
abline(v = 0, h = 0)

classes = cutree(clas.var, k = 4)
sort(classes)
names(classes[classes == 2])
coul = classes
plot(mdspam, type = 'n', xlab = 'Dim 1', ylab = 'Dim 2', main = 'CAH euclid')
text(mdspam, dN, col = coul)


