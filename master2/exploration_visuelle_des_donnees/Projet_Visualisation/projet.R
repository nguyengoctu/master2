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

df = data.frame(X, y)
set.seed(2018)
tr = sample(1:nrow(X), as.integer(0.8 * nrow(X)))
train = df[tr, ]
test = df[-tr, ]

# Methodes
# 1. ACP
library(FactoMineR)
res.pca = PCA(gordon, scale.unit = T, ncp = 2, quali.sup = 1627)
plot.PCA(res.pca, choix = 'ind', habillage = 1627)

# 2. ADL
library(mda)
library(MASS)
# res.adl = fda(y ~ ., data = gordon[, -1627], dimension = 2)
res.adl = lda(y ~., data = train)
predict.adl = predict(res.adl, test[, -1627])
table(predict.adl$class, test[, 1627])

projected_data = as.matrix(df[, -1627]) %*% res.adl$scaling
plot(projected_data, col = df[, 1627], pch = 19)

# 3. MDS

# 4. Isomap
# 5. LLE
# 6. SOM