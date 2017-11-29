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
