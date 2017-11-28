diam = function(X, a, b){
  # Cette fonction calcule le diamètre
  #
  # Inputs :
  # - X : matrice de données, avec la taille : 𝑛 lignes, 𝑝 colonnes.
  # - a : nombre entier
  # - b : nombre entier (b >= a)
  # 
  # Outputs :
  #   - Valeur réelle calculée selon la formule donné
  
  if (a == b) {
    0
  }
  else{
    sum((scale(X[a:b, ], center = TRUE, scale = FALSE)) ** 2)
  }
}


diam_matrice = function(X){
  # Cette fonction calcule la matrice des diamètres des classes (Etape 0)
  # 
  # Inputs :
  #   - X : matrice de données
  # 
  # Outputs :
  #   - Matrice de diamètres des classes
   n = nrow(X)
  D = matrix(NA, nrow = n, ncol = n)
  for (a in 1:n){
    for (b in a:n){
      D[a, b] = diam(X, a, b)
    }
  }
  D
}


clustfisher = function(D, K){
  # Cette fonction utilise l’algorithme de programmation dynamique de Fisher 
  # pour classifier des données représentées par la matrice des diamètres des classes D en K classes.
  # 
  # Inputs :
  # - D : matrice des diamètres des classes
  # - K : nombre de classes
  # 
  # Outputs :
  # - cluster : vecteur des labels des individus
  # - t : vecteur des instants de changement
  # - tot.withinss : valeur d’inertie intra-classes

  n = nrow(D)
  # D = matrix(NA, nrow = n, ncol = n)

    M1 = matrix(NA, nrow = n, ncol = n)
  M2 = matrix(NA, nrow = n, ncol = n)
  t = rep(NA, K - 1)
  cluster = rep(NA, n)
  
  # Etape 1: calcul de la matrice triangulaire supérieure des diamètres
  # for (a in 1:n){
  #   for (b in a:n){
  #     D[a, b] = diam(X, a, b)
  #   }
  # }
  
  
  # Etape 2: calcul récursif des critères optimaux
  for (i in 1:n){
    M1[i, 1] = D[1, i]
  }
  
  for (k in 2:K){
    for (i in k:n){
      t_ = c(k:i)
      tmp = M1[t_ - 1, k - 1] + D[t_, i]
      M1[i, k] = min(tmp)
      M2[i, k] = which.min(tmp) + k - 1
    }
  }
  
  
  # Etape 3: Calcul récursif des instants de changement optimaux
  k = K - 1
  m = n
  while (k >= 1){
    t[k] = M2[m, k + 1]
    m = t[k] - 1
    k = k - 1
  }
  
  
  # Etape 4: labels des classes formés à partir des instants de changement
  for (i in 1:(t[1] - 1)){
    cluster[i] = 1
  }
  
  if (K > 2){
    for (k in 2:(K - 1)){
      for (i in t[k - 1]:(t[k] - 1)){
        cluster[i] = k
      }
    }
  }
  
  
  for (i in (t[K - 1]:n)){
    cluster[i] = K
  }
  
  list('cluster' = cluster, 't' = t, 'tot.withinss' = M1[n, K])
}


clustering = function(X, K){
  # L’objectif de cette fonction est de résoudre les questions 2) et 3). 
  # Car il y a deux jeux des données à travailler, je l’ai implémenté 
  # pour éviter le redoublement du code. Cette fonction va prendre les données X 
  # et les classifier en K classes, en utilisant trois algorithmes de classification : 
  # Fisher, K-means et CAH-Ward.
  # 
  # Inputs :
  # - X : matrice des données
  # - K : nombre de classes voulu. Si K se présente, cette fonction va classifier X en K classes. 
  # Si non, elle va classifier X plusieurs fois avec le K varie (en utilisant l’algorithme Fisher). 
  # Ensuite, elle va montrer un graphe des inerties intra-classes et vous laisser choisir K manuellement.
  # 
  # Outputs :
  # - clust_fisher : résultat obtenu avec l’algorithme de programmation dynamique de Fisher
  # - clust_kmeans : résultat obtenu avec l’algorithme K-means
  # - clust_cah_ward : résultat obtenu avec l’algorithme CAH-Ward
  
  #clusts_fisher = rep(NA, 10)
  D = distance(X)
  n = nrow(X)
  
  # Si K ne se présente pas, je vais classifier X plusieurs fois avec le K varie 
  # (en utilisant l’algorithme Fisher) pour la méthode du coude. 
  if (missing(K)){
    cat("Calcul d'inertie intra-classe pour la méthode du coude...\n")
    inerties_intra_classes = rep(NA, 10)
    inerties_intra_classes[1] = D[1, n]
    
    for (K in 2:10){
      cat("avec K =", K, "...\n")
      clust_fisher = clustfisher(D, K)
      inerties_intra_classes[K] = clust_fisher$tot.withinss
    }
  
    plot(inerties_intra_classes,
         xlab = 'Nombre de classes',
         ylab = 'Inertie intra-classes',
         main = 'Méthode du coude',
         type = 'o')
    
    # Choisir K
    K = 1
    while (T){
      K = as.numeric(readline(prompt = 'K = '))
      if (K < 2){
        cat('K est invalide, répétez svp')
      }
      else{
        break
      }
    }
  }
  
  clust_fisher = clustfisher(D, K)
  cat('fisher clustering...terminé\n')
  
  clust_kmeans = kmeans(X, K)
  cat('kmeans clustering...terminé\n')
  
  D = dist(X, method = 'euclidean')
  clust_cah_ward = hclust(D, method = 'ward.D2')
  cat('CAH-Ward clustering...terminé\n')
  list('clust_fisher' = clust_fisher, 'clust_kmeans' = clust_kmeans, 'clust_cah_ward' = clust_cah_ward, 'K' = K)
}

# Jeu de données simulées: sequencesimu
sequencesimu = read.table('workspace/master2/apprentissage_non_supervisé/projet/sequencesimu.txt')
summary(sequencesimu)
boxplot(sequencesimu)

sequencesimu_clustering = clustering(sequencesimu)

par(mfrow = c(2, 2))
plot(sequencesimu$V1, main = 'Jeu de données: sequencesimu', ylab = 'X')
plot(sequencesimu$V1, main = 'Fisher clustering', col = sequencesimu_clustering$clust_fisher$cluster, ylab = 'X')
plot(sequencesimu$V1, main = 'K-means clustering', col = sequencesimu_clustering$clust_kmeans$cluster, ylab = 'X')
plot(sequencesimu$V1, main = 'CAH-Ward clustering', col = cutree(sequencesimu_clustering$clust_cah_ward, k = sequencesimu_clustering$K), ylab = 'X')


# Jeu de données réelles: aiguillage
aiguillage = read.table('workspace/master2/apprentissage_non_supervisé/Aiguillage.txt', header = FALSE, sep = ',')
aiguillage_label = aiguillage[, 553]
aiguillage = aiguillage[, -553]

smmary(aiguillage)

K
# Cette ligne de code diminue enormement le temps d'execution
aiguillage = as.matrix(aiguillage) 
= 4
aiguillage_clustering = clustering(aiguillage, K = K)

par(mfrow = c(2, 2))
matplot(t(aiguillage), main = 'Jeu de données: aiguillage', type = 'l', lty = 1, col = aiguillage_label, xlab = 'time', ylab = 'Power (Watts)')
matplot(t(aiguillage), main = 'Fisher clustering', type = 'l', lty = 1, col = aiguillage_clustering$clust_fisher$cluster, xlab = 'time', ylab = 'Power (Watts)')
matplot(t(aiguillage), main = 'K-means clustering', type = 'l', lty = 1, col = aiguillage_clustering$clust_kmeans$cluster, xlab = 'time', ylab = 'Power (Watts)')
matplot(t(aiguillage), main = 'CAH-Ward clustering', type = 'l', lty = 1, col = cutree(aiguillage_clustering$clust_cah_ward, k = K), xlab = 'time', ylab = 'Power (Watts)')


# Evaluer la performance des algorithmes avec l'Indice de Rand (Adjusted Rand Index)
library(mclust)

# Performance de l'algorithme de Fisher
cat("L'Indice de Rand entre des labels vrais et des labels Fisher: ", adjustedRandIndex(aiguillage_label, aiguillage_clustering$clust_fisher$cluster))

# Performance de l'algorithme de CAH-Ward
cat("L'Indice de Rand entre des labels vrais et des labels CAH-Ward: ", adjustedRandIndex(aiguillage_label, cutree(aiguillage_clustering$clust_cah_ward, k = K)))


# Re-classifier plusieurs fois avec K-means
par(mfrow = c(2, 2))
max_rand_index = 0.0

best_KM = NA
for (i in 1:4){
  KM = kmeans(aiguillage, 4)
  if (adjustedRandIndex(aiguillage_label, KM$cluster) > max_rand_index){
    best_KM = KM
    max_rand_index = adjustedRandIndex(aiguillage_label, best_KM$cluster)
  }
  matplot(t(aiguillage), type = 'l', lty = 1, col = KM$cluster, xlab = 'time', ylab = 'Power (Watts)')
}
cat("Le meilleur K-means a l'indice Rand: ", max_rand_index)


# Evaluer le temps d'exécution
system.time(diam_matrice(aiguillage))
system.time(clustering(aiguillage, 4))
