# distance function
diam = function(X, a, b){
  if (a == b) {
    0
  }
  else{
    sum((scale(X[a:b, ], center = TRUE, scale = FALSE)) ** 2)
  }
}

distance = function(X){
  n = nrow(X)
  D = matrix(NA, nrow = n, ncol = n)
  for (a in 1:n){
    for (b in a:n){
      D[a, b] = diam(X, a, b)
    }
  }
  D
}

# Fisher Clustering
clustfisher = function(D, K){
  # X: donnee
  # K: nombre de segments souhaite
  
  # Etape 0: Initialiser les matrices D, M1, M2, ainsi que le vecteur t des instants de changement
  # et le vecteur des classes (sous R, on pourra utiliser des matrices et des vecteurs nuls)
  
  # D: matrice taille n x n (triangulaire superieure)

  n = nrow(D)
  # D = matrix(NA, nrow = n, ncol = n)
  M1 = matrix(NA, nrow = n, ncol = n)
  M2 = matrix(NA, nrow = n, ncol = n)
  t = rep(NA, K - 1)
  cluster = rep(NA, K)
  
  # Etape 1: calcul de la matrice triangulaire superieure des diametres
  # for (a in 1:n){
  #   for (b in a:n){
  #     D[a, b] = diam(X, a, b)
  #   }
  # }
  
  
  # Etape 2: calcul recursif des criteres optimaux
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
  
  
  # Etape 3: Calcul recursif des instants de changement optimaux
  k = K - 1
  m = n
  while (k >= 1){
    t[k] = M2[m, k + 1]
    m = t[k] - 1
    k = k - 1
  }
  
  
  # Etape 4: labels des classes formes a partir des instants de changement
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
  
  list('cluster' = cluster, 't' = t, 'tot.withinss' = M1[n, K], 'totss' = D[1, n])
}

clustering = function(X){
  # Methode du coude
  inerties_totales_intra_classes = rep(NA, 10)
  #clusts_fisher = rep(NA, 10)
  D = distance(X)
  n = nrow(X)
  inerties_totales_intra_classes[1] = D[1, n]
  cat("Calculer inertie totale intra-classes...\n")
  
  for (K in 2:10){
    cat("Avec K =", K, "...\n")
    clust_fisher = clustfisher(D, K)
    inerties_totales_intra_classes[K] = clust_fisher$tot.withinss
  }

  plot(inerties_totales_intra_classes,
       xlab = 'Nombre de classes',
       ylab = 'Inertie intra-classes',
       main = 'Nombre optimal de classes',
       type = 'o')
  
  # Choisir K
  K = 1
  while (T){
    K = as.numeric(readline(prompt = 'K = '))
    if (K < 2){
      cat('K invalid, repetez svp')
    }
    else{
      break
    }
  }
  clust_fisher = clustfisher(D, K)
  cat('fisher clustering...done\n')
  clust_kmeans = kmeans(X, K)
  cat('kmeans clustering...done\n')
  D = dist(X, method = 'euclidean')
  clust_cah_ward = hclust(D, method = 'ward.D2')
  cat('CAH-Ward clustering...done\n')
  list('clust_fisher' = clust_fisher, 'clust_kmeans' = clust_kmeans, 'clust_cah_ward' = clust_cah_ward, 'K' = K)
}

sequenceimu = read.table('workspace/master2/apprentissage_non_supervisé/projet/sequencesimu.txt')
sequenceimu_clustering = clustering(sequenceimu)

boxplot(sequenceimu)

par(mfrow = c(2, 2))
plot(sequenceimu$V1, main = 'Jeu des donnees: sequenceimu')
plot(sequenceimu$V1, main = 'Fisher clustering', col = sequenceimu_clustering$clust_fisher$cluster)
plot(sequenceimu$V1, main = 'K means clustering', col = sequenceimu_clustering$clust_kmeans$cluster)
plot(sequenceimu$V1, main = 'CAH Ward clustering', col = cutree(sequenceimu_clustering$clust_cah_ward, k = sequenceimu_clustering$K))


aquillage = read.table('workspace/master2/apprentissage_non_supervisé/Aiguillage.txt', header = FALSE, sep = ',')
aquillage_label = aquillage[, 553]
aquillage = aquillage[, -553]

aquillage_clustering = clustering(aquillage)
par(mfrow = c(2, 2))
matplot(t(aquillage), main = 'Jeu des donnees: aquillage', type = 'l', lty = 1, col = aquillage_label)
matplot(t(aquillage), main = 'Fisher clustering', type = 'l', lty = 1, col = aquillage_clustering$clust_fisher$cluster)
matplot(t(aquillage), main = 'K means clustering', type = 'l', lty = 1, col = aquillage_clustering$clust_kmeans$cluster)
matplot(t(aquillage), main = 'CAH Ward clustering', type = 'l', lty = 1, col = cutree(aquillage_clustering$clust_cah_ward, k = aquillage_clustering$K))

KM = kmeans(aquillage, 3)
matplot(t(aquillage), main = 'CAH Ward clustering', type = 'l', lty = 1, col = KM$cluster)
