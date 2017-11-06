data = read.table('workspace/master2/apprentissage_non_supervisé/projet/sequencesimu.txt')
X = iris[, -5]
X = matrix(sample.int(10, size = 5 * 3, replace = TRUE), nrow = 5, ncol = 3)
X

# distance function
diam = function(X, a, b){
  if (a == b) {
    0
  }
  else{
    sum((scale(X[a:b, ], center = TRUE, scale = FALSE)) ** 2)
  }
}

# Fisher Clustering
clustfisher = function(X, K){
  # X: donnee
  # K: nombre de segments souhaite
  
  # Etape 0: Initialiser les matrices D, M1, M2, ainsi que le vecteur t des instants de changement
  # et le vecteur des classes (sous R, on pourra utiliser des matrices et des vecteurs nuls)
  
  # D: matrice taille n x n (triangulaire superieure)

  n = nrow(X)
  D = matrix(NA, nrow = n, ncol = n)
  M1 = matrix(NA, nrow = n, ncol = n)
  M2 = matrix(NA, nrow = n, ncol = n)
  t = rep(NA, K - 1)
  label = rep(NA, K)
  
  # Etape 1: calcul de la matrice triangulaire superieure des diametres
  for (a in 1:n){
    for (b in a:n){
      D[a, b] = diam(X, a, b)
    }
  }
  
  
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
    label[i] = 1
  }
  
  for (k in 2:(K - 1)){
    for (i in t[k - 1]:(t[k] - 1)){
      label[i] = k
    }
  }
  
  for (i in (t[K - 1]:n)){
    label[i] = K
  }
  
  list('label' = label, 't' = t)
}

X = data
H_fisher = clustfisher(X, 4)
index = c(1:nrow(X))
plot(index, X$V1,type = 'l')
X$V1
H_fisher
