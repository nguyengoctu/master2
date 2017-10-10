library('cluster')
data = iris[, -5]

# Matrice des distances
D = dist(data, method = 'euclidean')

# Mise en oeuvre de l'algorithm
H = hclust(D, method = 'ward.D2')
H = hclust(D, method = 'complete')  # maximum distance
H = hclust(D, method = 'single')    # minimum distance  

# Representation graphique (dendogramme)
plot(rev(H$height), type = 'h')
plot(H)

# Coupe de l'arbre pour trouver la meilleure partition en K = 2 classes
classes = cutree(H, k = 3)
plot(classes)

# Graphique dans le premier plan principal (ACP)
ACP = princomp(data)
plot(ACP$scores[, 1], ACP$scores[, 2], col=classes, pch=classes)

H = agnes(D, method = 'ward')
plot(H, which.plots = 2)
plot(as.hclust(H), hang = -1)
rect.hclust(as.hclust(H), k = 3, border = 'red')

