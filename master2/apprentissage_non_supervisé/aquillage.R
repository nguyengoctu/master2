data = read.table('workspace/master2/apprentissage_non_supervise/Aiguillage.txt', header = FALSE, sep = ',')
X = data[, -553]
y = data[, 553]


summary(X)
D = dist(X, method = 'euclidean')
matplot(t(X), type = 'l', lty = 1, col = y)

H = hclust(D, method = 'single')
plot(rev(H$height), type = 'p')
plot(H)

H = hclust(D, method = 'complete')
plot(rev(H$height), type = 'h')
plot(H)

H = hclust(D, method = 'ward.D2')
plot(rev(H$height), type = 'h')
plot(H)

KM = kmeans(X, 3)
summary(KM)

summary(y)
