data = read.table('workspace/master2/apprentissage_non_supervise/Velib.txt', header = T)
summary(data)
y = data[, 1]
coordinates = data[, 2:3]
X = data[, 4:171]
X = as.matrix(data[, -(1:3)])

n = dim(X)[1]
ii = sample(n, 1) # tirage aleatoire d'une station
plot(X[ii, ], main = y[ii])
lines(X[ii, ], main = y[ii])
hist(X[ii, ], main = y[ii], breaks = 35)


# lon <- c(-38.31,-35.5)
# lat <- c(40.96, 37.5)
# df <- as.data.frame(cbind(lon,lat))


# # getting the map
# mapgilbert <- get_map(location = c(lon = mean(df$lon), lat = mean(df$lat)), zoom = 4,
#                       maptype = "satellite", scale = 2)
# 
# # plotting the map with some points on it
# ggmap(mapgilbert) +
#   geom_point(data = df, aes(x = lon, y = lat, fill = "red", alpha = 0.8), size = 5, shape = 21) +
#   guides(fill=FALSE, alpha=FALSE, size=FALSE)

library(fpc)
KM = kmeans(X, centers = 5)

plotcluster(X, KM$cluster)

matplot(t(X[1:4, ]), type = 'l', lty = 1, col = y[1:4])

library('cluster')
D = dist(X, method = 'euclidean')
H = agnes(D, method = 'ward')
plot(H, which.plots = 2)
plot(as.hclust(H), hang = -1)
rect.hclust(as.hclust(H), k = 4, border = 'red')

acp = princomp(X)
cumul = cumsum(acp$sdev^2/sum(acp$sdev^2))
ncomp = min(which((cumul >= 0.95)))

X_compressed = acp$scores[, 1:ncomp]
D = dist(X_compressed, method = 'euclidean')
H = hclust(D, method = 'ward.D2')
classes = cutree(H, k = 6)
plot(H)
plot(coordinates, col=classes)

position=c(min(coordinates[, 1]), min(coordinates[,2]), max(coordinates[, 1]), max(coordinates[, 2]))
mymap=get_map(location=position, source="google", maptype="roadmap")

ggmap(mymap)+geom_point(aes(coordinates[, 1], coordinates[, 2]),
              data = data.frame(longitude=coordinates[, 1],latitude=coordinates[, 2]),
              alpha = .5, color=classes, size = 4)

centres = matrix(0, 6, 168)
for (k in 1:6){
  centres[k, ] = colMeans(X[classes == k, ])
}
plot.ts(t(centres))
matplot(t(centres), type = 'l')
