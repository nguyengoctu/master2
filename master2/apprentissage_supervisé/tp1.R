library('MASS')
data = cats
summary(data)
X = data[, -1]
y = data[, 1]

table(y)

boxplot(X)

# Histogramme
layout(matrix(c(1, 2),2,2))
for(i in 1:2) {
  hist(X[,i],main=names(X)[i],xlab="")}
layout(1)

pairs(X, col=as.numeric(y))

names(cats)

sd(cats$Bwt)
sd(cats$Hwt)

par(mfrow=c(2, 2))
plot(cats$Bwt, cats$Hwt, xlab =  'Poid du corps')
cor(cats$Bwt, cats$Hwt)

# regression linéaire de Hwt en Bwt
modele = lm(Hwt~Bwt)

summary(modele)[[0]]

confint(modele)

