data("iris")
X = iris[, 1:4]
label = iris[, 5]
summary(X)

table(label)

boxplot(X)
