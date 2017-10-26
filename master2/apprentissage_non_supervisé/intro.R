data("iris")
dataset <- iris
filename = 'workspace/datasets/iris.data'
dataset = read.csv(filename, header = FALSE)
colnames(dataset) = c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width', 'Species')
library(caret)
validation_index = createDataPartition(dataset$Species, p=0.8, list = FALSE)
validation = dataset[-validation_index,]
dim(dataset)
dim(validation)
dataset = dataset[validation_index,]
dim(dataset)
sapply(dataset, class)
head(dataset)
levels(dataset$Species)
levels(dataset$Sepal.Length)

# summarize class distribution
percentage = prop.table(table(dataset$Species)) * 100
cbind(freq=table(dataset$Species), percentage=percentage)

# summarize attribute distributions
summary(dataset)

# Visualization
# split input and output
X = dataset[, 1:4]
y = dataset[, 5]

# boxplot for each attribute on one image
par(mfrow=c(1,4))
  for(i in 1:4){
  boxplot(X[,i], main=names(iris)[i])
}

# barplot for class breakdown
plot(y)


# scatterplot matrix
featurePlot(x = X, y = y, plot = 'ellipse')

# box and whisker plots for each attribute
featurePlot(x = X, y = y, plot = 'box')

# run algorithms using 10-fold cv
control = trainControl(method = 'cv', number = 10)
metric = 'Accuracy'

