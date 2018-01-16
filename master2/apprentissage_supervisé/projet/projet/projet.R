# Donnees synthetiques: flame, spiral, aggregation
flame = read.table('/media/ngoctu/769829E69829A599/workspace/master2/master2/apprentissage_supervisé/projet/Projet_App-Sup_MLDS18/Data_projet_MLDS18/flame.txt', 
                   header = F,
                   sep = '\t')
spiral = read.table('/media/ngoctu/769829E69829A599/workspace/master2/master2/apprentissage_supervisé/projet/Projet_App-Sup_MLDS18/Data_projet_MLDS18/spiral.txt',
                    header = F,
                    sep = '\t')
aggregation = read.table('/media/ngoctu/769829E69829A599/workspace/master2/master2/apprentissage_supervisé/projet/Projet_App-Sup_MLDS18/Data_projet_MLDS18/Aggregation.txt',
                         header = F,
                         sep = '\t')

# Donnees reelles: visa_premier, credit_card_fraud
visapremier = read.table('/media/ngoctu/769829E69829A599/workspace/master2/master2/apprentissage_supervisé/projet/Projet_App-Sup_MLDS18/Data_projet_MLDS18/VisaPremier.txt', 
                         header = T, 
                         sep = '\t')
creditcard = read.table('~/Downloads/Projet_App-Sup_MLDS18/Data_projet_MLDS18/creditcard.csv', header = T, sep = ',')

# AUC scores
auc.scores = data.frame(matrix(ncol = 11, nrow = 5))
column.names = c('Dataset', 'Bayesien Naif', 'KNN', 'LDA', 
                 'QDA', 'Linear SVM', 'Polynomial SVM', 
                 'Gaussian SVM', 'Logistic Regression', 'CART', 'Random Forest')
colnames(auc.scores) = column.names


# Accuracy on test data
accuracies = data.frame(matrix(ncol = 11, nrow = 5))
colnames(accuracies) = column.names


setwd('/media/ngoctu/769829E69829A599/workspace/master2/master2/apprentissage_supervisé/projet/Projet_App-Sup_MLDS18/3. aggregation/')

data = spiral
data[is.na(data)] = 0
# data = data[, -1]
name = 'spiral'
row = 2

auc.scores[row, 1] = name
accuracies[row, 1] = name


dim(data)
View(data)

# data$Amount = scale(data$Amount)

col = c('anciente', 'mtrejet', 'nbopguic', 'moycred3', 'aveparmo', 'endette', 'engagemt', 'engagemc', 'engagemm', 'nbcptvue', 'moysold3', 'moycredi', 'agemvt', 'nbop', 'mtfactur', 'engageml', 'nbvie', 'mtvie', 'nbeparmo', 'mteparmo', 'nbeparlo', 'mteparlo', 'nblivret', 'mtlivret', 'nbeparlt', 'nbeparte', 'mteparte', 'nbbon', 'mtbon', 'nbpaiecb', 'nbcb', 'nbcbptar', 'avtscpte', 'aveparfi', 'nbjdebit')
# X = data[, col]
# y = as.factor(data$cartevpr)

X = data[, -3]
y = as.factor(data[, 3])
summary(X)
summary(y)

png(paste(name,'.png'), width = 800, height = 600, units = 'px')
plot(X, col = y, main = name)
dev.off()

png(paste(name,'.boxplot.png'), width = 800, height = 600, units = 'px')
boxplot(X, main = name)
dev.off()

png(paste(name,'.histogram.png'), width = 800, height = 600, units = 'px')
par(mfrow = c(1, 2))
hist(X$V1, main = paste('Histogram of ',name, "'s V1"))
hist(X$V2, main = paste('Histogram of ',name, "'s V2"))
dev.off()


cl = cor(X)
library(corrplot)
png(paste(name,'.cor.png'), width = 800, height = 600, units = 'px')
corrplot(cl, method = 'square')
dev.off()


# Splitting train-val-test (60-20-20)
set.seed(2018)
num_train_val = as.integer(0.80 * nrow(X))
tr_val = sample(1:nrow(X), num_train_val)
X_test = X[-tr_val, ]
y_test = y[-tr_val]

X_train_val = X[tr_val, ]
y_train_val = y[tr_val]

num_train = as.integer(0.6 * nrow(X))
tr = sample(1:nrow(X_train_val), num_train)

X_train = X_train_val[tr, ]
y_train = y_train_val[tr]

X_val = X_train_val[-tr, ]
y_val = y_train_val[-tr]

# Normalization
# Get the shift and scale from the training data
X_train_val_std = apply(X_train_val, 2, sd)
X_train_val_mean = colMeans(X_train_val)

# Normalize train data
X_train_val = scale(X_train_val)
X_train = scale(X_train, scale = X_train_val_std, center = X_train_val_mean)
X_val = scale(X_val, scale = X_train_val_std, center = X_train_val_mean)

# Normalize test data
X_test = scale(X_test, scale = X_train_val_std, center = X_train_val_mean)

  
# ROC curve
library(ROCR)

# Methodes
# 1. Bayesien Naif
library(e1071)
nb = naiveBayes(X_train_val, y_train_val)
nb.prob = predict(nb, X_test, type = 'raw')
# nb.prob = predict(nb, X_test)
nb.preds = prediction(nb.prob[, 2], y_test)
nb.roc = performance(nb.preds, 'tpr', 'fpr')
nb.auc = performance(nb.preds, measure = 'auc')
auc.scores[row, 2] = nb.auc@y.values[[1]]
accuracies[row, 2] = mean(predict(nb, X_test) == y_test)


# 2. KNN
library(class)

# Find the best K
kmax = 50
err_val = rep(NA, kmax)
for (k in 1:kmax){
  pred = knn(X_train, X_val, y_train, k)
  err_val[k] = sum(pred != y_val) / length(y_val)
}
# plot(err_val, type = "l", col = 2, xlab = "nombre de voisins", 
#     ylab = "taux d'erreur") 
K = which.min(err_val) 

knn.predictions = knn(X_train_val, X_test, y_train_val, K, prob = T)
# knn.predictions = knn(X_train_val, X_test, y_train_val, K)

knn.prob = attr(knn.predictions, 'prob')
knn.prob <- 2*ifelse(knn.predictions == 0, 1 - knn.prob, knn.prob) - 1
knn.pred = prediction(knn.prob, y_test)
knn.roc = performance(knn.pred, 'tpr', 'fpr')
knn.auc = performance(knn.pred, measure = 'auc')
auc.scores[row, 3] = knn.auc@y.values[[1]]

# plot(knn.roc)
accuracies[row, 3] = mean(knn(X_train_val, X_test, y_train_val, K) == y_test)


# 3. LDA
library(MASS)
lda.res = lda(X_train_val, y_train_val)
lda.prob = predict(lda.res, X_test)$posterior
lda.pred = prediction(lda.prob[, 2], y_test)
lda.roc = performance(lda.pred, 'tpr', 'fpr')
lda.auc = performance(lda.pred, measure = 'auc')
auc.scores[row, 4] =lda.auc@y.values[[1]]
accuracies[row, 4] = mean(predict(lda.res, X_test)$class == y_test)
# plot(lda.roc)


# 4. QDA?
qda.res = qda(X_train_val, y_train_val)
qda.prob = predict(qda.res, X_test)$posterior
qda.pred = prediction(qda.prob[, 2], y_test)
qda.roc = performance(qda.pred, 'tpr', 'fpr')
qda.auc = performance(qda.pred, measure = 'auc')
auc.scores[row, 5] = qda.auc@y.values[[1]]
accuracies[row, 5] = mean(predict(qda.res, X_test)$class == y_test)
# plot(qda.roc)


# SVM
costs = c(1e+3, 1, 1e-3)
degrees = c(2, 3, 5, 10)
gammas = c(1e-3, 1e-5)

# 5. Linear SVM
best_cost = 1
best_accuracy = 0.0
for (cost in costs){
  lin.svm.res = svm(X_train, y_train, kernel = 'linear', cost = cost)
  acc = mean(predict(lin.svm.res, X_val) == y_val)
  if (acc > best_accuracy){
    best_accuracy = acc
    best_cost = cost
  }
}

lin.svm.res = svm(X_train_val, y_train_val, kernel = 'linear', cost = best_cost, probability = TRUE)
# lin.svm.res = svm(X_train_val, y_train_val, kernel = 'linear', cost = best_cost)
lin.svm.prob = attr(predict(lin.svm.res, X_test, probability = T), 'probabilities')
lin.svm.pred = prediction(lin.svm.prob[, 2], y_test)
lin.svm.roc = performance(lin.svm.pred, 'tpr', 'fpr')
lin.svm.auc = performance(lin.svm.pred, measure = 'auc')
auc.scores[row, 6] = lin.svm.auc@y.values[[1]]
accuracies[row, 6] = mean(predict(lin.svm.res, X_test) == y_test)
# plot(lin.svm.roc)

# 6. Non-linear SVM
# 6.1. Polynomial SVM
best_cost = 0.0
best_gamma = 0.0
best_degree = 0
best_accuracy = 0.0
for (cost in costs){
  for (gamma in gammas){
    for (degree in degrees){
      poly.svm.res = svm(X_train, y_train, kernel = 'polynomial', cost = cost, gamma = gamma, degree = degree)
      acc = mean(predict(poly.svm.res, X_val) == y_val)
      if (acc > best_accuracy){
        best_degree = degree
        best_accuracy = acc
        best_cost = cost
        best_gamma = gamma
      }
    }
  }
}

poly.svm.res = svm(X_train_val, y_train_val, kernel = 'polynomial', 
                  cost = best_cost, gamma = best_gamma, degree = best_degree, probability = TRUE)

# poly.svm.res = svm(X_train_val, y_train_val, kernel = 'polynomial', 
#                    cost = best_cost, gamma = best_gamma, degree = best_degree)
poly.svm.prob = attr(predict(poly.svm.res, X_test, probability = T), 'probabilities')
poly.svm.pred = prediction(poly.svm.prob[, 2], y_test)
poly.svm.roc = performance(poly.svm.pred, 'tpr', 'fpr')
poly.svm.auc = performance(poly.svm.pred, measure = 'auc')
auc.scores[row, 7] = poly.svm.auc@y.values[[1]]
accuracies[row, 7] = mean(predict(poly.svm.res, X_test)== y_test)
# plot(poly.svm.roc)

# 6.2. Radial basis
best_cost = 0.0
best_gamma = 0.0
best_accuracy = 0.0
for (cost in costs){
  for (gamma in gammas){
    rbf.svm.res = svm(X_train, y_train, kernel = 'radial', cost = cost, gamma = gamma, degree = degree)
    acc = mean(predict(rbf.svm.res, X_val) == y_val)
    if (acc > best_accuracy){
      best_degree = degree
      best_accuracy = acc
      best_cost = cost
      best_gamma = gamma
    }
  }
}

rbf.svm.res = svm(X_train_val, y_train_val, kernel = 'radial', 
                  cost = best_cost, gamma = best_gamma, degree = best_degree, probability = TRUE)
#rbf.svm.res = svm(X_train_val, y_train_val, kernel = 'radial', 
#                  cost = best_cost, gamma = best_gamma, degree = best_degree)
rbf.svm.prob = attr(predict(rbf.svm.res, X_test, probability = T), 'probabilities')
rbf.svm.pred = prediction(rbf.svm.prob[, 2], y_test)
rbf.svm.roc = performance(rbf.svm.pred, 'tpr', 'fpr')
rbf.svm.auc = performance(rbf.svm.pred, measure = 'auc')
auc.scores[row, 8] = rbf.svm.auc@y.values[[1]]
accuracies[row, 8] = mean(predict(rbf.svm.res, X_test) == y_test)
# plot(rbf.svm.roc)

# 7. Logistic Regression
library(nnet)
glm.fit = multinom(y_train_val ~ ., as.data.frame(X_train_val))
glm.fit = glm(y_train_val ~ ., as.data.frame(X_train_val), family = 'binomial')
glm.prob = predict(glm.fit, as.data.frame(X_test), type = 'response')
glm.pred = prediction(glm.prob, y_test)
glm.roc = performance(glm.pred, 'tpr', 'fpr')
glm.auc = performance(glm.pred, measure = 'auc')

# plot(glm.roc)

glm.prediction = rep(1, length(y_test))
glm.prediction[glm.prob > 0.5] = 2
accuracies[row, 9] = mean(glm.prediction == y_test)
auc.scores[row, 9] = glm.auc@y.values[[1]]

# multiclass
accuracies[row, 9] = mean(predict(glm.fit, X_test) == y_test)


# 8. CART
library(rpart)
cart.res = rpart(y_train_val ~ ., data = as.data.frame(X_train_val))
cart.prob = predict(cart.res, as.data.frame(X_test))
cart.pred = prediction(cart.prob[, 2], y_test)
cart.roc = performance(cart.pred, 'tpr', 'fpr')
cart.auc = performance(cart.pred, measure = 'auc')
auc.scores[row, 10] = cart.auc@y.values[[1]]
# plot(cart.roc)
accuracies[row, 10] = mean(predict(cart.res, as.data.frame(X_test), type = 'class') == y_test)

# 9. Random Forest
library(randomForest)

# number of features: 1 + log2(M), M: number of variables, according to Breiman
rf.res = randomForest(X_train_val, y_train_val, ntree = 100, mtry = as.integer(1 + log2(dim(X)[2])), importance = T)
rf.prob = predict(rf.res, X_test, type = 'prob')
rf.pred = prediction(rf.prob[, 2], y_test)
rf.roc = performance(rf.pred, 'tpr', 'fpr')
rf.auc = performance(rf.pred, measure = 'auc')
auc.scores[row, 11] = rf.auc@y.values[[1]]
accuracies[row, 11] = mean(predict(cart.res, as.data.frame(X_test), type = 'class') == y_test)

par("mar")
par(mar=c(1,1,1,1))
png(paste(name,'.roc.png'), width = 800, height = 600, units = 'px')
par(mfrow = c(2, 5))
plot(nb.roc, main = 'Bayesien Naif')
plot(knn.roc, main = 'KNN')
plot(lda.roc, main = 'LDA')
plot(qda.roc, main = 'QDA')
plot(lin.svm.roc, main = 'Linear SVM')
plot(poly.svm.roc, main = 'Polynomial SVM')
plot(rbf.svm.roc, main = 'Gaussian SVM')
plot(glm.roc, main = 'Logistic Regression')
plot(cart.roc, main = 'CART')
plot(rf.roc, main = 'Random Forest')
dev.off()

auc.scores
write.csv(auc.scores, file = 'AUC.csv')
accuracies
write.csv(accuracies, file = 'accuracies.csv')
