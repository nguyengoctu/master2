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
# visa_premier 
# credit_card_fraud

data = flame
X = data[, -3]
y = as.factor(data[, 3])

summary(X)
summary(y)
boxplot(X)
hist(X$V1)
hist(X$V2)

cl = cor(X)
library(corrplot)
corrplot(cl, method = 'square')
plot(X, col = y)

 
# Splitting train-val-test (60-20-20)
set.seed(2018)
num_train_val = as.integer(0.80 * nrow(X))
tr_val = sample(1:nrow(X), num_train_val)
X_test = X[-tr_val, ]
y_test = y[-tr_val]
  
X_train_val = X[tr_val, ]
y_train_val = y[tr_val]

# Get the shift and scale from the training data
X_train_val_std = apply(X_train_val, 2, sd)
X_train_val_mean = colMeans(X_train_val)

# Normalize train data
X_train_val = scale(X_train_val)

# Normalize test data
X_test = scale(X_test, scale = X_train_val_std, center = X_train_val_mean)
  
num_train = as.integer(0.6 * nrow(X))
tr = sample(1:nrow(X_train_val), num_train)
  
X_train = X_train_val[tr, ]
y_train = y_train_val[tr]
  
X_val = X_train_val[-tr, ]
y_val = y_train_val[-tr]




# ROC curve
library(ROCR)

# Methodes
# 1. Bayesien Naif
library(e1071)
nb = naiveBayes(X_train_val, y_train_val)
nb.prob = predict(nb, X_test, type = 'raw')
nb.preds = prediction(nb.prob[, 2], y_test)
nb.roc = performance(nb.preds, 'tpr', 'fpr')
nb.auc = performance(nb.preds, measure = 'auc')
nb.auc@y.values
nb.test_accuracy = mean(predict(nb, X_test) == y_test)


# 2. KNN
library(class)

# Find the best K
kmax = 50
err_val = rep(NA, kmax)
for (k in 1:kmax){
  pred = knn(X_train, X_val, y_train, k)
  err_val[k] = sum(pred != y_val) / length(y_val)
}
plot(err_val, type = "l", col = 2, xlab = "nombre de voisins", 
     ylab = "taux d'erreur") 
K = which.min(err_val[-1]) 

knn.predictions = knn(X_train_val, X_test, y_train_val, K, prob = T)
knn.prob = attr(knn.predictions, 'prob')
knn.prob <- 2*ifelse(knn.predictions == 1, 1 - knn.prob, knn.prob) - 1
knn.pred = prediction(knn.prob, y_test)
knn.roc = performance(knn.pred, 'tpr', 'fpr')
knn.auc = performance(knn.pred, measure = 'auc')
knn.auc@y.values
plot(knn.roc)
# knn.test_accuracy = mean(knn(X_train_val, X_test, y_train_val, K) == y_test)


# 3. LDA
library(MASS)
lda.res = lda(X_train_val, y_train_val)
lda.prob = predict(lda.res, X_test)$posterior
lda.pred = prediction(lda.prob[, 2], y_test)
lda.roc = performance(lda.pred, 'tpr', 'fpr')
lda.auc = performance(lda.pred, measure = 'auc')
lda.auc@y.values
mean(predict(lda.res, X_test)$class == y_test)
plot(lda.roc)


# 4. QDA?
qda.res = qda(X_train_val, y_train_val)
qda.prob = predict(qda.res, X_test)$posterior
qda.pred = prediction(qda.prob[, 2], y_test)
qda.roc = performance(qda.pred, 'tpr', 'fpr')
qda.auc = performance(qda.pred, measure = 'auc')
qda.auc@y.values
mean(predict(qda.res, X_test)$class == y_test)
plot(qda.roc)


# SVM
costs = c(1e+6, 1e+3, 1, 1e-3)
degrees = c(2, 3, 5, 10)
gammas = c(1, 1e-3, 1e-5)

# 5. Linear SVM
best_cost = 0.0
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
lin.svm.prob = attr(predict(lin.svm.res, X_test, probability = T), 'probabilities')
lin.svm.pred = prediction(lin.svm.prob[, 1], y_test)
lin.svm.roc = performance(lin.svm.pred, 'tpr', 'fpr')
lin.svm.auc = performance(lin.svm.pred, measure = 'auc')
lin.svm.auc@y.values
mean(predict(lin.svm.res, X_test)== y_test)
plot(lin.svm.roc)

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
poly.svm.prob = attr(predict(poly.svm.res, X_test, probability = T), 'probabilities')
poly.svm.pred = prediction(poly.svm.prob[, 1], y_test)
poly.svm.roc = performance(poly.svm.pred, 'tpr', 'fpr')
poly.svm.auc = performance(poly.svm.pred, measure = 'auc')
poly.svm.auc@y.values
mean(predict(poly.svm.res, X_test)== y_test)
plot(poly.svm.roc)

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
rbf.svm.prob = attr(predict(rbf.svm.res, X_test, probability = T), 'probabilities')
rbf.svm.pred = prediction(rbf.svm.prob[, 1], y_test)
rbf.svm.roc = performance(rbf.svm.pred, 'tpr', 'fpr')
rbf.svm.auc = performance(rbf.svm.pred, measure = 'auc')
rbf.svm.auc@y.values
mean(predict(rbf.svm.res, X_test)== y_test)
plot(rbf.svm.roc)

# 7. Logistic Regression
glm.fit = glm(y_train_val ~ ., as.data.frame(X_train_val), family = 'binomial')
glm.prob = predict(glm.fit, as.data.frame(X_test), type = 'response')
glm.pred = prediction(glm.prob, y_test)
glm.roc = performance(glm.pred, 'tpr', 'fpr')
glm.auc = performance(glm.pred, measure = 'auc')
glm.auc@y.values
plot(glm.roc)

glm.prediction = rep(1, length(y_test))
glm.prediction[glm.prob > 0.5] = 2
mean(glm.prediction == y_test)


# 8. CART
library(rpart)
cart.res = rpart(y_train_val ~ ., data = as.data.frame(X_train_val))
cart.prob = predict(cart.res, as.data.frame(X_test))
cart.pred = prediction(cart.prob[, 2], y_test)
cart.roc = performance(cart.pred, 'tpr', 'fpr')
cart.auc = performance(cart.pred, measure = 'auc')
cart.auc@y.values
plot(cart.roc)
mean(predict(cart.res, as.data.frame(X_test), type = 'class') == y_test)

# 9. Random Forest
library(randomForest)
rf.res = randomForest(X_train_val, y_train_val, ntree = 100, mtry = 2, importance = T)
rf.prob = predict(rf.res, X_test, type = 'prob')
rf.pred = prediction(rf.prob[, 2], y_test)
rf.roc = performance(rf.pred, 'tpr', 'fpr')
rf.auc = performance(rf.pred, measure = 'auc')
rf.auc@y.values
mean(predict(cart.res, as.data.frame(X_test), type = 'class') == y_test)
plot(rf.roc)
