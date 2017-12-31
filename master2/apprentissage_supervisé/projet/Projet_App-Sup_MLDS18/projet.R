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
corrplot(cl, method = 'square')
plot(X, col = y)


# Methodes
# 1. Bayesien Naif
# 2. KNN
# 3. LDA
# 4. QDA?
# 5. Linear SVM
# 6. Non-linear SVM
# 7. Logistic Regression
# 8. CART
# 9. Random Forest