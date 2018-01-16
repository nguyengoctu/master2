#####################"exemple cours################""

####SAheart data
#A retrospective sample of males in a heart-disease high-risk region
#of the Western Cape, South Africa. There are roughly two controls per
#case of CHD. Many of the CHD positive men have undergone blood
#pressure reduction treatment and other programs to reduce their risk
#factors after their CHD event. In some cases the measurements were
#made after these treatments. These data are taken from a larger
#dataset, described in  Rousseauw et al, 1983, South African Medical Journal. 

#sbp		systolic blood pressure
#tobacco		cumulative tobacco (kg)
#ldl		low densiity lipoprotein cholesterol
#adiposity
#famhist		family history of heart disease (Present, Absent)
#typea		type-A behavior
#obesity
#alcohol		current alcohol consumption
#age		 age at onset
#chd		response, coronary heart disease


## charger les données

dataheart=read.table("C:/Users/pc/Documents/Cours/App-sup/Course/Course/TD4/TP4_RegLog_SVM/SAheart.data",sep=",",header=T,row.names=1)

## attacher la table 
attach(dataheart)


## statistiques descriptives

summary(dataheart)

## nuage de points en fonction des calsses

pairs(dataheart)

pairs(dataheart[,1:9], col=as.factor(chd))

## effectuer une régression logistique simple chd~age
heart.glm=glm(chd~age,family=binomial)


## afficher le résumé de la régression logistique
summary(heart.glm)

#Bilan sur l'échantillon d'apprentissage

table(heart.glm$fitted.values>0.5,chd)
chd


############################################################################
#Régression logistique 
############################################################################


# Données : The Stock Market Data

library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket)
cor(Smarket[,-9])
attach(Smarket)
plot(Volume)

# Régression logistique

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef
summary(glm.fit)$coef[,4]

glm.probs=predict(glm.fit,type="response")
glm.probs[1:10]
contrasts(Direction)
glm.pred=rep("Down",1250)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction)
(507+145)/1250
mean(glm.pred==Direction)


train=(Year<2005)
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)
Direction.2005=Direction[!train]
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fit,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005)


glm.fit=glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fit,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
106/(106+76)


predict(glm.fit,newdata=data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type="response")

# Linear Discriminant Analysis

library(MASS)
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class=lda.pred$class
table(lda.class,Direction.2005)
mean(lda.class==Direction.2005)
sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)
lda.pred$posterior[1:20,1]
lda.class[1:20]
sum(lda.pred$posterior[,1]>.9)

# Quadratic Discriminant Analysis

qda.fit=qda(Direction~Lag1+Lag2,data=Smarket,subset=train)
qda.fit
qda.class=predict(qda.fit,Smarket.2005)$class
table(qda.class,Direction.2005)
mean(qda.class==Direction.2005)


