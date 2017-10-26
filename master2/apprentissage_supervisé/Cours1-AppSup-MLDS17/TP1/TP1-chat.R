library(MASS)
cats
names(cats)
attach(cats)
#statistique descriptive
#univari?e
summary(cats)
sd(cats$Bwt)
sd(cats$Hwt)

par(mfrow=c(2,2))
boxplot(cats$Bwt,main="Poids du corps")
hist(cats$Bwt,main="Poids du corps")
boxplot(cats$Hwt,main="Poids du coeur")
hist(cats$Hwt,main="Poids du coeur")

#bivariee
par(mfrow=c(1,1))
plot(cats$Bwt,cats$Hwt,xlab="Poids du Corps",
ylab="Poids du coeur", 
main="Poids du coeur des chats en fonction du poids du corps")
cor(cats$Bwt,cats$Hwt)

#regression de Hwt en Bwt
modele2=lm(Hwt~Bwt)
#Resultats
#Estimation et Test de la nullite des parametres d'esperance du modele
summary(modele2)

#intervalle de confiance des param?tres d'esp?rance
confint(modele2)

#analyse de la variance de la r?gression et test de Fisher
anova(modele2)

#ou bien
#Modele sans x :Yi = beta + epsi
modele1=lm(Hwt~1)
summary(modele1)
#comparaison des 2 modeles
anova(modele1,modele2)

#predictions avec la droite des moindres carres hat(yi)=axi+b
fitted(modele2)

#qualit? de l'ajustement
#coeef de d?termination
summary(modele2)
R2=summary(modele2)[[8]]
#comparaison des pr?visions aux y observ?s
plot(Hwt,modele2$fitted)
abline(0,1)

#pr?vision du poids du coeur d'un chat dont le poids du corps est de 2.5 
newdata=data.frame(Bwt=2.5)
predict(modele2, newdata,interval="confidence") #intervalle de confiance deE(Y0)
predict(modele2, newdata,interval="prediction") #intervalle de pr?dictionde Y0

#trace du nuage de points, de la MDC et des int de confiance de la droite et de prediction
plot(Bwt,Hwt,xlab="Poids du Corps",
ylab="Poids du coeur", 
main="Poids du coeur des chats en fonction du poids du corps")
abline(modele2,col=1)#trace de la DMC
new <- data.frame(Bwt = seq(min(Bwt),max(Bwt),length=100))#calcul des predictions pour 100 valeurs de x comprises entre min(xi) et max(xi) 
pc=predict(modele2,new,interval="confidence") 
pp=predict(modele2, new,interval="prediction")
matlines(new,pc[,2:3], lty=c(2,2), col="blue")
matlines(new,pp[,2:3], lty=c(3,3), col="red")
legend("topleft",c("DMC","Intervalle de confiance de E(Y)",
"Intervalle de pari de Y"),lty=c(1,2,3), col=c(1,"blue","red"))
#tout tracer d'un coup : intervalle de prediction et intervalle de confiance
matlines(new,cbind(pc,pp[,-1]),lty=c(1,2,2,3,3),
col=c("red","blue","blue",1,1))

#validation du mod?le M2
residus=resid(modele2)#residus
restd=rstandard(modele2)#residus standardises

hist(residus,main="r?sidus")

plot(fitted(modele2),residus)
abline(0,0)
abline(qnorm(0.975,0,1)*sd(residus),0)
abline(-qnorm(0.975,0,1)*sd(residus),0)

#si on prend en compte le fait que les residus n'ont pas tous la meme variance
#qqplot gaussien des residus standardises
par(mfrow=c(2,1))
hist(restd,main="r?sidus standardis?s")
qqnorm(restd)
qqline(restd)

n=length(Bwt)
#graphe des residus standardises
plot(fitted(modele2),restd)
abline(0,0)
abline(qt(0.975,n-2)*sd(restd),0)# 
abline(-qt(0.975,n-2)*sd(restd),0)

</div></pre>