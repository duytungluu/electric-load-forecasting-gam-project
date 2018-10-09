#-------------------------------------------------------------------
#INITIALISATION
#-------------------------------------------------------------------

# Importer packages (mgcv implemente Models additifs généralisés)
rm(list=objects())
install.packages("mgcv")
install.packages("date")
install.packages("randomForest")
library(mgcv)
library(date)
library(randomForest)

# Importer données
chemin<-"C:/TP/Datamining/DONNEES US/"
Data0<-read.table("data_elec0.txt",sep='\t',header=TRUE,dec='.')
Data1<-read.table("data_elec1.txt",sep='\t',header=TRUE,dec='.')

# Formater date
Data0$date=as.POSIXct(Data0$date,format="%Y-%m-%d %H:%M:%S", tz = "UTC")
Data1$date=as.POSIXct(Data1$date,format="%Y-%m-%d %H:%M:%S", tz = "UTC")

names(Data0)
# [1] "Year" "Month" "Day" "Hour" "Time" "Toy" "dow" "daytype" "Zone1" "Zone2"
#[11] "Zone3" "Zone4" "Zone5" "Zone6" "Zone7" "Zone8" "Zone9" "Zone10"
#[21] "Zone13" "Zone14" "Zone15" "Zone16" "Zone17" "Zone18" "Zone19" "Zone20"
#[31] "Station3" "Station4" "Station5" "Station6" "Station7" "Station8" "Station9" "Station10" "Station11"
# Créer Hourf Yearf Monthf Dayf la forme qualitative des covariables Hour Year Month Day
Data0 = data.frame(Data0,Hourf= as.factor(Data0$Hour),Yearf= as.factor(Data0$Year),Monthf =
                     as.factor(Data0$Month),Dayf = as.factor(Data0$Day))

# Comme j'ai un problème de données test, je dois diviser les données apprentissage pour faire le projet
# On peut voir ce problème en executant ce code.
plot(c(Data0$Zone10,Data1$Zone10))
split = 30048
donnees=Data0[1:split,]
test = Data0[(split+1):length(Data0$date),]

#----------------------------------------------------------------------------
# STATISTIQUE EXPLORATOIRE
#----------------------------------------------------------------------------
attach(Data0)

# Tendance et saisonnalité
par(mfrow=c(2,1))

# Lissage
l = lowess(date,Zone10)
plot(date,Zone10,type="l",main="La consommation électrique avec sa courbe de lissage")
lines(l,col="red",lwd=3)
plot(l,type="l",main="Courbe de lissage")

# Saisonnalité de la consommation électrique comparant avec celle des stations de météo
fac = Monthf

#Moyenner par mois
x=tapply(Zone10,fac,mean)
s1 = tapply(Station1,fac,mean)
s3 = tapply(Station3,fac,mean)
s5 = tapply(Station5,fac,mean)
par(mfrow=c(2,1))
plot(x,type="l",main="La consommation électrique moyenne pour chaque mois")
plot(s1,type="l",ylim=c(30,90),main="La température moyenne pour chaque mois",col="green")
lines(s3,col="red")
lines(s5,col="blue")
legend("topleft",legend=c("Station1","Station3","Station5"),col=c("green","red","blue"),lty=1)

# ACF
acf(Zone10,lag.max=24*35,main="ACF")

# Saisonnalité hebdomadaire
fac = dow:Hourf

#Moyenner toutes les 1 heure de lundi, toutes les 2 heure de lundi…toutes les 00h de samedi…
x=tapply(Zone10,fac,mean)
nomJour = c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche")
col = rainbow(7) 

# Chercher les valeurs moyennes des heures à lundi
sel = grep(nomJour[1],names(x))
plot(x[sel],type="l",col=col[1],,xlab="Heure",ylab="Zone10",ylim=c(15000,40000))

# Idem pour les jours après
for(i in 2:7){
  sel = grep(nomJour[i],names(x))
  lines(x[sel],type="l",col=col[i])
}
legend("topleft",legend=nomJour,col=col,lty="solid",ncol=4)
detach(Data0)
       
#-----------------------------------------------------------
#Random Forest
#-----------------------------------------------------------
attach(donnees)

# Foret1 (sans terme autoregressif)
model <- randomForest(Zone10 ~ Station1 + Station2 + Station3 + Station4 + Station5 +Station6+Station7+Station8+Station9+Station10+Station11+Year+Monthf+Dayf+Hourf+Toy+dow+daytype+Time, data=donnees, na.action=na.omit,ntree=250)

#ACF de résidus
resid = Zone10-model$predicted
acf(resid,lag=32*24,main="foret1$resid, lag.max=24*32")

# Foret2 (avec terme autoregressif)
# Préparation des valeurs de la consommation électrique au passé
lagtot = 168
nr = length(Zone10)
for (k in c(1,2,3,4,5,24,48,72,96,120,144,168)) assign(paste("lag", k, sep = ""),Zone10[(lagtot-k+1):(nr-k)]);

#Ajouter dans data.frame
donneesr = data.frame(donnees[(lagtot+1):nr,],lag1,lag2,lag3,lag4,lag5,lag24,lag48,lag72,lag96,lag120,lag144,lag168)

# Foret2
model <- randomForest(Zone10 ~ Station1 + Station2 + Station3 + Station4 + Station5 +
                        Station6+Station7+Station8+Station9+Station10+Station11+Year+Monthf+Dayf+Hourf+Toy+dow+da
                      ytype+Time+lag1+lag2+lag3+lag4+lag5+lag24+lag48+lag72+lag96+lag120+lag144+lag168,
                      data=donneesr, importance=TRUE, na.action=na.omit,ntree=250)

#ACF de résidus
acf(Zone10-model$predicted,lag.max=24*32)

#------------------------------------------------------------
#Model GAM
#------------------------------------------------------------
#ETAPE 1
#Importance des covariables
# Option Importance = TRUE pour calculer l’importance
model <- randomForest(Zone10 ~ Station1 + Station2 + Station3 + Station4 + Station5 +
                        Station6+Station7+Station8+Station9+Station10+Station11+Year+Monthf+Dayf+Hourf+Toy+dow+da
                      ytype+Time, data=donnees, importance=TRUE, na.action=na.omit,ntree=150)

# Tracer graphe de l’importance, type=1 : l’importance par comparaison de la prédiction avant et après une permutation des valeurs out of bag, type=2 : l’importance de Gini
varImpPlot(model, type = 2, main = "Importance des variables pour la prévision de qualité de
           Zone10")

#Analyse l'effet temperature-mois
col = rainbow(12)
sel = which(Monthf==1)

#Moyenner et tracer la consommation des mois en fonction de température
sf = as.factor(Station1[sel])
m = tapply(Zone10[sel],sf,mean)
plot(as.numeric(levels(sf)),m,type="l",lwd=3,col=col[1],xlab="Température",ylab="Zone10",xlim=c(10,105),ylim=c(10000,60000))

# Idem pour les mois 2 à 12
for (i in 2:12){
  sel = which(Monthf==i)
  sf = as.factor(Station1[sel])
  m = tapply(Zone10[sel],sf,mean)
  lines(as.numeric(levels(sf)),m,col=col[i],lwd=3)
}
nomMois =
  c("janvier","février","mars","avril","mai","juin","juillet","aout","septembre","octobre","novembre","
    décembre")
legend("top",legend=nomMois,col=col,lty="solid",ncol=2,lwd=3)

# Construction g0
# Base thin-plat-spline avec l’interaction s(covariable,by=variable_interaction)
# Base thin-plat-spline simple s(covariable)
# Il existe l’option “bs” pour préviser le type du base spline (par défaut : thin plate spline), « k » pour limiter la dégre liberté maximale (par défaut : k=10).
g0<- gam(Zone10~s(Station1,by=Monthf),data=donnees)
summary(g0)

#Re-analyse l'effet temperature-mois (idem précédent, juste remplacer « Zone10 » par « g0$resid »)
col = rainbow(12)
sel = which(Monthf==1)
sf = as.factor(Station1[sel])
m = tapply(g0$resid[sel],sf,mean)
plot(as.numeric(levels(sf)),m,type="l",lwd=3,col=col[1],xlab="Température",ylab="g0$residuals",xlim
     =c(10,105),ylim=c(-9000,12000))
for (i in 2:12){
  sel = which(Monthf==i)
  sf = as.factor(Station1[sel])
  m = tapply(g0$resid[sel],sf,mean)
  lines(as.numeric(levels(sf)),m,col=col[i],lwd=3)
}
nomMois =
  c("janvier","février","mars","avril","mai","juin","juillet","aout","septembre","octobre","novembre","
    décembre")
legend("top",legend=nomMois,col=col,lty="solid",ncol=2,lwd=3)

#-----------------------------------------------------------
#ETAPE 2
#Importance des covariables
model <- randomForest(g0$residuals ~ Station1 + Station2 + Station3 + Station4 + Station5 +
                        Station6+Station7+Station8+Station9+Station10+Station11+Year+Monthf+Dayf+Hourf+Toy+dow+da
                      ytype+Time, data=donnees, importance=TRUE, na.action=na.omit, ntree=150)
varImpPlot(model, type = 2, main = "Importance des variables pour la prévision de qualité de\n
           Zone10~s(Station1,by=Monthf)$residuals")

#Analyse l'effet Hour-dow
#Moyenner toutes les 1 heure de lundi, toutes les 2 heure de lundi…toutes les 00h de samedi
fac = dow:Hourf
x=tapply(g0$resid,fac,mean)
nomJour = c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche")
col = rainbow(7)

#Chercher les valeurs moyennes des heures de lundi
sel = grep(nomJour[1],names(x))
plot(x[sel],type="l",col=col[1],lwd=3,xlab="Heure",ylab="g0$resid",ylim=c(10000,50000))

#Idem pour Mardi à dimanche
for(i in 2:7){
  sel = grep(nomJour[i],names(x))
  lines(x[sel],type="l",col=col[i],lwd=3)
}
legend("topleft",legend=nomJour,col=col,lty="solid",ncol=2,lwd=3)

#Construction g1
g1<- gam(Zone10~s(Hour,by=dow)+s(Station1,by=Monthf),data=donnees)
summary(g1)

#Re-Analyse l'effet Hour-dow, idem précédent juste remplacer g0$resid par g1$resid
fac = dow:Hourf
x=tapply(g1$residuals,fac,mean)
nomJour = c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche")
col = rainbow(7)
sel = grep(nomJour[1],names(x))
plot(x[sel],type="l",lwd=3,col=col[1],xlab="Heure",ylab="g1$residuals",ylim=c(-7000,7000))
for(i in 2:7){
  sel = grep(nomJour[i],names(x))
  lines(x[sel],type="l",col=col[i],lwd=3)
}
legend("topleft",legend=nomJour,col=col,lty="solid",ncol=2,lwd=3)

#---------------------------------------------------------
#ETAPE 3
#Importance des covariables
model <- randomForest(g1$residuals ~ Station1 + Station2 + Station3 + Station4 + Station5 +
                        Station6+Station7+Station8+Station9+Station10+Station11+Year+Monthf+Dayf+Hourf+Toy+dow+da
                      ytype+Time, data=donnees, importance=TRUE, na.action=na.omit, ntree=200)
varImpPlot(model, type = 2, main = "Importance des variables pour la prévision de qualité \n de
           Zone10~s(Station1,by=Monthf)+s(Hour,by=dow)$residuals")

#Construction g2
g2<- gam(Zone10~s(Hour,by=dow)+s(Station1,by=Monthf)+daytype,data=donnees)
summary(g2)

#Re-Analyse l'effet Hour-dow, idem précédent, juste remplacer g1$resid par g2$resid
fac = dow:Hourf
x=tapply(g2$residuals,fac,mean)
nomJour = c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche")
col = rainbow(7)
sel = grep(nomJour[1],names(x))
plot(x[sel],type="l",lwd=3,col=col[1],xlab="Heure",ylab="g2$residuals",ylim=c(-7000,7000))
for(i in 2:7){
  sel = grep(nomJour[i],names(x))
  lines(x[sel],type="l",col=col[i],lwd=3)
}
legend("topleft",legend=nomJour,col=col,lty="solid",ncol=2,lwd=3)

#---------------------------------------------------------
#ETAPE 4
#Importance des covariables
model <- randomForest(g2$residuals ~ Station1 + Station2 + Station3 + Station4 + Station5 +
                        Station6+Station7+Station8+Station9+Station10+Station11+Year+Monthf+Dayf+Hourf+Toy+dow+da
                      ytype+Time, data=donnees, importance=TRUE, na.action=na.omit, ntree=200)

varImpPlot(model, type = 2, main = "Importance des variables pour la prévision de qualité de\n
           Zone10~s(Station1,by=Monthf)+s(Hour,by=dow)+daytype$resid")

# Analyse la saison et la tendance
par(mfrow=c(2,1))

#Courbe de lissage
l = lowess(date,g2$resid)
plot(date,g2$resid,type="l",main="g2$resid avec sa courbe de lissage")
lines(date,l$y,col="red",lwd=1)
plot(date,l$y,type="l",main="Courbe de lissage")

# Construction g3
# bs=”cc” base cyclique, k=3 limité le degré liberté de la variable Time => limiter la dimension de
spline
g3<-
  gam(Zone10~s(Hour,by=dow)+s(Station1,by=Monthf)+daytype+s(Hour,by=Monthf)+s(Time,k=3)+s(Toy,bs="cc"),data=donnees)
summary(g3)

#---------------------------------------------------------
#ETAPE 5
# ACF
acf(g3$residuals,lag.max=24*10,main="acf de g3$resid, lag.max=24*10")

# Rajouter les termes autoregressifs
lagtot = 168
nr = length(Zone10)
for (k in c(1,2,3,4,5,24,48,72,96,120,144,168)) assign(paste("lag", k, sep = ""),Zone10[(lagtot-k+1):(nr-k)]);
donneesr =
  data.frame(donnees[(lagtot+1):nr,],lag1,lag2,lag3,lag4,lag5,lag24,lag48,lag72,lag96,lag120,lag144,lag168)

#Construction g4
g4<-
  gam(Zone10~s(Hour,by=dow)+s(Station1,by=Monthf)+daytype+s(Hour,by=Monthf)+Dayf+s(Time,k=3)+s(Toy,bs="cc")+s(lag1)+s(lag2)+s(lag3)+s(lag4)+s(lag5)+s(lag24)+s(lag48)+s(lag72)+s(lag96)+s(lag120)+s(lag144)+s(lag168),data=donneesr)
summary(g4)

#--------------------------------------------------------------------------------------
#Construction g4s3
g4s3<-
  gam(Zone10~s(Hour,by=dow)+s(Station3,by=Monthf)+daytype+s(Hour,by=Monthf)+Dayf+s(Time,k=3)+s(Toy,bs="cc")+s(lag1)+s(lag2)+s(lag3)+s(lag4)+s(lag5)+s(lag24)+s(lag48)+s(lag72)+s(lag96)+s(lag120)+s(lag144)+s(lag168),data=donneesr)
summary(g4s3)

#Construction g4s5
g4s5<-
  gam(Zone10~s(Hour,by=dow)+s(Station5,by=Monthf)+daytype+s(Hour,by=Monthf)+Dayf+s(Time,k=3)+s(Toy,bs="cc")+s(lag1)+s(lag2)+s(lag3)+s(lag4)+s(lag5)+s(lag24)+s(lag48)+s(lag72)+s(lag96)+s(lag120)+s(lag144)+s(lag168),data=donneesr)
summary(g4s5)

#--------------------------------------------------------------------------------------
#Interpretation graphique
detach(donnees)
attach(donneesr)

# se.fit=TRUE pour calculer l’estimateur de l’ecart type
p4 = predict(g4,se.fit=TRUE)
p4s3 = predict(g4s3,se.fit=TRUE)
p4s5 = predict(g4s5,se.fit=TRUE)

# Calculer l’interval de confiance à 95%
upr4 = p4$fit + 1.96*p4$se.fit
lwr4 = p4$fit - 1.96*p4$se.fit
upr43 = p4s3$fit + 1.96*p4s3$se.fit
lwr43 = p4s3$fit - 1.96*p4s3$se.fit
upr45 = p4s5$fit + 1.96*p4s5$se.fit
lwr45 = p4s5$fit - 1.96*p4s5$se.fit

# Superposer les donnees + valeurs ajustées
col=rainbow(4)
plot(date,Zone10,type="l",xlab="Date",ylab="Consommation électrique",main="Consommation
     électrique et les valeurs ajustées")
lines(date,p4$fit,col=col[1])
lines(date,p4s3$fit,col=col[2])
lines(date,p4s5$fit,col=col[3])
lines(date,model$pred,col=col[4])
legend("topright",legend=c("Valeur apprentissage","Valeur estimée g4","Valeur estimée
                           g4s3","Valeur estimée g4s5","Valeur estimée fa"),lty=1,col=c(1,col))

# Zoomer + interval de confiance à 95%
plot(date[1:300],Zone10[1:300],type="l",xlab="Date",ylab="Consommation
     électrique",main="Consommation électrique, les valeurs ajustées des modèles et les\nintervalles de
     confiance associés(courbes pointillés en même couleur)")
lines(date[1:300],p4$fit[1:300],col=col[1])
lines(date[1:300],upr4[1:300],col=col[1],lty=2)
lines(date[1:300],lwr4[1:300],col=col[1],lty=2)
lines(date[1:300],p4s3$fit[1:300],col=col[2])
lines(date[1:300],upr43[1:300],col=col[2],lty=2)
lines(date[1:300],lwr43[1:300],col=col[2],lty=2)
lines(date[1:300],p4s5$fit[1:300],col=col[3])
lines(date[1:300],upr45[1:300],col=col[3],lty=2)
lines(date[1:300],lwr45[1:300],col=col[3],lty=2)
lines(date[1:300],model$pred[1:300],col=col[4])
legend("topright",legend=c("Valeur apprentissage","Valeur estimée g4","Valeur estimée g4s3","Valeur estimée g4s5","Valeur estimée fa"),lty=1,col=c(1,col))

#---------------------------------------------------------
#PREDICTION
#---------------------------------------------------------
detach(donneesr)
attach(test)
source("prediction.R")
load("C:/TP/Datamining/DONNEES US/g4")
p = predict(g4,se.fit=TRUE)

# Prédiction jour par jour
predjg4 = prediction(donnees,test,g4,24)
predjg4s3 = prediction(donnees,test,g4s3,24)
predjg4s5 = prediction(donnees,test,g4s5,24)
predjfa = prediction(donnees,test,model,24)

# Prédiction heure par heure
predhg4 = prediction(donnees,test,g4,1)
predhg4s3 = prediction(donnees,test,g4s3,1)
predhg4s5 = prediction(donnees,test,g4s5,1)
predhfa = prediction(donnees,test,model,1)

# Prédiction semaine par semaine
predsg4 = prediction(donnees,test,g4,24*7)
predsg4s3 = prediction(donnees,test,g4s3,24*7)
predsg4s5 = prediction(donnees,test,g4s5,24*7)
predsfa = prediction(donnees,test,model,24*7)

# Interpretation graphique de prédiction à très court terme (l'heure par l'heure)
col=rainbow(4)
plot(date,Zone10,type="l",xlab="Date",ylab="Consommation électrique",main="Prévision d'heure
     par heure")
lines(date,predhg4,col=col[1])
lines(date,predhg4s3,col=col[2])
lines(date,predhg4s5,col=col[3])
lines(date,predhfa,col=col[4])
legend("topright",legend=c("Valeur test","Valeur prédite g4","Valeur prédite g4s3","Valeur prédite
                           g4s5","Valeur prédite fa"),lty=1,col=c(1,col))

# Zoom
plot(date[100:500],Zone10[100:500],type="l",xlab="Date",ylab="Consommation
     électrique",main="Un zoom de la prévision d'heure par heure")
lines(date[100:500],predhg4[100:500],col=col[1])
lines(date[100:500],predhg4s3[100:500],col=col[2])
lines(date[100:500],predhg4s5[100:500],col=col[3])
lines(date[100:500],predhfa[100:500],col=col[4])
legend("topright",legend=c("Valeur test","Valeur prédite g4","Valeur prédite g4s3","Valeur prédite
                           g4s5","Valeur prédite fa"),lty=1,col=c(1,col),ncol=2)

# Interpretation graphique de prédiction à courte terme (jour par jour)
fac = as.factor(Year):as.factor(test$Month):Dayf 

# si on utilise Monthf ici, on a les niveaux de 1..12 au lieu de 8...12, idem Yearf niveaux de 2004 2007 au lieu de 2007
# Changer le facteur au type date
datej = as.POSIXct(levels(fac),format="%Y:%m:%d")
datej = datej[!is.na(datej)] # Car il donne NA pour 31/11, 31/9 (il n’existe pas ces jours)
# Moyenner la consommation d’électrique pendant une journée, supprimer les NA pour 31/11 et 31/9
Zone10jm = tapply(Zone10,fac,mean)
Zone10jm = Zone10jm[!is.na(Zone10jm)]
predjg4m = tapply(predjg4,fac,mean)
predjg4m = predjg4m[!is.na(predjg4m)]
predjg4s3m = tapply(predjg4s3,fac,mean)
predjg4s3m = predjg4s3m[!is.na(predjg4s3m)]
predjg4s5m = tapply(predjg4s5,fac,mean)
predjg4s5m = predjg4s3m[!is.na(predjg4s5m)]
predjfam = tapply(predjfa,fac,mean)
predjfam = predjfam[!is.na(predjfam)]

# Présentation graphique
col =rainbow(4)
plot(datej,Zone10jm,type="l",xlab="Date",ylab="Consommation électrique",main="Prévision à court
     terme")
lines(datej,predjg4m,col=col[1],type="l")
lines(datej,predjg4s3m,col=col[2],type="l")
lines(datej,predjg4s5m,col=col[3],type="l")
lines(datej,predjfam,col=col[4],type="l")
legend("topright",legend=c("Valeur test","Valeur prédite g4","Valeur prédite g4s3","Valeur prédite
                           g4s5","Valeur prédite fa"),lty=1,col=c(1,col),ncol=2)

# Interpretation graphique de prédiction à moyen terme (semaine par semaine)
facsem = numeric(length(Zone10))
j=1;

#Creer un vecteur facsem de facteur c(1,1 ,1,1,1,1,1,2,2,2,2,2,2,2…) pour calculer la moyenne par
semaine
for (i in (1:length(Zone10))){ facsem[i] = j; if (i%%(24*7)==0) j = j+1;}
facsem = as.factor(facsem)

#Moyenner la consommation électrique pendant une semaine
Zone10sm = tapply(Zone10,facsem,mean)
predsg4m = tapply(predsg4,facsem,mean)
predsg4s3m = tapply(predsg4s3,facsem,mean)
predsg4s5m = tapply(predsg4s5,facsem,mean)
predsfam = tapply(predsfa,facsem,mean)
col=rainbow(4)

#Présentation graphique
plot(Zone10sm,type="l",main="Prévision à moyen terme",xlab="Indice de la
     semaine",ylab="Consommation électrique")
lines(predsg4m,type="l",col=col[1])
lines(predsg4s3m,type="l",col=col[2])
lines(predsg4s5m,type="l",col=col[3])
lines(predsfam,type="l",col=col[4])
legend("topright",legend=c("Valeur test","Valeur prédite g4","Valeur prédite g4s3","Valeur prédite
g4s5","Valeur prédite fa"),lty=1,col=c(1,col),ncol=2)

#-------------------------------------------------------------------------------
# CRITERE D'EVALUATION MAPE et RMSE
#-------------------------------------------------------------------------------
# Definition
mape <- function(y, yhat) 100*mean(abs((y - yhat)/y))
rmse <- function(y, yhat) sqrt(mean((y - yhat)^2))

# HEURE
mape(Zone10,predhg4); rmse(Zone10,predhg4)
mape(Zone10,predhg4s3); rmse(Zone10,predhg4s3)
mape(Zone10,predhg4s5); rmse(Zone10,predhg4s5)
mape(Zone10,predhfa); rmse(Zone10,predhfa)
# JOUR
mape(Zone10,predjg4); rmse(Zone10,predjg4)
mape(Zone10,predjg4s3); rmse(Zone10,predjg4s3)
mape(Zone10,predjg4s5); rmse(Zone10,predjg4s5)
mape(Zone10,predjfa); rmse(Zone10,predjfa)
#SEMAINE
mape(Zone10,predsg4); rmse(Zone10,predsg4)
mape(Zone10,predsg4s3); rmse(Zone10,predsg4s3)
mape(Zone10,predsg4s5); rmse(Zone10,predsg4s5)
mape(Zone10,predsfa); rmse(Zone10,predsfa)