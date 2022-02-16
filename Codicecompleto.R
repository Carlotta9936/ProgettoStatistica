#Librerie
library(e1071)
library(readr)
library(corrplot)
library(ROCR)
library(ggplot2)
library(tidyverse)
library(knitr)
library(data.table)

#Caricamento del dataset
ds<-read.csv("data.csv")

#Pulizia del dataset
ds$X<-NULL
ds<-na.omit(ds)
levels(ds$diagnosis)

#Salviamo il backup
dsBU<-ds

#Grafico 1: Matrice di correlazione
mydata<-dsBU
mydata$id<-NULL
numero<-as.numeric(mydata$diagnosis)
mydata$diagnosis<-numero
cor.matrix<- cor( mydata)
corrplot( cor.matrix,method= c("circle" ),tl.cex=0.6,number.font=6,tl.col="black")

#Grafico 2: Pazienti per range di ragggio
raggruppamentoradius<-c("6.98-10","10.01-13.03","13.04-16.06","16.07-19.09","19.1-22.12","22.13-25.15","25.16-29")
rrad<-ds$radius_mean
rrad<-cut(ds$radius_mean,breaks = c(6.98,10,13.03,16.06,19.09,22.12,25.15,29),labels = raggruppamentoradius)
ds$appoggio<-rrad

ggplot(data=NULL, aes(x=ds$appoggio, y=1)) + 
  geom_bar(stat = "identity",color="pink",fill="pink")+
  xlab("range")+
  ylab("numero pazienti")+
  labs(
    title="distribuzione delle dimensioni del raggio")

#Grafico 3: Barplot benigni maligni per range di raggio
conta<-c((table(ds$diagnosis[ds$appoggio=="6.98-10"])),(table(ds$diagnosis[ds$appoggio=="10.01-13.03"])),(table(ds$diagnosis[ds$appoggio=="13.04-16.06"])),(table(ds$diagnosis[ds$appoggio=="16.07-19.09"])),(table(ds$diagnosis[ds$appoggio=="19.1-22.12"])),(table(ds$diagnosis[ds$appoggio=="22.13-25.15"])),(table(ds$diagnosis[ds$appoggio=="25.16-29"])))
data2 <- matrix(conta, nrow=2)
colnames(data2)<- c(raggruppamentoradius)
rownames(data2)<- c("benigno","maligno")
media<-mean(ds$radius_mean)
mediana<-median(ds$radius_mean)

barplot(data2,
        border="white",
        axis.lty = 1,
        col=colors()[c(49,554)] ,
        font.axis=6, 
        cex.axis = 0.85,
        cex.names = 0.85,
        las=2,
        beside=T, 
        main = "maligni e benigni per range di raggio",
        ylab="numero di pazienti",
        font.lab=1)
legenda2<-legend(15,140,cex=0.7,fill=colors()[c(49,554)] ,legend=rownames(data2),title="legenda")

#Grafico 4:  Correlazione raggio e punti concavi
basic <- ggplot(ds, aes(ds$radius_mean, ds$concave.points_mean,colour=ds$diagnosis, 
                        shape = factor(ds$diagnosis))) +
  
  labs(title="Analisi punti concavi e raggio")+
  xlab("raggio")+
  ylab("punti concavi")+
  geom_point(size=2)+
  scale_color_brewer(palette="Set2")     
basic+
  labs(
    colour="legenda",
    shape="legenda")

#Grafico 5: Boxplot simmetrie
box2<-boxplot(ds$symmetry_mean[ds$diagnosis=="M"],ds$symmetry_mean[ds$diagnosis=="B"],
              names=c("maligno","benigno"), main ="box plot delle simmetrie",ylab="simmetria",
              las=2,col=c("red","green"))


#Grafico 6: Correlazione compattezza e simmetria
basic2 <- ggplot(ds, aes(ds$compactness_mean, ds$symmetry_mean,colour=ds$diagnosis,
                 shape = factor(ds$diagnosis))) +
labs(title="Analisi compattezza e simmetria")+
xlab("compattezza")+
ylab("simmetria")+
geom_point(size=2)+
scale_color_brewer(palette="Set2")     
basic2+
labs(colour="legenda",
      shape="legenda")
                  
#Grafico 7: Box plot Fractal dimension
box1<-boxplot(ds$fractal_dimension_mean[ds$diagnosis=="M"],ds$fractal_dimension_mean[ds$diagnosis=="B"],
              names=c("maligno","benigno"),main ="box plot fractal dimension",ylab="fractal_dimension",
              las=2,col=c("red","green"))


### Machine learning
#Splitting 
N<-nrow(ds)

N.train<- 399
N.val<-85
N.test<-85

train.sample<-sample(N, N.train)
ds.train<-ds[train.sample, ]
ds.test<-ds[-train.sample, ]

val.sample<-sample(N.test+N.val, N.val)
ds.val<-ds.test[val.sample, ]
ds.test<-ds.test[-val.sample, ]

# Funzioni che useremo per valutare il modello
#Errore
MR<- function(y.pred, y.true){
  res<-mean(y.pred != y.true)
  return(res)
}

#Accuratezza 
Acc<-function(y.pred, y.true){
  res<-1-mean(y.pred!=y.true)
  return(res)
}

## Addestramento del modello
# Lineare
MR.lin.total.Train<-1:10
for(c in 1:10){
  model.SVC<-svm(diagnosis~ ., ds, kernel="linear", cost=c)
  y.pred<-predict(model.SVC, ds)
  MR.lin<-MR(y.pred, ds$diagnosis)
  MR.lin.total.Train[c]<-MR.lin
}
MR.lin.total.Train

# Polinomiale
MR.poly.total.Train<-matrix(ncol=10, nrow=10)
Error.poly.train<-c(0)
for(c in 1:10){
  for(d in 1:10){
    model.SVM<-svm(diagnosis~., ds, kernel="polynomial", cost=c, degree=d)
    y.pred<-predict(model.SVM, ds)
    MR.poly<-MR(y.pred, ds$diagnosis)
    MR.poly.total.Train[c,d]<-MR.poly
    Error.poly.train<-append(Error.poly.train, MR.poly)
    
  }
}
Error.poly.train<-Error.poly.train[-1]
MR.poly.total.Train

# Radiale
MR.rad.total.Train<-matrix(ncol=10, nrow=10)
Error.rad.train<-c(0)
for(c in 1:10){
  for(g in 1:10){
    model.SVM<-svm(diagnosis ~ ., ds, kernel="radial", cost=c, gamma=g/100)
    y.pred<-predict(model.SVM, ds)
    MR.rad<-MR(y.pred, ds$diagnosis)
    MR.rad.total.Train[c,g]<-MR.rad
    Error.rad.train<-append(Error.rad.train, MR.rad)
  }
}
Error.rad.train<-Error.rad.train[-1]
MR.rad.total.Train

## Hyperparameter Tuning
# Lineare
MR.lin.total.Val<-1:10
for(c in 1:10){
  model.SVC<-svm(diagnosis ~ ., ds, kernel="linear", cost=c)
  y.pred<-predict(model.SVC, ds.val)
  MR.lin<-MR(y.pred, ds.val$diagnosis)
  MR.lin.total.Val[c]<-MR.lin
}
MR.lin.total.Val

# Polinomiale
MR.poly.total.Val<-matrix(ncol=10, nrow=10)
Error.poly.val<-c(0)
for(c in 1:10){
  for(d in 1:10){
    model.SVM<-svm(diagnosis~., ds, kernel="polynomial", cost=c, degree=d)
    y.pred<-predict(model.SVM, ds.val)
    MR.poly<-MR(y.pred, ds.val$diagnosis)
    MR.poly.total.Val[c,d]<-MR.poly
    Error.poly.val<-append(Error.poly.val, MR.poly)
  }
}
Error.poly.val<-Error.poly.val[-1]
MR.poly.total.Val

# Radiale
MR.rad.total.Val<-matrix(ncol=10, nrow=10)
Error.rad.val<-c(0)
for(c in 1:10){
  for(g in 1:10){
    model.SVM<-svm(diagnosis~., ds, kernel="radial", cost=c, gamma=g/100)
    y.pred<-predict(model.SVM, ds.val)
    MR.rad<-MR(y.pred, ds.val$diagnosis)   
    MR.rad.total.Val[c,g]<-MR.rad
    Error.rad.val<-append(Error.rad.val, MR.rad)
  }
}
Error.rad.val<-Error.rad.val[-1]
MR.rad.total.Val

## Spaghetti Plot
# Lineare
#Creo i dataframe per i plot
df_Spaghetti_Linear_Train<-data.frame(
  cost=c(1:10),
  error=MR.lin.total.Train,
  id="Training")

df_Spaghetti_Linear_Val<-data.frame(
  cost=c(1:10),
  error=MR.lin.total.Val,
  id="Validation")

df_Spaghetti_Linear<-rbind(df_Spaghetti_Linear_Train, df_Spaghetti_Linear_Val)

#Plot
ggplot(df_Spaghetti_Linear, aes(x=cost, y=error, color=id))+ 
  geom_line()+geom_point()+theme_bw()+scale_y_continuous(limits = c(0,0.25))


#Polinomiale
#Creo i dataframe per i plot
df_Spaghetti_Polinomial_Train<-data.frame(cost=ceiling(c(1:100)/10),
                                          degree=c(1:10),
                                          error=Error.poly.train,
                                          id="Training")

df_Spaghetti_Polinomial_Val<-data.frame(cost=ceiling(c(1:100)/10),
                                        degree=c(1:10),
                                        error=Error.poly.val,
                                        id="Validation")

df_Spaghetti_Polinomial<-rbind(df_Spaghetti_Polinomial_Train, df_Spaghetti_Polinomial_Val)

#Plot
ggplot(df_Spaghetti_Polinomial, aes(x=cost, y=error, color=factor(degree), fill=interaction(id, degree)), xlim=degree) +
  geom_line()+
  geom_point()+
  labs(title = "Spaghetti plot polinomiale", x = "Costo", y = "Errore", color = "Degree")+
  guides(fill="none")+
  theme_bw()+
  scale_y_continuous(limits = c(0,0.25))

#Plot diviso
ggplot(df_Spaghetti_Polinomial, aes(x=cost, y=error, color=factor(id), fill=interaction(id, degree))) + 
  geom_line() + 
  geom_point() + 
  labs(title = "Spaghetti plot polinomiale diviso per Degree", color = "Degree")+
  theme_bw() + 
  guides(fill="none")+
  scale_y_continuous(limits = c(0,0.25)) +
  facet_wrap(~degree)


#Radiale
#Creo i dataframe per i plot
df_Spaghetti_Radial_Train<-data.frame(cost=ceiling(c(1:100)/10),
                                      gamma=c(1:10),
                                      error=Error.rad.train,
                                      id="Training")

df_Spaghetti_Radial_Val<-data.frame(cost=ceiling(c(1:100)/10),
                                    gamma=c(1:10),
                                    error=Error.rad.val,
                                    id="Validation")

#Plot
df_Spaghetti_Radial<-rbind(df_Spaghetti_Radial_Train, df_Spaghetti_Radial_Val)
ggplot(df_Spaghetti_Radial, aes(x=cost, y=error, color=factor(gamma), fill=interaction(id, gamma))) + 
  geom_line() + 
  geom_point() + 
  labs(title = "Spaghetti plot radiale", x = "Costo", y = "Errore", color = "Gamma")+
  guides(fill="none")+
  theme_bw() + 
  scale_y_continuous(limits = c(0,0.05))

#Plot diviso
ggplot(df_Spaghetti_Radial, aes(x=cost, y=error, color=factor(id), fill=interaction(id, gamma))) + 
  geom_line() + 
  geom_point() + 
  labs(title = "Spaghetti plot radiale diviso per gamma", color = "Gamma")+
  theme_bw() + 
  guides(fill="none")+
  scale_y_continuous(limits = c(0,0.05)) +
  facet_wrap(~gamma)

### Valutazione performance
## Ricerca del minimo
#Lineare
modello<-'lineare'
modello
minimo.Lin<-min(MR.lin.total.Val)
minimo.Lin#Trova il valore minimo nel array degli errori del lineare

#Polinomiale
modello<-'polinomiale'
modello
minimo.Poly<-min(MR.poly.total.Val) #Trova il valore minimo nella matrice degli errori del polinomiale
minimo.Poly

#Radiale
modello<-'radiale'
modello
minimo.Rad<-min(MR.rad.total.Val) #Trova il valore minimo nella matrice degli errori del radiale
minimo.Rad

#Confronto tutti i valori minimi per trovare IL minimo in assoluto
if(minimo.Lin<minimo.Poly){
  if(minimo.Lin<minimo.Rad){
    errore_minimo<-minimo.Lin
    pos_errore_minimo<-which(MR.lin.total.Val==min(MR.lin.total.Val), arr.ind = TRUE)
    x<-pos_errore_minimo[1]
    
    #applichiamo il machine learning con i parametri migliori
    model<-svm(diagnosis ~ ., ds, kernel="linear", cost=x)
    y.pred<-predict(model.SVC, ds.test)
    Errore<-MR(y.pred, ds.test$diagnosis)
    Accuratezza<-Acc(y.pred, ds.test$diagnosis)
    modello<-"linear"
  } else {
    errore_minimo<-minimo.Rad
    pos_errore_minimo<-which(MR.rad.total.Val==min(MR.rad.total.Val), arr.ind = TRUE)
    y<-pos_errore_minimo[1,1]
    x<-pos_errore_minimo[1,2] 
    
    #applichiamo il machine learning con i parametri migliori
    model.SVM<-svm(diagnosis~., ds, kernel="radial", cost=x, gamma=y/100)
    y.pred<-predict(model.SVM, ds.test)
    Errore<-MR(y.pred, ds.test$diagnosis)
    Accuratezza<-Acc(y.pred, ds.test$diagnosis)
    modello<-"radial"
  }
} else {
  if(minimo.Poly<minimo.Rad){
    errore_minimo<-minimo.Poly
    pos_errore_minimo<-which(MR.poly.total.Val==min(MR.poly.total.Val), arr.ind = TRUE)
    y<-pos_errore_minimo[1,1]
    x<-pos_errore_minimo[1,2]
    
    #applichiamo il machine learning con i parametri migliori
    model.SVM<-svm(diagnosis~., ds, kernel="polynomial", cost=x, degree=y)
    y.pred<-predict(model.SVM, ds.test)
    Errore<-MR(y.pred, ds.test$diagnosis)
    Accuratezza<-Acc(y.pred, ds.test$diagnosis)
    modello<-"polynomial"
    
  } else {
    errore_minimo<-minimo.Rad
    pos_errore_minimo<-which(MR.rad.total.Val==min(MR.rad.total.Val), arr.ind = TRUE)
    x<-pos_errore_minimo[1,1]
    y<-pos_errore_minimo[1,2]
    
    #applichiamo il machine learning con i parametri migliori
    model.SVM<-svm(diagnosis~., ds, kernel="radial", cost=x, gamma=y/100)
    y.pred<-predict(model.SVM, ds.test)
    Errore<-MR(y.pred, ds.test$diagnosis)
    Accuratezza<-Acc(y.pred, ds.test$diagnosis)
    modello<-"radial"
  }
}

errore_minimo #Stampo l'errore minimo
x             #riga del minimo
y             #colonna del minimo, eventuale perch? potrebbe essere lineare
Errore
Accuratezza
modello

### Interpretazione probabilistica
SVM.probs <- svm(diagnosis ~ ., data=ds, kernel="radial", cost=x, degree=y, probability=TRUE)
y.pred <- predict(SVM.probs, ds.test, probability=TRUE)
"probabilit? di diagnosi"
y.pred <- attr(y.pred, "probabilities")
y.vec <- y.pred[, 2] 
y.vec

"predizione del modello"
y.final <- rep(0, 85)
y.final[y.vec > 0.5] <- 1
y.final

table(y.final, ds.test$diagnosis)

y.final <- rep(0, 85)
y.final[y.vec > 0.2] <- 1
y.final

table(y.final, ds.test$diagnosis)

#Curva di ROC
pred <- prediction(y.vec, ds.test$diagnosis)
perf <- performance(pred, "tpr", "fpr")

#AUC
auc <- performance(pred, "auc")
auc <- auc@y.values[[1]]

plot(perf, main=auc, colorize=TRUE)

### Studio statistico dei risultati

## SRS(k)
dstat<-dsBU #Carico il dataset 
Nstat<-nrow(dstat)

Nstat.train<- 399
Nstat.val<-85
Nstat.test<-85

MR.radStatVector <- 1:50 #vettore che conterr? i 50 valori di MR (kernel radiale)
Acc.radStatVector <- 1:50

for (c in 1:50){
  train.sample<-sample(Nstat, Nstat.train)
  dstat.train<-ds[train.sample, ]
  dstat.test<-ds[-train.sample, ]
  
  val.sample<-sample(Nstat.test+Nstat.val, Nstat.val)
  dstat.val<-ds.test[val.sample, ]
  dstat.test<-dstat.test[-val.sample, ]
  
  model.SVM<-svm(diagnosis~., dstat.train, kernel="radial", cost=x, gamma=y/100)
  y.pred<-predict(model.SVM, dstat.train)
  
  y.pred<-predict(model.SVM, dstat.test)
  MR.radStat<-MR(y.pred, dstat.test$diagnosis)
  Acc.radStat <- Acc(y.pred, dstat.test$diagnosis)
  
  MR.radStatVector[c] <-  MR.radStat
  Acc.radStatVector[c] <- Acc.radStat
}

# Vettori con 50 valori di MR e Acc ottenuti valutando modelli con kernel radiale
'Errore'
MR.radStatVector
'Accuratezza'
Acc.radStatVector

#### Statistica descrittiva

## Calcolo del centro
# Media
MR.radMean <- mean(MR.radStatVector)
MR.radMean

Acc.radMean <- mean(Acc.radStatVector)
Acc.radMean

# Mediana
MR.radMedian <- median(MR.radStatVector)
MR.radMedian

Acc.radMedian <- median(Acc.radStatVector)
Acc.radMedian

#Quantile semplice
MR.radQuant <- quantile(MR.radStatVector)
MR.radQuant

Acc.radQuant <- quantile(Acc.radStatVector)
Acc.radQuant

#Plot quantili
sum.MR <- summary(MR.radStatVector)
sum.Acc <- summary(Acc.radStatVector)

boxplot(sum.MR, col="red",main="Box plot errore")
boxplot(sum.Acc, col="blue",main="Box plot accuratezza")

## Diffusione dei dati
# Varianza campionaria
MR.varCamp <- var(MR.radStatVector)
MR.varCamp

Acc.varCamp <- var(Acc.radStatVector)
Acc.varCamp

# Daviazione standard
MR.sd <- sd(MR.radStatVector)
MR.sd 

Acc.sd <- sd(Acc.radStatVector)
Acc.sd

# Range interquartile (IQR)
```{r range, echo=FALSE,message=TRUE}
MR.iqr <- IQR(MR.radStatVector)
MR.iqr

Acc.iqr <- IQR(Acc.radStatVector)
Acc.iqr

# Simmetria
sym <- function(y){
  m1 <- mean((y-mean(y))^3)
  s <- m1/(sd(y)^3)
}

sym.MR <- sym(MR.radStatVector)
sym.MR

sym.Acc <- sym(Acc.radStatVector)
sym.Acc

# Curtosi
cur <- function(y){
  m2 <- mean((y-mean(y))^4)
  c <- m2/(sd(y)^4)
}

cur.MR <- cur(MR.radStatVector)
cur.MR
cur.Acc <- cur(Acc.radStatVector)
cur.Acc

# Deviazione assoluta dalla media (MAD)
```{r mad, echo=FALSE,message=TRUE}
MR.mad <- mad(MR.radStatVector)
MR.mad

Acc.mad <- mad(Acc.radStatVector)
Acc.mad 

## Statistica inferenziale
# Distribuzione della media campionaria
mu <- mean(MR.radStatVector)
sigma <- sd(MR.radStatVector)
k <- 100 #numero di SRS che prendo
n <- 20 #dimensione del campione 
hist(m, main="Distribuzione della media campionaria",breaks=20, col="green") #plot

# Mostro la media calcolata dalle k volte in cui realizzo la media campionaria e
# la media esatta, poi calcolo l'errore
mu_c
mu
errM <- abs(mu-mu_c)
errM

# Mostro la deviazione standard calcolata, che deve essere uguale o avvicinarsi
# a sigma/sqrt(n), poi calcolo l'errore.
sigma_c <- sd(m)
sigma_c
h <- sigma/sqrt(n)
h
errSD <- abs(h-sigma_c)
errSD

# Distribuzione della variabile aleatoria Z
z <- (m-mu)/(sigma/sqrt(n))

# Mostro media e deviazione standard di Z, la media si avviciner? a 0 e la
# deviazione standard a 1
zM <- mean(z)
zM
zSD <- sd(z)
zSD
hist(z, breaks=20, col="purple")

mu2 <- mean(MR.radStatVector)
sigma2 <- sd(MR.radStatVector)
k <- 100 #numero di SRS che prendo
n2 <- 35 #dimensione del campione 

m2 <- replicate(k, mean(rnorm(n2, mu2, sigma2)))

hist(m2, main="Distribuzione della media campionaria",breaks=20, col="yellow") #plot

mu_c2 <- mean(m2)
mu_c2 
mu2 
errM2 <- abs(mu2-mu_c2)
errM2

sigma_c2 <- sd(m2)
sigma_c2 
h2 <- sigma2/sqrt(n2)
h2
errSD2 <- abs(h2-sigma_c2)
errSD2

z2 <- (m2-mu2)/(sigma2/sqrt(n2))
hist(z2, breaks=20)

zM2 <- mean(z2)
zM2
zSD2 <- sd(z2)
zSD2

mu <- mean(MR.radStatVector)
sigma <- sd(MR.radStatVector)
n3 <- 50
alpha <- 0.05

x3 <- rnorm(n3, mu, sigma)
mCamp <- mean(x3) #media campionaria
S <- sd(x3) #deviazione standard campionaria

z_alpha <- qnorm(1-alpha, mean=0, sd=1)

# Calcolo i 2 estremi dell'intervallo
infM <- mCamp - z_alpha*S/sqrt(n3)
supM <- mCamp + z_alpha*S/sqrt(n3)

infM
supM

mu #Stampiamo la media esatta e vediamo se sta nell'intervallo di confidenza

#Proviamo a vedere su 100 prove se effettivamente rispetta la probabilit? del 95%
k <- 0
V.infM <- 1:100
V.supM <- 1:100

for(i in 1:100){
  y=rnorm(n3, mu, sigma)
  mCamp1 <- mean(y)
  S1<-sd(y)
  V.infM[i] <- mCamp1 - z_alpha*S1/sqrt(n3)
  V.supM[i] <- mCamp1 + z_alpha*S1/sqrt(n3)
  if (V.infM[i] <=  mu & mu <= V.supM[i]){
    k <- k+1
  }
}

# Ora stampo il numero di volte in cui la media esatta si trova all'interno
# dell'intervallo di confidenza calcolato sulla media campionaria
k

#### Features Selection
# Modello che usa tutti i features
model.SVM <- svm(diagnosis ~ ., ds, kernel="radial", cost=x, gamma=y/100)
y.pred <- predict(model.SVM, ds.test)

MR(y.pred, ds.test$diagnosis)
Acc(y.pred, ds.test$diagnosis)
 
# togliano dati relativi a: Raggio
model.SVM <- svm(diagnosis ~ . -radius_mean -radius_se -radius_worst, ds, kernel="radial", cost=x, gamma=y/100)
y.pred <- predict(model.SVM, ds.test)
MR(y.pred, ds.test$diagnosis)
Acc(y.pred, ds.test$diagnosis)

# togliano dati relativi a: Perimetro
model.SVM <- svm(diagnosis ~ . -perimeter_mean -perimeter_se -perimeter_worst, ds, kernel="radial", cost=x, gamma=y/100)
y.pred <- predict(model.SVM, ds.test)
MR(y.pred, ds.test$diagnosis)
Acc(y.pred, ds.test$diagnosis)

# togliano dati relativi a: Texture
model.SVM <- svm(diagnosis ~ . -texture_mean -texture_se -texture_worst, 
                 ds, kernel="radial", cost=x, gamma=y/100)
y.pred <- predict(model.SVM, ds.test)
MR(y.pred, ds.test$diagnosis)
Acc(y.pred, ds.test$diagnosis)

# togliano dati relativi a: Smoothness
model.SVM <- svm(diagnosis ~ . -smoothness_mean -smoothness_se -smoothness_worst, 
                 ds, kernel="radial", cost=x, gamma=y/100)
y.pred <- predict(model.SVM, ds.test)
MR(y.pred, ds.test$diagnosis)
Acc(y.pred, ds.test$diagnosis)

# togliano dati relativi a: Compactness
model.SVM <- svm(diagnosis ~ . -compactness_mean -compactness_se -compactness_worst, 
                 ds, kernel="radial", cost=x, degree=y/100)
y.pred <- predict(model.SVM, ds.test)
MR(y.pred, ds.test$diagnosis)
Acc(y.pred, ds.test$diagnosis)

# togliano dati relativi a: Concavity
model.SVM <- svm(diagnosis ~ . -concavity_mean -concavity_se -concavity_worst, ds, 
                 kernel="radial", cost=x, gamma=y/100)
y.pred <- predict(model.SVM, ds.test)
MR(y.pred, ds.test$diagnosis)
Acc(y.pred, ds.test$diagnosis)

# togliano dati relativi a: Concave
model.SVM <- svm(diagnosis ~ . -concave.points_mean -concave.points_se -concave.points_worst, 
                 ds, kernel="radial", cost=x, degree=y/100)
y.pred <- predict(model.SVM, ds.test)
MR(y.pred, ds.test$diagnosis)
Acc(y.pred, ds.test$diagnosis)

# togliano dati relativi a: Simmetry
model.SVM <- svm(diagnosis ~ . -symmetry_mean -symmetry_se -symmetry_worst, 
                 ds, kernel="radial", cost=x, gamma=y/100)
y.pred <- predict(model.SVM, ds.test)
MR(y.pred, ds.test$diagnosis)
Acc(y.pred, ds.test$diagnosis)

# togliano dati relativi a: fractal
model.SVM <- svm(diagnosis ~ . -fractal_dimension_mean -fractal_dimension_se -fractal_dimension_worst, 
                 ds, kernel="radial", cost=x, gamma=y/100)
y.pred <- predict(model.SVM, ds.test)
MR(y.pred, ds.test$diagnosis)
Acc(y.pred, ds.test$diagnosis)

# Teniamo solamente i peggiori
model.SVM <- svm(diagnosis ~ radius_worst + perimeter_worst + area_worst + 
                   texture_worst + smoothness_worst + compactness_worst + 
                   concavity_worst + concave.points_worst + concavity_worst + 
                   symmetry_worst + fractal_dimension_worst, ds, kernel="radial",
                   cost=x, gamma=y/100)

y.pred <- predict(model.SVM, ds.test)
MR(y.pred, ds.test$diagnosis)
Acc(y.pred, ds.test$diagnosis)