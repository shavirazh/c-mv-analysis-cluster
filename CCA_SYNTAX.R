print("Hello World")

#Input data
library(readxl)
ispu_iklim <- read_excel("D:/datanum/ispu-iklim.xlsx", 
                           col_types = c("skip", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric"))
View(ispu_iklim)
head(ispu_iklim)


summary(ispu_iklim)

#Pengujian asumsi
##LINEARITAS menggunakan Ramsey test
library(lmtest)
resettest(Tavg ~ pm10, power=2, data=ispu_iklim) #terpenuhi
resettest(Tavg ~ so2, power=2, data=ispu_iklim) #terpenuhi
resettest(Tavg ~ co, power=2, data=ispu_iklim) #terpenuhi
resettest(Tavg ~ o3, power=2, data=ispu_iklim) #terpenuhi
resettest(Tavg ~ no2, power=2, data=ispu_iklim) #terpenuhi

resettest(RH_avg ~ pm10, power=2, data=ispu_iklim) #terpenuhi
resettest(RH_avg ~ so2, power=2, data=ispu_iklim) #terpenuhi
resettest(RH_avg ~ co, power=2, data=ispu_iklim) #terpenuhi
resettest(RH_avg ~ o3, power=2, data=ispu_iklim) #terpenuhi
resettest(RH_avg ~ no2, power=2, data=ispu_iklim) #terpenuhi

##Normalitas Multivariat
library(MVN)

mvn(data = ispu_iklim, multivariatePlot = 'qq')
tes <- mvn(ispu_iklim, mvnTest="mardia")
tes$multivariateNormality

result <- mvn(ispu_iklim, multivariateOutlierMethod = 'adj', showOutliers = TRUE, showNewData = TRUE)
ispu_iklim2 <- result$newData
View(ispu_iklim2)
mvn(data = ispu_iklim2, multivariatePlot = 'qq')
tes1 <- mvn(ispu_iklim2, mvnTest="mardia")
tes1$multivariateNormality

result2 <- mvn(ispu_iklim2, multivariateOutlierMethod = 'adj', showOutliers = TRUE, showNewData = TRUE)
ispu_iklim3 <- result2$newData
View(ispu_iklim3)
mvn(data = ispu_iklim3, multivariatePlot = 'qq')
tes2 <- mvn(ispu_iklim3, mvnTest="mardia")
tes2$multivariateNormality
#menghilangkan 43 data (47% dari data asli)...............

##Non-multikolinearitas (terpenuhi)
library(car)
ModelCC <- lm(Tavg + RH_avg ~ pm10 + so2 + co + o3 + no2, data=ispu_iklim1)
vif(ModelCC)

#ANALISIS KORELASI KANONIK
library(CCP)
library(CCA)
x <- cbind(ispu_iklim3[,1:5])
y <- cbind(ispu_iklim3[,6:7])

CC1 <- cc(x,y)
CC1

#Memunculkan korelasi kanonik
CC1$cor

#Memunculkan raw canonical
CC1$ycoef
CC1$xcoef

#Menghitung loading kanonik
CC2 <- comput(x,y,CC1)
CC2

#uji signifikansi
library(candisc)
cca <- cancor(x,y)
summary(cca)

# tests of canonical dimensions
rho <- CC1$cor
## Define number of observations, number of variables in first set, and number of variables in the second set.
n <- nrow(x)
p <- ncol(x)
q <- ncol(y)

## Calculate p-values using the F-approximations of different test statistics:
p.asym(rho, n, p, q, tstat = "Wilks")

#Uji signifikansi keseluruhan
a <- 0.48346^(1/2)

b <- (1-a)/a

F <- b*80/10
print(F)
#nilai tabel F sama dengan F(0.05;10;80)=1.91
#nilai secara keseluruhan signifikan karena 3.51>1.91

#REDUNDANSI
loadXU <- CC1$scores$corr.X.xscores #Loadings Kanonik X dengan U
loadYV <- CC1$scores$corr.Y.yscores #Loading kanonik Y dengan V

loadXUb <- loadXU[,1]
loadXUb

loadYVb <- loadYV[,1]
loadYVb

loadXU2 <- loadXUb^2
loadYV2 <- loadYVb^2

aveXU <- sum(loadXU2)/5
aveYV <- sum(loadYV2)/2

korUV2 <- CC1$cor[1]^2
korUV2
#KERAGAMAN X yg dapat djelaskan U1 dan U2 (Koef redundansi)
koef_redXU <- aveXU*korUV2
koef_redXU

#KERAGAMAN Y yg dapat dijelaskan V1 dan V2 (koef redundansi)
koef_redYV <- aveYV * korUV2
koef_redYV

#bobot kanonik
V <- CC1$xcoef
V

V1 <- V[,1]
V2 <- V[,2]

U <- CC1$ycoef
U

U1 <- U[,1]
U2 <- U[,2]

#muatan kanonik
#korelasi X dan V

CLV1 <- Kor$Xcor%*%V1
CLV1