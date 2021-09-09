library(readxl)
kemiskinanri <- read_excel("D:/datanum/kemiskinanri.xlsx", 
                           col_types = c("text", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric"))
View(kemiskinanri)

#------------------------------Outlier----------------------------------------

#pengecekan tipe data
str(kemiskinanri)
#eksplorasi data
summary(kemiskinanri)

#jarak
jarak <- dist(scale(kemiskinanri[,2:6]))
jarak
library(car)
modell <- lm(kemiskinanri$`Persentase penduduk miskin` ~ kemiskinanri$`Persentase Penduduk Tamat SLTA+`
             + kemiskinanri$`Persentase Pengangguran/Tidak Bekerja`
             + kemiskinanri$`Pengeluaran Perkapita`
             + kemiskinanri$`Ketersediaan Air Layak`
             + kemiskinanri$`Penerima Manfaat Program`)

vif(modell)
#----------------------------METODE AVERAGE------------------------------------
hierarkiave <- hclust(dist(scale(kemiskinanri[,2:6])), method="ave")
hierarkiave
plot(hierarkiave, labels=kemiskinanri$Provinsi) #dendogram

rect.hclust(hierarkiave,3) 		#plot mengelompokkan data
anggotaave <- cutree(hierarkiave,k=3) #hasil kelompok data
anggotaave

tabulasiave <- data.frame(kemiskinanri,anggotaave) # hasil kelompok data dalam bentuk data frame
View(tabulasiave)

cophenetic(hierarkiave) #jarak cophenetic average
#korelasi cophenetic
d1 <- dist(kemiskinanri)
hc <- hclust(d1, "ave")
d2 <- cophenetic(hc)
corave=cor(d1, d2)
corave
#-----------------------------METODE COMPLETE----------------------------------
hierarkicomp<-hclust(dist(scale(kemiskinanri[,2:6])), method="complete")
hierarkicomp
plot(hierarkicomp,labels=kemiskinanri$Provinsi) #dendogram

rect.hclust(hierarkicomp,3) 		  #plot mengelompokkan data
anggotacomp<-cutree(hierarkicomp,k=3) #hasil kelompok data
anggotacomp

tabulasicomp<-data.frame(kemiskinanri,anggotacomp) # hasil kelompok data dalam bentuk data frame
View(tabulasicomp)

cophenetic(hierarkicomp) #jarak cophenetic complete 
#korelasi cophenetic
d1 <- dist(kemiskinanri)
hc <- hclust(d1, "complete")
d2 <- cophenetic(hc)
corcomp=cor(d1, d2)
corcomp

#-------------------------------METODE SINGLE----------------------------------
hierarkising<-hclust(dist(scale(kemiskinanri[,2:6])), method="single")
hierarkising
plot(hierarkising,labels=kemiskinanri$Provinsi) #dendogram

rect.hclust(hierarkising,3) 		  #plot mengelompokkan data
anggotasing<-cutree(hierarkising,k=3) #hasil kelompok data
anggotasing

tabulasising <- data.frame(kemiskinanri,anggotasing) # hasil kelompok data dalam bentuk data frame
View(tabulasising)

cophenetic(hierarkising) #jarak cophenetic single
#korelasi cophenetic
d1 <- dist(kemiskinanri)
hc <- hclust(d1, "single")
d2 <- cophenetic(hc)
corsing=cor(d1, d2)
corsing
d1
#---------------------------------METODE WARD----------------------------------
hierarkiward <- hclust(dist(scale(kemiskinanri[,2:6])), method="ward.D")
hierarkiward
plot(hierarkiward,labels=kemiskinanri$Provinsi) #dendogram

rect.hclust(hierarkiward,3) 		  #plot mengelompokkan data
anggotaward<-cutree(hierarkiward,k=3) #hasil kelompok data
anggotaward

tabulasiward<-data.frame(kemiskinanri,anggotaward) # hasil kelompok data dalam bentuk data frame
View(tabulasiward)

cophenetic(hierarkiward) #jarak cophenetic ward
#korelasi cophenetic
d1 <- dist(kemiskinanri)
hc <- hclust(d1, "ward.D")
d2 <- cophenetic(hc)
corward=cor(d1, d2)
corward
