library("ggplot2")

# Leemos las bases de datos
library(readxl)

cluster1 <- read_excel("C:/Users/Usuario/Dropbox/LibroAnalisisdatos/Data/k-means3paso1.xlsx")
cluster2 <- read_excel("C:/Users/Usuario/Dropbox/LibroAnalisisdatos/Data/k-means3paso2.xlsx")

# Cálculo de distancias

d1=round(dist(cluster1[,2:4], method = "manhattan"),2)
d2=round(dist(cluster2[,2:4], method = "manhattan"),2)

cluster1=as.matrix(cluster1[,2:4]) # Convierte en formato matriz

# Cálculo de promedios por grupo
xrayag1k2=as.matrix(apply(cluster1[1:5,],2,mean))
xrayag2k2=as.matrix(apply(cluster1[6:9,],2,mean))
xrayag1k3=as.matrix(apply(cluster1[1:3,],2,mean))
xrayag2k3=as.matrix(apply(cluster1[4:6,],2,mean))
xrayag3k3=as.matrix(apply(cluster1[7:9,],2,mean))

# Inicialización
scdg1k2=matrix(0,nrow=5,ncol=3)
scdg2k2=matrix(0,nrow=4,ncol=3)
scdg1k3=matrix(0,nrow=3,ncol=3)
scdg2k3=matrix(0,nrow=3,ncol=3)
scdg3k3=matrix(0,nrow=3,ncol=3)

# Cálculo de diferencias de coordenadas
for (i in 1:5){scdg1k2[i,]=cluster1[i,]-t(xrayag1k2)}
for (i in 6:9){scdg2k2[i-5,]=cluster1[i,]-t(xrayag2k2)}
for (i in 1:3){scdg1k3[i,]=cluster1[i,]-t(xrayag1k3)}
for (i in 4:6){scdg2k3[i-3,]=cluster1[i,]-t(xrayag2k3)}
for (i in 7:9){scdg3k3[i-6,]=cluster1[i,]-t(xrayag3k3)}

# Cálculo de suma de cuadrados dentro del grupo
SCD2=sum(scdg1k2^2)+sum(scdg2k2^2)
SCD3=sum(scdg1k3^2)+sum(scdg2k3^2)+sum(scdg3k3^2)
