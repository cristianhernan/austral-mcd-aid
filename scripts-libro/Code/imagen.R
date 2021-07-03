library(jpeg) # Paquete para trabar con archivos de imagen JPEG
library(ggplot2) # Paquete para confeccionar dibujos

mapa=readJPEG("C:/Users/Usuario/Dropbox/LibroAnalisisdatos/Images/aerea.jpg") 
# Lee la imagen de un archivo jpg

imgDm=dim(mapa) # Obtiene la dimensión de la imagen
imgRGB=data.frame(
  x=rep(1:imgDm[2], each=imgDm[1]),
  y=rep(imgDm[1]:1, imgDm[2]),
  R=as.vector(mapa[,,1]),
  G=as.vector(mapa[,,2]),
  B=as.vector(mapa[,,3])
) # Asigna los canales RGB a los datos

clusters=3
# Variar con 4,5,6,7,8
kmimg=kmeans(imgRGB[, c("R","G","B")], centers=clusters)
colores=rgb(kmimg$centers[kmimg$cluster,])
# Aplica la clusterización por el algoritmo de k-means

ggplot(data=imgRGB, aes(x=x, y=y)) + 
  geom_point(colour=colores) +
  theme_void() 

## Calculamos sumas de cuadrados

kmimg3=kmeans(imgRGB[, c("R","G","B")], centers=3)
kmimg4=kmeans(imgRGB[, c("R","G","B")], centers=4)
kmimg5=kmeans(imgRGB[, c("R","G","B")], centers=5)
kmimg6=kmeans(imgRGB[, c("R","G","B")], centers=6)
kmimg7=kmeans(imgRGB[, c("R","G","B")], centers=7)
kmimg8=kmeans(imgRGB[, c("R","G","B")], centers=8)

ssd3=sum(kmimg3$withinss)
ssd4=sum(kmimg4$withinss)
ssd5=sum(kmimg5$withinss)
ssd6=sum(kmimg6$withinss)
ssd7=sum(kmimg7$withinss)
ssd8=sum(kmimg8$withinss)

sse3=kmimg3$betweens
sse4=kmimg4$betweens
sse5=kmimg5$betweens
sse6=kmimg6$betweens
sse7=kmimg7$betweens
sse8=kmimg8$betweens

ssd=c(ssd3,ssd4,ssd5,ssd6,ssd7,ssd8) # Suma de cuadrados dentro del grupo
sse=c(sse3,sse4,sse5,sse6,sse7,sse8) # Suma de cuadrados entre grupos

n=242*640 # Cantidad de objetos
est=0
for (k in 1:5) {est[k]=(ssd[k]-ssd[k+1])/(ssd[k+1]/(n-k-1))}
# Calcula los estadísticos
which.min(est) # Dice que el valor mínimo del estadístico es para k=6

