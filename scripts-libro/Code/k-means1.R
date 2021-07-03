library("scatterplot3d")
library("ggplot2")

# Leemos las bases de datos
library(readxl)
cluster <- read_excel("C:/Users/Usuario/Dropbox/LibroAnalisisdatos/Data/k-means1.xlsx")
cluster1 <- read_excel("C:/Users/Usuario/Dropbox/LibroAnalisisdatos/Data/k-means1paso1.xlsx")
cluster2 <- read_excel("C:/Users/Usuario/Dropbox/LibroAnalisisdatos/Data/k-means1paso2.xlsx")

#Hacemos los gráficos

# Paso 0

ggplot(cluster, aes(x, y)) + 
  geom_point(colour=c("red","green","orange","deepskyblue","purple","blue"),
             size=2) +
  geom_text(aes(label=c("1","2","3","4","5","6")),hjust=-1, vjust=0.5,
            col=c("red","green","orange","deepskyblue","purple","blue"))

# Cálculo de distancias
d0=round(dist(cluster, method = "euclidean", p = 2),2)
d1=round(dist(cluster1[,2:3], method = "euclidean", p = 2),2)
d2=round(dist(cluster2[,2:3], method = "euclidean", p = 2),2)
