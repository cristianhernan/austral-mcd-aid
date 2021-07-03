library("scatterplot3d")

# Leemos las bases de datos
library(readxl)

cluster <- read_excel("C:/Users/Usuario/Dropbox/LibroAnalisisdatos/Data/k-means2.xlsx")
cluster1 <- read_excel("C:/Users/Usuario/Dropbox/LibroAnalisisdatos/Data/k-means2paso1.xlsx")
cluster2 <- read_excel("C:/Users/Usuario/Dropbox/LibroAnalisisdatos/Data/k-means2paso2.xlsx")

# Cálculo de distancias

d1=round(dist(cluster1[,2:4], method = "manhattan"),2)
d2=round(dist(cluster2[,2:4], method = "manhattan"),2)

# Gráfico
s3d=scatterplot3d(cluster, angle=20, color=c("darkgreen","green","orange",
                                             "deepskyblue","purple","blue",
                                              "chocolate", "brown1","red"), 
                  pch=16, grid=TRUE, box=FALSE)
text(s3d$xyz.convert(cluster), labels=c("1","2","3","4","5","6","7","8","9"),
     cex= 0.8, col=c("darkgreen","green","orange","deepskyblue","purple","blue",
                     "chocolate", "brown1","red"), pos=4)

