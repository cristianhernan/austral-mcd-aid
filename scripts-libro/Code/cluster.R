library("scatterplot3d")

# Leemos las bases de datos
library(readxl)
cluster <- read_excel("C:/Users/Debie/Dropbox/Libro Análisis de datos/Data/cluster.xlsx")
cluster1 <- read_excel("C:/Users/Usuario/Dropbox/LibroAnalisisdatos/Data/cluster1.xlsx")
cluster2 <- read_excel("C:/Users/Debie/Dropbox/Libro Análisis de datos/Data/cluster2.xlsx")
cluster3 <- read_excel("C:/Users/Debie/Dropbox/Libro Análisis de datos/Data/cluster3.xlsx")
cluster4 <- read_excel("C:/Users/Debie/Dropbox/Libro Análisis de datos/Data/cluster4.xlsx")
cluster5 <- read_excel("C:/Users/Debie/Dropbox/Libro Análisis de datos/Data/cluster5.xlsx")

#Hacemos los gráficos

# Paso 0
s3d=scatterplot3d(cluster[,2:4], angle=20, color=c("red","green","orange",
                                                   "deepskyblue","purple","blue",
                                                   "chocolate", "brown1"), 
                  pch=16, grid=TRUE, box=FALSE)
text(s3d$xyz.convert(cluster[, 2:4]), labels=c("1","2","3","4","5","6","7","8"),
     cex= 0.8, col=c("red","green","orange","deepskyblue","purple","blue",
                     "chocolate", "brown1"), pos=4)

# Paso 1
s3d1=scatterplot3d(cluster[,2:4], angle=20, color=c("red","green","orange",
                                                   "deepskyblue","purple","blue",
                                                   "chocolate", "blue"), 
                  pch=16, grid=TRUE, box=FALSE)
text(s3d1$xyz.convert(cluster[, 2:4]), labels=c("1","2","3","4","5","A","7",""),
     cex= 0.8, col=c("red","green","orange","deepskyblue","purple","blue",
                     "chocolate", "brown1"), pos=4)
p6 <- s3d1$xyz.convert(cluster[6,2],cluster[6,3],cluster[6,4])
p8 <- s3d1$xyz.convert(cluster[8,2],cluster[8,3],cluster[8,4])
segments(p6$x,p6$y,p8$x,p8$y,lwd=2,col="blue")

# Paso 2
s3d2=scatterplot3d(cluster[,2:4], angle=20, color=c("red","green","orange",
                                                    "deepskyblue","blue","blue",
                                                    "chocolate", "blue"), 
                   pch=16, grid=TRUE, box=FALSE)
text(s3d2$xyz.convert(cluster[, 2:4]), labels=c("1","2","3","4","","B","7",""),
     cex= 0.8, col=c("red","green","orange","deepskyblue","blue","blue",
                     "chocolate", "brown1"), pos=4)
p5 <- s3d2$xyz.convert(cluster[5,2],cluster[5,3],cluster[5,4])
p6 <- s3d2$xyz.convert(cluster[6,2],cluster[6,3],cluster[6,4])
p8 <- s3d2$xyz.convert(cluster[8,2],cluster[8,3],cluster[8,4])
segments(p6$x,p6$y,p8$x,p8$y,lwd=2,col="blue")
segments(p5$x,p5$y,p6$x,p6$y,lwd=2,col="blue")
segments(p5$x,p5$y,p8$x,p8$y,lwd=2,col="blue")

# Paso 3
s3d3=scatterplot3d(cluster[,2:4], angle=20, color=c("red","green","green",
                                                    "deepskyblue","blue","blue",
                                                    "chocolate", "blue"), 
                   pch=16, grid=TRUE, box=FALSE)
text(s3d3$xyz.convert(cluster[, 2:4]), labels=c("1","","C","4","","B","7",""),
     cex= 0.8, col=c("red","green","green","deepskyblue","blue","blue",
                     "chocolate", "brown1"), pos=4)
p2 <- s3d3$xyz.convert(cluster[2,2],cluster[2,3],cluster[2,4])
p3 <- s3d3$xyz.convert(cluster[3,2],cluster[3,3],cluster[3,4])
p5 <- s3d3$xyz.convert(cluster[5,2],cluster[5,3],cluster[5,4])
p6 <- s3d3$xyz.convert(cluster[6,2],cluster[6,3],cluster[6,4])
p8 <- s3d3$xyz.convert(cluster[8,2],cluster[8,3],cluster[8,4])
segments(p6$x,p6$y,p8$x,p8$y,lwd=2,col="blue")
segments(p5$x,p5$y,p6$x,p6$y,lwd=2,col="blue")
segments(p5$x,p5$y,p8$x,p8$y,lwd=2,col="blue")
segments(p2$x,p2$y,p3$x,p3$y,lwd=2,col="green")

# Paso 4
s3d4=scatterplot3d(cluster[,2:4], angle=20, color=c("green","green","green",
                                                    "deepskyblue","blue","blue",
                                                    "chocolate", "blue"), 
                   pch=16, grid=TRUE, box=FALSE)
text(s3d4$xyz.convert(cluster[, 2:4]), labels=c("","","D","4","","B","7",""),
     cex= 0.8, col=c("green","green","green","deepskyblue","blue","blue",
                     "chocolate", "brown1"), pos=4)
p1 <- s3d4$xyz.convert(cluster[1,2],cluster[1,3],cluster[1,4])
p2 <- s3d4$xyz.convert(cluster[2,2],cluster[2,3],cluster[2,4])
p3 <- s3d4$xyz.convert(cluster[3,2],cluster[3,3],cluster[3,4])
p5 <- s3d4$xyz.convert(cluster[5,2],cluster[5,3],cluster[5,4])
p6 <- s3d4$xyz.convert(cluster[6,2],cluster[6,3],cluster[6,4])
p8 <- s3d4$xyz.convert(cluster[8,2],cluster[8,3],cluster[8,4])
segments(p6$x,p6$y,p8$x,p8$y,lwd=2,col="blue")
segments(p5$x,p5$y,p6$x,p6$y,lwd=2,col="blue")
segments(p5$x,p5$y,p8$x,p8$y,lwd=2,col="blue")
segments(p2$x,p2$y,p3$x,p3$y,lwd=2,col="green")
segments(p1$x,p1$y,p2$x,p2$y,lwd=2,col="green")
segments(p1$x,p1$y,p3$x,p3$y,lwd=2,col="green")

# Paso 5
s3d5=scatterplot3d(cluster[,2:4], angle=20, color=c("green","green","green",
                                                    "blue","blue","blue",
                                                    "blue", "blue"), 
                   pch=16, grid=TRUE, box=FALSE)
text(s3d4$xyz.convert(cluster[, 2:4]), labels=c("","","D","","","","E",""),
     cex= 0.8, col=c("green","green","green","blue","blue","blue",
                     "blue", "blue"), pos=4)
p1 <- s3d5$xyz.convert(cluster[1,2],cluster[1,3],cluster[1,4])
p2 <- s3d5$xyz.convert(cluster[2,2],cluster[2,3],cluster[2,4])
p3 <- s3d5$xyz.convert(cluster[3,2],cluster[3,3],cluster[3,4])
p4 <- s3d5$xyz.convert(cluster[4,2],cluster[4,3],cluster[4,4])
p5 <- s3d5$xyz.convert(cluster[5,2],cluster[5,3],cluster[5,4])
p6 <- s3d5$xyz.convert(cluster[6,2],cluster[6,3],cluster[6,4])
p7 <- s3d5$xyz.convert(cluster[7,2],cluster[7,3],cluster[7,4])
p8 <- s3d5$xyz.convert(cluster[8,2],cluster[8,3],cluster[8,4])
segments(p2$x,p2$y,p3$x,p3$y,lwd=2,col="green")
segments(p1$x,p1$y,p2$x,p2$y,lwd=2,col="green")
segments(p1$x,p1$y,p3$x,p3$y,lwd=2,col="green")
segments(p4$x,p4$y,p5$x,p5$y,lwd=2,col="blue")
segments(p5$x,p5$y,p8$x,p8$y,lwd=2,col="blue")
segments(p8$x,p8$y,p7$x,p7$y,lwd=2,col="blue")
segments(p7$x,p7$y,p6$x,p6$y,lwd=2,col="blue")
segments(p6$x,p6$y,p4$x,p4$y,lwd=2,col="blue")

# Calculamos las distancias euclídeas

d1=round(dist(cluster[,2:4], method = "euclidean", p = 2),2)
d2=round(dist(cluster1[,2:4], method = "euclidean", p = 2),2)
d3=round(dist(cluster2[,2:4], method = "euclidean", p = 2),2)
d4=round(dist(cluster3[,2:4], method = "euclidean", p = 2),2)
d5=round(dist(cluster4[,2:4], method = "euclidean", p = 2),2)
d6=round(dist(cluster5[,2:4], method = "euclidean", p = 2),2)

# Hacemos el dendograma

d=dist(cluster[,2:4], method="euclidean", p = 2)
# Calcula la distancias euclídeas entre los datos originales
dendoaux=hclust(d, method="ward.D")
# Produce un dendograma
dendo=as.dendrogram(dendoaux)
# Se utiliza para personalizar el dendograma
nodePar=list(lab.cex=1, pch=c(NA, 19), cex=0.7, col="royalblue")
plot(dendo, xlab="", nodePar=nodePar, edgePar=list(col=2:3))
# Grafica un dendograma
#########################




d=dist(cluster[,2:4], method="manhattan", p = 2)
# Calcula la distancias euclídeas entre los datos originales
dendoaux=hclust(d, method="ward.D")
# Produce un dendograma
dendo=as.dendrogram(dendoaux)
# Se utiliza para personalizar el dendograma
nodePar=list(lab.cex=1, pch=c(NA, 19), cex=0.7, col="royalblue")
plot(dendo, xlab="", nodePar=nodePar, edgePar=list(col=2:3))
# Grafica un dendograma
