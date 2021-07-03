puntos <- read_excel("C:/Users/Debie/Dropbox/Libro Análisis de datos/Data/pruebaclusterkmeans.xlsx")
dist(puntos)

c1=puntos[1,]
c2=puntos[5,]


distan=as.matrix(dist(puntos))
paso1=cbind(disc1=distan[,1],
disc5=distan[,5])
grupo1=0
for(i in 1:9){
  grupo1[i]=which.min(as.vector(paso1[i,]))
}
grupo1
##

c12=apply(puntos[1:3,],2,mean)
c22=apply(puntos[4:9,],2,mean)

distan2=as.matrix(dist(rbind(puntos,c12,c22)))
paso2=cbind(disc12=distan2[,10],
            disc22=distan2[,11])
grupo2=0
for(i in 1:9){
  grupo2[i]=which.min(as.vector(paso2[i,]))
}
grupo2