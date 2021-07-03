library(MASS) # Paquete de funciones y datos
library(lattice) # Paquete para visaulizar datos
library(grid) # Paquete con un sistema para gráficos
library(DMwR) # Paquete con funciones para data mining
 
cov1=cov.rob(stack.x, method="mcd", nsamp="exact") # Calcula MCD
cov2=cov.rob(stack.x, method="mve", nsamp="best") # Calcula MVE
cov3=cov.rob(stack.x, method="classical", nsamp="best")
# Calcula la matriz de covarianzas clásica
center1=apply(stack.x, 2, mean) # Calcula el vector de medias
center2=apply(stack.x,2,median) # Calcula el vector de medianas

dcov1=0 ; dcov2=0 ; dcov3=0 # Inicializaciones

for(i in 1:21){
 dcov1[i]=mahalanobis(stack.x[i,], cov1$center, cov1$cov, inverted = FALSE)
 dcov2[i]=mahalanobis(stack.x[i,], cov2$center, cov2$cov, inverted = FALSE)
 dcov3[i]=mahalanobis(stack.x[i,], cov3$center, cov3$cov, inverted = FALSE)
}
# Calcula distancias de Mahalanobis utilizando las distintas estimaciones
# de la matriz de covarianzas
round(cbind(dcov1,dcov2,dcov3),2)
# Combina las tres distancias para observar el resultado

distancias.outliers=lofactor(stack.x, k=5) 
# Calcula las distancias teniendo en cuenta cinco vecinos

plot(density(distancias.outliers), col="royalblue", main="", 
     xlab="n=21, ancho de banda = 0.06518", ylab="Densidad") 
# Dibuja la densidad estimada de las distancias de Mahalanobis de las 
# observaciones

outliers=order(distancias.outliers, decreasing=T)[1:5] 
# Arroja las observaciones correspondientes a las cinco distancias mayores
print(outliers)
  