setwd("C:/Users/Debie/Dropbox/Libro Análisis de datos/Code")
getwd()


library(readxl) # Permite leer archivos xlsx
library(FactoMineR) # Paquete con métodos de análisis exploratorio de datos
library(factoextra) # Paquete para análisis multivariado de datos
library(ggplot2) # Paquete para confeccionar dibujos


tabla1=cbind(c(10,12,15),c(12,10,14),c(11,13,13))
colnames(tabla1)=c("B1","B2","B3")
rownames(tabla1)=c("A1","A2","A3")
sum(tabla1)

ejem1.ac=CA(tabla1, graph = FALSE) # Realiza el analisis de correspondencias 
summary(ejem1.ac) # Muestra el resultado del análisis de correspondencias
inertia1=chisq.test(tabla1)$statistic/110
inertia1
plot.new()
fviz_ca_biplot(ejem1.ac, repel=TRUE, col.row="royalblue", 
               col.col="indianred") +
  theme_gray() +
   ggtitle('') 

###
tabla2=cbind(c(4,12,15),c(12,10,14),c(11,13,19))
sum(tabla2)
colnames(tabla2)=c("B1","B2","B3")
rownames(tabla2)=c("A1","A2","A3")
inertia2=chisq.test(tabla2)$statistic/110
inertia2

ejem2.ac=CA(tabla2, graph = FALSE) # Realiza el analisis de correspondencias 
summary(ejem2.ac) # Muestra el resultado del análisis de correspondencias

plot.new()
fviz_ca_biplot(ejem2.ac, repel=TRUE, col.row="royalblue", 
               col.col="indianred") +
  theme_gray() +
    ggtitle('') 
###
tabla3=cbind(c(5,12,15),c(16,5,14),c(11,13,19))
colnames(tabla3)=c("B1","B2","B3")
rownames(tabla3)=c("A1","A2","A3")
inertia3=chisq.test(tabla3)$statistic/110
inertia3
sum(tabla3)
ejem3.ac=CA(tabla3, graph = FALSE) # Realiza el analisis de correspondencias 
summary(ejem3.ac) # Muestra el resultado del análisis de correspondencias

plot.new()
fviz_ca_biplot(ejem3.ac, repel=TRUE, col.row="royalblue", 
               col.col="indianred") +
  theme_gray() +
  ggtitle('') 
###
tabla4=cbind(c(5,2,25),c(16,5,14),c(13,19,11))
colnames(tabla4)=c("B1","B2","B3")
rownames(tabla4)=c("A1","A2","A3")
inertia4=chisq.test(tabla4)$statistic/110
inertia4
sum(tabla4)
ejem4.ac=CA(tabla4, graph = FALSE) # Realiza el analisis de correspondencias 
summary(ejem4.ac) # Muestra el resultado del análisis de correspondencias

plot.new()
fviz_ca_biplot(ejem4.ac, repel=TRUE, col.row="royalblue", 
               col.col="indianred") +
  theme_gray() +
  ggtitle('') 
###
inertia1
inertia2
inertia3
inertia4

tabla4
