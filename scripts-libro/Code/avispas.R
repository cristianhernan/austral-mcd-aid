library(readxl) # Permite leer archivos xlsx
library(ggplot2) # Paquete para confeccionar dibujos

avispas=read_excel("C:/.../avispas.xlsx")
# Importa la base con la cual se va a trabajar

avispas$Especie=factor(avispas$Especie) # Declara las especies como factor

ggplot(avispas, aes(Antena, Pata)) + 
  geom_point(aes(colour=Especie)) +
  xlab('Longitud de la antena (mm)') +
  ylab('Longitud de la pata (mm)')
# Realiza un diagrama de dispersión

especie.avispa=split(avispas,avispas$Especie)
# Agrupa los datos según la especie

prom.esp1=apply(especie.avispa[[1]][,1:2],2,mean)
prom.esp2=apply(especie.avispa[[2]][,1:2],2,mean)
prom.total=apply(avispas[,1:2],2,mean)
# Calcula los promedios para cada especie y del grupo general

S1=var(especie.avispa[[1]][,1:2])
round(S1,4)
S2=var(especie.avispa[[2]][,1:2])
round(S2,4)
# Calcula las matrices de varianzas-covarianzas para cada especie

S=(8*S1+5*S2)/13
round(S,4)
# Calcula las matrices de varianzas-covarianzas común

dist=t(prom.esp1-prom.esp2)%*%solve(S)%*%(prom.esp1-prom.esp2)

ggplot(avispas, aes(Antena, Pata)) + 
  geom_point(aes(colour=Especie)) +
  geom_point(aes(x=1.25, y=1.8), colour="black") +
  geom_point(aes(x=1.3, y=2), colour="blue") +
  geom_point(aes(x=1.5, y=1.7), colour="red") +
  xlab('Longitud de la antena (mm)') +
  ylab('Longitud de la pata (mm)')
# Realiza un diagrama de dispersión para AD

ggplot(avispas, aes(Antena, Pata)) + 
  geom_point(aes(colour=Especie)) +
  geom_abline(intercept=0.55, slope=1, linetype="dashed", color = "green") +
  xlab('Longitud de la antena (mm)') +
  ylab('Longitud de la pata (mm)')
# Realiza un diagrama de dispersión con línea discriminante

#######################################################
# Análsis discriminante

W=13*S 
round(W,4) # Calcula la matriz de covarianzas dentro de los grupos
B=(prom.esp1-prom.total)%*%t(prom.esp1-prom.total)+
  (prom.esp2-prom.total)%*%t(prom.esp2-prom.total)
round(B,4) # Calcula la matriz de covarianzas entre grupos

mat.avispas=as.matrix(avispas[1:15,1:2])
# Convierte los datos en matriz

mat.disc=solve(W)%*%B
round(mat.disc,4)
# Calcula la matriz discriminante

avect1=eigen(mat.disc)$vectors[,1] # Calcula el primer autovector
coord.disc.1=mat.avispas%*%avect1 # Calcula las proyecciones 

centroide.esp1=prom.esp1%*%avect1 
centroide.esp2=prom.esp2%*%avect1
# Calcula los centroides por especie

corte=prom.esp1%*%avect1+(9/15)*(prom.esp2%*%avect1-prom.esp1%*%avect1) 
# Calcula el punto de corte

clase=0
for(i in 1:15)
 {ifelse(coord.disc.1[i,1]>corte, 
         clase[i]<-"chaqueta amarilla", clase[i]<-"negra pequeña")}
clase 
# Clasifica los individuos con la función discriminante
table(clase,avispas$Especie)
# Compara la clasificación con la clase original

library(mvnormtest)	
# Paquete que generaliza el test de Shapiro-Wilk para el caso multivariado

mshapiro.test(t(mat.avispas))
# Realiza el test de Shapiro de normalidad multivariada

library(biotools) 
# Paquete con herramientas para análisis de conglomerados y de discriminante

boxM(avispas[,1:2], avispas$Especie)
# Realiza el test para comparar matrices de varianzas-covarianzas

z=lda(avispas$Especie~avispas$Antena+avispas$Pata, prior=c(9/15,6/15), 
      method="mle")
# Realiza el análisis discriminante lineal
proyecciones=-14.6*avispas[,1]+9.01*avispas[,2]
# Calcula las proyecciones aplicando la función discriminante lineal

library(klaR) # Paquete con funciones para clasificación y visualización
colores=c("cadetblue1","plum2")
partimat(Especie~Antena+Pata, data=avispas, method='lda', 
         image.colors=colores, col.mean="royalblue", pch=18, 
         main="Gráfico de partición", print.err=0,
         gs=c(rep(1,9),rep(20,6)))
# Produce un gráfico de partición de clases

prediccion=predict(z, avispas[,1:2])$class 
predict(z, avispas[,1:2])$x-proyecciones
# Calcula los valores predichos
table(avispas$Especie,prediccion) 
# Tabula al grupo original comparando con los valores predichos

################################################################

