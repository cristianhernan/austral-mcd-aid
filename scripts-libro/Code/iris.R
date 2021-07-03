Sys.setenv(R_ZIPCMD= "C:/Rtools/bin/zip") 
# Requerido para generar archivos xlsx
library(readxl) # Permite leer archivos xlsx
library(openxlsx) # Permite escribir archivos xlsx
library(ggplot2) # Paquete para confeccionar dibujos
library(corpcor) 
#Paquete que incluye una estimación eficiente de covarianza y correlación
library(Hotelling) # Paquete que implementa el test de Hotelling

iris.especie=split(iris,iris$Species)
# Agrupa los datos por especie

setosa=data.frame(iris.especie[[1]])[,-c(4,5)]
versicolor=data.frame(iris.especie[[2]])[,-c(4,5)]
# Toma las tres primeras tres primeras para cada variedad

total=data.frame(rbind(iris.especie[[1]][,-c(4,5)],
                       iris.especie[[2]][,-c(4,5)]))
media.conjunta=apply(total,2,mean)
media.setosa=apply(setosa,2,mean)
media.versicolor=apply(versicolor,2,mean)
# Calcula la media conjunta y por especie

# Vamos a preparar los datos para el gráfico de perfiles de medias
ms=as.matrix(media.setosa)
mv=as.matrix(media.versicolor)
medias=rbind(ms,mv)
datos=cbind(rep(c(1,2,3),2),medias,c(rep("setosa",3),rep("versicolor",3)))
colnames(datos)=c("Variables","Medias","Especie")
data=data.frame(datos)
nombre=paste("C:/.../datosiris.xlsx")
write.xlsx(data, file=nombre)
datosiris=read_excel("C:/.../datosiris.xlsx")
datosiris[,1:2]=as.numeric(unlist(datosiris[,1:2]))
ggplot(datosiris, aes(x=Variables, y=Medias, colour=Especie)) + 
  geom_line() +
  scale_x_discrete(limit=c("1", "2", "3"),
                   labels=c("Longitud sépalo", "Ancho sépalo",
                            "Longitud pétalo"))

var.setosa=round(var(setosa),3)
var.versicolor=round(var(versicolor),3)
var.amalgamada=round(49*(var.setosa+var.versicolor)/98,3)
# Calcula la matriz de varianzas-covarianza por especie y amalgamada

dif.med=(media.setosa-media.versicolor)
# Calcula la diferencia entre los vectores medios
T2=(50*50/100)*t(dif.med)%*%solve(var.amalgamada)%*%dif.med
# Calcula el estadístico de Hotelling
F.obs=(96/(3*98))*T2
# Calcula el valor observado F de Fisher-Snedecor
pvalor=1-pf(F.obs,3,96)  
# Estimamos el p-valor de la prueba
total.especie=data.frame(cbind(total,c(rep("setosa",50),rep("versicolor",50))))
colnames(total.especie)=c("Sepal.Length","Sepal.Width","Petal.Length",
                          "Especie")
fit=hotelling.test(.~Especie, data=total.especie)
# Aplica el test de Hotelling

# Replicamos lo anterior en el caso matricial    
C=rbind(c(1,-1,0),c(0,1,-1))
transf.setosa=as.matrix(setosa)%*%t(C)
transf.versicolor=as.matrix(versicolor)%*%t(C)
transf.total=cbind(rbind(transf.setosa,transf.versicolor),
                  Especie=factor(c(rep("setosa",50),rep("versicolor",50))))
transf.difmed=C%*%(media.setosa-media.versicolor)
transf.var=C%*%var.amalgamada%*%t(C)
transf.T2=(50*50/100)*t(transf.difmed)%*% solve(transf.var)%*%transf.difmed
transf.F.obs=(96/(3*98))*transf.T2
transf.fit=hotelling.test(.~Especie, data=data.frame(transf.total))



                      
                          