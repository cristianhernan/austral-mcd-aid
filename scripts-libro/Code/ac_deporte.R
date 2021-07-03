library(ca)	# Paquete para análisis de correspondencias
library(FactoMineR) # Paquete con métodos de análisis exploratorio de datos
library(factoextra) # Paquete para análisis multivariado de datos
library(ggplot2) # Paquete para confeccionar dibujos

# Armamos la base de datos

noprac=c(31,22)
hasta3=c(38,10)
masde3=c(40,6)
deporte=as.matrix(rbind(noprac,hasta3,masde3))
colnames(deporte)=c("Ausencia de depresión","Presencia de depresión")
rownames(deporte)=c("Sin práctica","Hasta 3 veces por semana",
                    "Más de 3 veces por semana")

Xsq=chisq.test(deporte)
Xsq$expected


