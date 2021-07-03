library(scatterplot3d) # Paquete para generar gráficos en 3D
library(readxl) # Permite leer archivos xlsx

par(mfrow=c(1,2)) # Permite hacer gráficos simultáneos

riesgo=read_excel("C:/.../riesgo.xlsx")

scatterplot3d(riesgo[,2:4], angle=35, pch=16, color="royalblue", 
              box=FALSE, grid=TRUE,
              xlab="Presión", ylab="Edad", zlab="Peso")
scatterplot3d(riesgo[,2:4], angle=225, pch=16, color="royalblue", 
              box=FALSE, grid=TRUE,
              xlab="Presión", ylab="Edad", zlab="Peso")
# Producen dispersogramas en 3D con distintos ángulos de visión
  

