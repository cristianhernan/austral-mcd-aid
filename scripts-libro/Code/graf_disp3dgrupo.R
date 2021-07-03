library(scatterplot3d) # Paquete para generar gráficos en 3D
library(readxl) # Permite leer archivos xlsx

riesgo=read_excel("C:/.../riesgo.xlsx")

datos=data.frame(x=riesgo$PRESION,
                 y=riesgo$PESO, 
                 z=riesgo$EDAD, 
                 group=riesgo$SEXO)
# Arregla los datos

with(datos, scatterplot3d(x, y, z, box=FALSE, grid=TRUE, pch = 16,
                          color=ifelse(group=="M","royalblue","indianred3"), 
                          xlab="Presión", ylab="Peso", zlab="Edad"))
legend("topright", legend=unique(riesgo$SEXO), title = "Sexo", pch = 16,
       col=c("indianred3","royalblue"))
# Produce un dispersograma en 3D clasificado por grupos 
