library(ggplot2) # Paquete para confeccionar dibujos
library(readxl) # Permite leer archivos xlsx
library(reshape) # Paquete para reestructurar datos

galle=read_excel("C:/.../galletitas.xlsx")
# Importa la base con la cual se va a trabajar
galle=read_excel("C:/Users/Debie/Dropbox/Libro Análisis de datos/Data/galletitas.xlsx")
dulces=split(galle, galle$Tipo)$dulce # Agrupa las dulces
saladas=split(galle, galle$Tipo)$salada # Agrupa las saladas
med.dul=apply(dulces[,2:6], 2, mean) # Calcula las medias de las dulces
med.sal=apply(saladas[,2:6], 2, mean) # Calcula las medias de las saladas

data.plot=data.frame(group=c(1,2,3,4,5), value1=med.dul+7, value2=med.sal)
melteddata = melt(data.plot, id = 'group')
# Arregla datos para gráfico

ggplot(melteddata, aes(x = group, y = value, colour = variable)) + 
  geom_line() +
  xlab("Variables") +
  ylab("Medias") +
  scale_x_discrete(limit=c("1", "2", "3", "4", "5"),
                 labels=c("Calorías", "Carbohidratos",
                          "Proteinas", "Grasas","Sodio")) +
  labs(colour='Tipo') +
  scale_colour_manual(labels = c("Saladas","Dulces" ),
                       values=c("royalblue", "green4"))
