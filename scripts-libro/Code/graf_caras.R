library(tcltk2) # Paquete que permite hacer caras de Chernoff
library(aplpack) # Paquete que permite hacer caras de Chernoff
library(readxl) # Permite leer archivos xlsx

galle=read_excel("C:/.../galletitas.xlsx")
# Importa la base con la cual se va a trabajar

saladas=split(galle, galle$Tipo)$salada # Agrupa las saladas

faces(saladas[,2:6], nrow.plot=2, ncol.plot=5, face.type=1, 
      labels=saladas$Marca) 
# Produce un diagrama de caras de Chernoff
