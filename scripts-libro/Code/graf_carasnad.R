library(tcltk2) # Paquete que permite hacer caras de Chernoff
library(aplpack) # Paquete que permite hacer caras de Chernoff
library(readxl) # Permite leer archivos xlsx

nad=read_excel("C:/.../nadadores.xlsx")
# Importa la base con la cual se va a trabajar

faces(nadadores, nrow.plot=3, ncol.plot=5, face.type=1, 
      labels=as.character(nadadores$nadador)) 
# Produce un diagrama de caras de Chernoff


