library(plotrix)  # Paquete para manipular dibujos
library(readxl) # Permite leer archivos xlsx

IMCinfantil=read_excel("C:/.../IMCinfantil.xlsx")
# Importa la base con la cual se va a trabajar
attach(IMCinfantil) # Se pone la base en la memoria

frec.catpeso=table(CatPeso) # Calcula las frecuencias de las categorías de peso
etiquetas=c("Deficiente", "Normal", "Obeso", "Con sobrepeso") # Define etiquetas

pie3D(frec.catpeso, labels=etiquetas, explode=0.5, labelcex=0.8, radius=2,
      height=0.1, shade=0.7, 
      col=c("palegreen1", "paleturquoise", "plum2", "lightpink1"))
# Produce un diagrama circular
