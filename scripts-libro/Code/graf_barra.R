library(readxl) # Permite leer archivos xlsx

IMCinfantil=read_excel("C:/.../IMCinfantil.xlsx")
# Importa la base con la cual se va a trabajar
attach(IMCinfantil) # Se pone la base en la memoria

barplot(table(CatPeso),ylab=("Cantidad"),
        names.arg=c("Deficiente", "Normal", "Obeso", "Con sobrepeso"),
        col=c("palegreen1", "paleturquoise", "plum2", "lightpink1")) 
# Produce un diagrama de barras