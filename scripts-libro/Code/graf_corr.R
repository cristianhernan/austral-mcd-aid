library(corrplot) # Paquete para representaciones gráficas de matrices
library(readxl) # Permite leer archivos xlsx

IMCinfantil=read_excel("C:/.../IMCinfantil.xlsx")
# Importa la base con la cual se va a trabajar
attach(IMCinfantil) # Se pone la base en la memoria

base.niños=data.frame(EDAD,PESO,TALLA,IMC,CC) 
# Arma una sub-base con las variables numéricas de IMCinfantil
base.niños$CC=max(base.niños$CC)-base.niños$CC 
# Cambia la variable para que correlacione en forma negativa con las restantes
M=cor(base.niños) # Calcula la matriz de correlación
corrplot.mixed(M, lower="number", upper="shade", addshade="all")
# Produce un correlograma
