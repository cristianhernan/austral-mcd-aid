library(readxl) # Permite leer archivos xlsx

IMCinfantil=read_excel("C:/.../IMCinfantil.xlsx")
# Importa la base con la cual se va a trabajar
attach(IMCinfantil) # Se pone la base en la memoria

SEX=4*(SEXO=="F")+5*(SEXO=="M")
base.niños=data.frame(EDAD, PESO, TALLA, IMC, CC) 
# Arma una sub-base con variables numéricas
pairs(base.niños, pch=19, cex=0.8, 
      col=SEX)
# Produce un diagrama de dispersión de a pares