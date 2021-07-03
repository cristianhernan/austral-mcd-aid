library(readxl) # Permite leer archivos xlsx

Berkeley=read_excel("C:/.../Berkeley.xlsx")
# Importa la base con la cual se va a trabajar

attach(Berkeley) # Fija la base de datos donde se trabaja

tabla.sexo=table(Sexo,Admisión) # Calcula la frecuencias por Sexo y Admisión
colnames(tabla.sexo)=c("NO","SI") # Cambia de nombre a las columnas
chisq.test(tabla.sexo) # Aplica el test Chi cuadrado

dptoA=table(Sexo[Departamento=="A"],Admisión[Departamento=="A"])
# Calcula la frecuencias por Sexo y Admisión del Departamento A
colnames(dptoA)=c("NO","SI")
chisq.test(dptoA)

dptoB=table(Sexo[Departamento=="B"],Admisión[Departamento=="B"])
# Calcula la frecuencias por Sexo y Admisión del Departamento B
colnames(dptoB)=c("NO","SI")
chisq.test(dptoB)

dptoC=table(Sexo[Departamento=="C"],Admisión[Departamento=="C"])
# Calcula la frecuencias por Sexo y Admisión del Departamento C
colnames(dptoC)=c("NO","SI")
chisq.test(dptoC)

dptoD=table(Sexo[Departamento=="D"],Admisión[Departamento=="D"])
# Calcula la frecuencias por Sexo y Admisión del Departamento D
colnames(dptoD)=c("NO","SI")
chisq.test(dptoD)

dptoE=table(Sexo[Departamento=="E"],Admisión[Departamento=="E"])
# Calcula la frecuencias por Sexo y Admisión del Departamento E
colnames(dptoE)=c("NO","SI")
chisq.test(dptoE)

dptoF=table(Sexo[Departamento=="F"],Admisión[Departamento=="F"])
# Calcula la frecuencias por Sexo y Admisión del Departamento F
colnames(dptoF)=c("NO","SI")
chisq.test(dptoF)
