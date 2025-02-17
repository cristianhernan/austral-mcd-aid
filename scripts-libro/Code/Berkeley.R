library(readxl) # Permite leer archivos xlsx

Berkeley=read_excel("C:/.../Berkeley.xlsx")
# Importa la base con la cual se va a trabajar

attach(Berkeley) # Fija la base de datos donde se trabaja

tabla.sexo=table(Sexo,Admisi�n) # Calcula la frecuencias por Sexo y Admisi�n
colnames(tabla.sexo)=c("NO","SI") # Cambia de nombre a las columnas
chisq.test(tabla.sexo) # Aplica el test Chi cuadrado

dptoA=table(Sexo[Departamento=="A"],Admisi�n[Departamento=="A"])
# Calcula la frecuencias por Sexo y Admisi�n del Departamento A
colnames(dptoA)=c("NO","SI")
chisq.test(dptoA)

dptoB=table(Sexo[Departamento=="B"],Admisi�n[Departamento=="B"])
# Calcula la frecuencias por Sexo y Admisi�n del Departamento B
colnames(dptoB)=c("NO","SI")
chisq.test(dptoB)

dptoC=table(Sexo[Departamento=="C"],Admisi�n[Departamento=="C"])
# Calcula la frecuencias por Sexo y Admisi�n del Departamento C
colnames(dptoC)=c("NO","SI")
chisq.test(dptoC)

dptoD=table(Sexo[Departamento=="D"],Admisi�n[Departamento=="D"])
# Calcula la frecuencias por Sexo y Admisi�n del Departamento D
colnames(dptoD)=c("NO","SI")
chisq.test(dptoD)

dptoE=table(Sexo[Departamento=="E"],Admisi�n[Departamento=="E"])
# Calcula la frecuencias por Sexo y Admisi�n del Departamento E
colnames(dptoE)=c("NO","SI")
chisq.test(dptoE)

dptoF=table(Sexo[Departamento=="F"],Admisi�n[Departamento=="F"])
# Calcula la frecuencias por Sexo y Admisi�n del Departamento F
colnames(dptoF)=c("NO","SI")
chisq.test(dptoF)
