library(readxl) # Permite leer archivos xlsx

m=read_excel("C:/.../ejemplosimple.xlsx")
# Importa la base con la cual se va a trabajar

M=as.matrix(m)
Mf=m[,2:3] # Guarda la matriz que caracteriza las filas
Mc=m[,4:6] # Guarda la matriz que caracteriza las columnas
F=t(Mf)%*%Mc # Arma la tabla de contingencia
totalf=F%*%rep(1,3) # Calcula el vector totales fila
totalc=rep(1,2)%*%F # Calcula el vector totales columna
n=sum(totalf) # Calcula el total de observaciones
Fr=F/n # Calcula las frecuencias relativas al total de observaciones 
round(Fr,2) # Exhibe el resultado con 2 decimales

Df=diag(as.vector(totalf))
# Arma la matriz diagonal con las frecuencias de las filas
R=solve(Df)%*%F # Calcula la matriz R
round(R,3) # Exhibe el resultado con 3 decimales

Dc=diag(as.vector(totalc)) 
# Arma la matriz diagonal con las frecuencias de las filas
distchi12=(R[1,]-R[2,])%*%solve(Dc)%*%(R[1,]-R[2,]) 
# Calculamos la distancia chi cuadrado entre las filas 1 y 2

      