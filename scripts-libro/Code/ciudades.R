library(readxl) # Permite leer archivos xlsx
library(ggplot2) # Paquete para confeccionar dibujos
library(ggrepel) # Paquete que manipula etiquetas para gráficos
library(plotrix) # Paquete para gráficos requerido para la librería smacof
library(smacof) # Paquete para MDS basado en la minimización del stress

cities=read_excel("C:/.../ciudades.xlsx")
# Importa la base con la cual se va a trabajar
ciudades=data.frame(cities[,2:3])

D=dist(ciudades) # Calcula las distancias euclídeas entre las filas
MCD_D=cmdscale(D, eig=TRUE, k=2)
# Realiza el MCD de una matriz de datos con k dimensiones de representación
x=MCD_D$points[,1] # Guarda las abscisas de los puntos
y=MCD_D$points[,2] # Guarda las ordenadas de los puntos

# Preparamos base de datos para el gráfico
data=cbind(-x,-y)
datos=data.frame(data)
colnames(datos)=c("Latitud","Longitud")
rownames(datos)=c("Buenos Aires","Córdoba","Rosario","Mendoza","Tucumán",
                  "Salta","Santa Fe","San Juan","Resistencia",
                  "Santiago del Estero","Corrientes","Posadas", 
                  "San Salvador de Jujuy","Bahía Blanca","Paraná",
                  "Neuquén")

ggplot(datos, aes(x=Latitud, y=Longitud))+
  geom_point(colour="royalblue") +
  geom_text_repel(aes(label=rownames(datos))) +
  theme_gray() 
# Realiza un gráfico de puntos 

MCD.D= smacofSym(D, ndim=2) # Realiza una escala multidimensional
MCD.D$stress # Calula el stress del ajuste
  

      
        
              