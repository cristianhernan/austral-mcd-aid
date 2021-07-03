library(ggplot2) # Paquete para confeccionar dibujos
library(devtools) # Colecciónn de herramientas de desarrollo para paquetes
install_github("vqv/ggbiplot") # Instala paquete desde GitHub
library(ggbiplot) # Paquete para visualización de componentes principales
library(readxl) # Permite leer archivos xlsx

nad=read_excel("C:/.../nadadores.xlsx")
# Importa la base con la cual se va a trabajar

nadadores=data.frame(nad[,2:5])
nad.pca.cov=prcomp(nadadores, center = TRUE, scale. = FALSE)
# Realiza el análisis de componentes principales
nad.pca.cor=prcomp(nadadores, center = TRUE, scale. = TRUE) 
# Realiza el análisis de componentes principales para las variables estandarizadas
summary(nad.pca.cor)
summary(nad.pca.cov) 
# Realiza un resumen de las variabilidades explicadas por las componentes principales
      
ggscreeplot(nad.pca.cov, type = c('pev', 'cev')) +
  xlab('Número de componentes principales') +
  ylab('Proporción de la variabilidad explicada') +
  geom_line(colour='royalblue') +
  geom_point(colour='royalblue')
# Produce un gráfico de sedimentación



