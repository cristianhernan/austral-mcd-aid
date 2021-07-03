library(ggplot2) # Paquete para confeccionar dibujos
library(devtools) # Colecciónn de herramientas de desarrollo para paquetes
install_github("vqv/ggbiplot") # Instala paquete desde GitHub
library(ggbiplot) # Paquete para visualización de componentes principales
library(readxl) # Permite leer archivos xlsx

nad=read_excel("C:/.../nadadores.xlsx")
# Importa la base con la cual se va a trabajar

nadadores=data.frame(nad[,2:5])
nad.pc=prcomp(nadadores, center=TRUE, scale.=TRUE)

carga1=data.frame(cbind(tramo=1:4, 
                        primeracarga=data.frame(nad.pc$rotation)[,1]))
carga2=data.frame(cbind(tramo=1:4,
                        segundacarga=data.frame(nad.pc$rotation)[,2]))

ggplot(carga1, aes(tramo, primeracarga), fill=tramo) + 
  geom_bar(stat="identity", position="dodge", fill="royalblue", width=0.5) +
  xlab('Tramo') +
  ylab('Primera carga')

ggplot(carga2, aes(tramo, segundacarga), fill=tramo) + 
  geom_bar(stat="identity", position="dodge", fill="royalblue", width=0.5) +
  xlab('Tramo') +
  ylab('Segunda carga')



