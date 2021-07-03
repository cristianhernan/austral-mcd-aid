library(ggplot2) # Paquete para confeccionar dibujos
library(devtools) # Colecciónn de herramientas de desarrollo para paquetes
install_github("vqv/ggbiplot") # Instala paquete desde GitHub
library(ggbiplot) # Paquete para visualización de componentes principales
library(ggrepel) # Paquete que manipula etiquetas para gráficos
library(readxl) # Permite leer archivos xlsx

nad=read_excel("C:/.../nadadores.xlsx")
# Importa la base con la cual se va a trabajar

nadadores=data.frame(nad[,1:5])
nad.pc=prcomp(nadadores[,2:5], center=TRUE, scale.=TRUE)

ggbiplot(nad.pc, obs.scale=1) +
geom_point(colour="royalblue") +
geom_text_repel(aes(label=nadadores[,1])) +
theme(legend.position="none") +
xlab("PC1 (73.1% de variabilidad explicada)") +
ylab("PC2 (22.9% de variabilidad explicada)") 
# Genera un biplot


