library(ggplot2) # Paquete para confeccionar dibujos
library(ggrepel) # Paquete que manipula etiquetas para gráficos
library(readxl) # Permite leer archivos xlsx

riesgo=read_excel("C:/.../riesgo.xlsx")

ggplot(riesgo, aes(x=PESO, y=SUPERFICIE)) + 
  geom_point(colour="royalblue", shape=8) +
  xlab("Peso") +
  ylab("Superficie corporal") +
  geom_text_repel(aes(label=rownames(riesgo), size = 2)) +
  theme(legend.position="none")
  
  
  
  
  
  