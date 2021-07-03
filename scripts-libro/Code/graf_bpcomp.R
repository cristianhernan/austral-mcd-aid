library(ggplot2) # Paquete para confeccionar dibujos
library(readxl) # Permite leer archivos xlsx

kcalab=read_excel("C:/.../kcalab.xlsx")
# Importa la base con la cual se va a trabajar
datos=data.frame(kcalab) # Arregla los datos

ggplot(data=datos,aes(y=kcal),colour=factor(Laboratorio)) +
  geom_boxplot(aes(x=Laboratorio,fill=factor(Laboratorio))) +
  xlab("") +
  ylab("Calorías") +
  theme(axis.text.x=element_blank(), axis.ticks=element_blank(),
        axis.line=element_line(colour="royalblue", size=0.5, linetype="solid")) +
  labs(fill='Laboratorio') +
  scale_fill_brewer(palette="BuPu")
# Produce un diagrama compartaivo de boxplots
