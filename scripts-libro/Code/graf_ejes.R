library(ggplot2) # Paquete para confeccionar dibujos
library(readxl) # Permite leer archivos xlsx

riesgo=read_excel("C:/.../riesgo.xlsx")

ggplot(riesgo, aes(x=PESO, y=SUPERFICIE)) + 
  geom_point(colour="royalblue", pch=16) +
  xlab("Peso") +
  ylab("Superficie corporal") +
  theme(legend.position="none") +
  geom_segment(aes(86,1.75,xend=101.5,yend=2.25),size=0.5,colour="indianred3",arrow = arrow(length = unit(0.03, "npc"))) +
  geom_segment(aes(95.5,1.78,xend=90.5,yend=2.23),size=0.5 ,colour="indianred3",arrow = arrow(length = unit(0.03, "npc"))) 
  
  
  
  
  
  
  