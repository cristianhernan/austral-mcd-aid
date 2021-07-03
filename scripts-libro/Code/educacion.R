library(ggplot2) # Paquete para confeccionar dibujos
library(pgirmess) 
# Paquete con herramientas para la lectura, escritura y transformación de datos

Puntajes=c(13,27,26,22,28,27,43,35,47,32,31,37,33,33,33,26,44,33,54)
Grupo=as.factor(c(rep("A",6), rep("B",6), rep("C",7)))
Rendimiento=data.frame(Grupo, Puntajes)
# Carga la base de datos

ggplot(Rendimiento, aes(x=Grupo, y=Puntajes, fill=Grupo)) +
  geom_boxplot() +
  xlab("") +
  scale_fill_brewer(palette="Pastel1") 
# Produce boxplots  

grupoA=Rendimiento[Rendimiento$Grupo=="A",2]
grupoB=Rendimiento[Rendimiento$Grupo=="B",2]
grupoC=Rendimiento[Rendimiento$Grupo=="C",2]
shapiro.test(grupoA)
shapiro.test(grupoB)
shapiro.test(grupoC)
# Aplica el test de Shapiro-Wilk a cada grupo

kruskal.test(Puntajes,Grupo)
# Realiza el test de Kruskal-Wallis
kruskalmc(Puntajes~Grupo)
# Realiza un test de comparación múltiple entre tratamientos luego del test de 
# Kruskal-Wallis
