# Cargamos los datos
Arbequina=c(34.5,20.1,21.8,18.2,19.5,20.2,22.5,23.9,22.1,24.2)
Carolea=c(16.4,14.8,17.8,12.3,11.9,15.5,13.4,16,15.8,16.2)

shapiro.test(Arbequina) # Testea la normalidad de los datos
shapiro.test(Carolea) # Testea la normalidad de los datos
 
wilcox.test(Arbequina, Carolea, alternative="two.sided")	
# Realiza el test de Mann-Whitney-Wilcoxon bilateral

library(ggplot2) # Paquete para confeccionar dibujos

# Carga de datos
a=data.frame(group = "Arbequina", value = Arbequina)
c=data.frame(group = "Carolea", value = Carolea)

plot.data=rbind(a, c) # Junta filas

ggplot(plot.data, aes(x=group, y=value, fill=group)) +  
  geom_boxplot() + 
  xlab("") +
  ylab("Aceite") +
  scale_fill_brewer(palette="Pastel1") +
  labs(fill='Variedad') 
# Produce boxplots

library(RVAideMemoire) 
# Paquete que contiene funciones misceláneas útiles en bioestadística
library(readxl) # Permite leer archivos xlsx

aceite=read_excel("C:/.../aceite.xlsx")
# Importa la base con la cual se va a trabajar

mood.medtest(Aceite~Variedad, data=aceite)
# Realiza el test de la mediana de Mood