library(readxl) # Permite leer archivos xlsx
library(ggplot2) # Paquete para confeccionar dibujos
library(ggrepel) # Paquete que manipula etiquetas para gráficos

atencion=read_excel("C:/.../atencion.xlsx")
# Importa la base con la cual se va a trabajar

data=data.frame(atencion)
ggplot(data, aes(x=Cultura, y=Cantidad, colour=Atención)) + 
  geom_line() +
  xlab("Nivel cultural") +
  ylab("Cantidad de niños") +
  scale_x_discrete(limit=c("1","2","3","4","5","6"),
                   labels=c("A","B","C","D","E","F")) +
  scale_fill_brewer(palette="BuPu", name="Nivel de atención",
                    breaks=c("atento", "disperso", "sint.lev", "sint.mod"),
                    labels=c("Atento", "Disperso", "Síntomas leves", 
                             "Síntomas moderados"))

