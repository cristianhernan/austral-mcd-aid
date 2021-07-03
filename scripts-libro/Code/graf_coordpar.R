library(ggplot2) # Paquete para confeccionar dibujos
library(GGally) # Paquete que extiende funciones de ggplot2

ggparcoord(data=iris, columns=1:4, mapping=aes(color=as.factor(Species))) +
  scale_color_discrete("Especies", labels=levels(iris$Species)) +
  xlab("") +
  ylab("") +
  scale_x_discrete(limit=c("Sepal.Length", "Sepal.Width", "Petal.Length", 
                             "Petal.Width"),
                   labels=c("Longitud del sépalo", "Ancho del sépalo",
                            "Longitud del pétalo", "Ancho del pétalo"))
# Produce diagrama de coordenadas paralelas  
  

