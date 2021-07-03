library(ggplot2) # Paquete para confeccionar dibujos

mtcars$cilind=factor(mtcars$cyl) # Declara las cilindradas como factor

ggplot(mtcars, aes(wt, mpg)) + 
   geom_point(aes(colour=cilind)) +
  xlab("Peso") +
  ylab("Millas por galón") +
  labs(colour='Cilindrada')
# Produce un diagrama de dispersión

