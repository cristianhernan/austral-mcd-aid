library(readxl) # Permite leer archivos xlsx

te=read_excel("C:/.../te.xlsx")
# Importa la base con la cual se va a trabajar

te.anova=aov(Vitamina.B~Marca, data=vitamina) # Realiza el ANOVA
summary(te.anova) # Devuelve la síntesis de la prueba

te=data.frame(te)
Marca=factor(te$Marca) # Transforma en factor
bartlett.test(te$Vitamina.B, Marca) # Aplica el test de Bartlett

library(car) # Paquete con funciones que acompañan regresión aplicada
leveneTest(te$Vitamina.B~Marca)

library(nortest) 
# Paquete con pruebas para probar la hipótesis compuesta de normalidad
library(moments) # Paquete requerido para el test de D'agostino

shapiro.test(residuals(te.anova)) # Aplica el test de Shapiro-Wilk
ad.test(residuals(te.anova)) # Aplica el test de Anderson-Darlin 
agostino.test(residuals(te.anova)) # Aplica el test de D'Agostino

library(ggplot2) # Paquete para confeccionar dibujos

y=quantile(te$Vitamina.B, c(0.25, 0.75), type=5)
# Encuentra los cuartiles 1 y 3 para la muestra
x <- qnorm( c(0.25, 0.75))
# Encuentra los cuartiles 1 y 3 para la distribución Normal
slope <- diff(y) / diff(x) # Calcula la pendiente de la recta de regresión
int   <- y[1] - slope * x[1] # Calcula la constante de la recta de regresión

ggplot(te, aes(sample=residuals(te.anova))) +
  stat_qq(alpha = 0.5, color="royalblue") +
  xlab("Valores teóricos") +
  ylab("Valores de la muestra") +
  geom_abline(int=int, slope=slope, color="indianred")
# Realiza un qqplot
  





