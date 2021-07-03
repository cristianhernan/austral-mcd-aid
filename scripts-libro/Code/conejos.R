library(readxl) # Permite leer archivos xlsx
library(ggplot2) # Paquete para confeccionar dibujos
library(car) # Paquete con funciones que acompañan regresión aplicada
library(MASS) 
# Paquete con funciones y bases de datos para la librería de Venables y Ripley

conejos=read_excel("C:/.../conejos.xlsx")
# Importa la base con la cual se va a trabajar

pordieta=split(conejos$Colesterol,conejos$Dieta) # Separa los datos según dieta
lapply(pordieta,mean) # Calcula las medias
lapply(pordieta,sd) # Calcula los desvíos estándar

ggplot(conejos, aes(x=Dieta, y=Colesterol, fill=Dieta)) +
  geom_boxplot() +
  xlab("") +
  scale_fill_brewer(palette="Pastel1") 
# Produce boxplots

attach(conejos)

colesterol.anova=aov(Colesterol~Dieta, data=conejos) # Realiza el ANOVA
summary(colesterol.anova) 
# Sirve para analizar si las diferencias son significativas
shapiro.test(residuals(colesterol.anova))
# Testea la normalidad de los residuos del modelo
leveneTest(Colesterol~Dieta, data=conejos)
# Testea el supuesto de homocedasticidad

boxcox(Colesterol~Dieta, plotit=T)
# Investiga qué transformación deja los datos más próximos a la normalidad
 
tcolesterol.anova=aov(Colesterol^(-0.5)~Dieta, data=conejos)
# Realiza el ANOVA para los datos transformados
summary(tcolesterol.anova)
# Analiza si las diferencias observadas siguen siendo significativas
shapiro.test(residuals(tcolesterol.anova))
# Testea la normalidad de los residuos
leveneTest(Colesterol^(-0.5)~Dieta, data=conejos)
# Testea el supuesto de homocedasticidad
TukeyHSD(tcolesterol.anova, conf.level=0.95)
# Realiza las comparaciones múltiples a posteriori entre los valores medios