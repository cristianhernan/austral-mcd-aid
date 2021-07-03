library(ggplot2) # Paquete para confeccionar dibujos
library(e1071) # Paquete que incluye análisis para las SVM

set.seed(12356) # Fija la semilla

x=c(rnorm(50,5,2), rnorm(50,8,1.5), rnorm(50,1,1.2))
y=c(abs(rnorm(50,5,2)),rnorm(50,8,1.5),rnorm(50,1,1.2))
# Simula un conjunto de puntos en el plano

Grupo=as.factor(c(rep("A",50),rep("B",50),rep("C",50))) 
# Agrupa los datos es tres grupos

datos=data.frame(x,y, Grupo)
# Arma la base de datos

ggplot(datos, aes(x,y)) +
  geom_point(aes(colour = factor(Grupo))) +
  labs(colour="Grupo") +
  xlab('Variable 1') +
  ylab('Variable 2') +
  c
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
# Produce una gráfico con los datos simulados

eliminados=sample(1:nrow(datos),100)
# Elimina algunos datos de la muestra
validacion=datos[eliminados,]
# Arma la muestra de validación
entrenamiento=datos[-eliminados,]
# Armamos la muestra de entrenamiento
modelo.svm=svm(Grupo~y+x, data=entrenamiento, method="C-classification",
               kernel="radial", cost=10, gamma=.1)
# Construye el modelo

predichos=data.frame(predict(modelo.svm, validacion))
clasificacion=cbind(validacion, predichos)
colnames(clasificacion)=c("x","y","Grupo","Predichos")
table(validacion$Grupo, clasificacion$Predichos)
# Calcula la tabla de confusión del modelo

plot(modelo.svm, datos, symbolPalette=topo.colors(4), dataSymbol="o",
     color.palette=cm.colors)
# visualiza la clasificacion del modelo
                                       