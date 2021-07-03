library(mclust) # Paquete con modelos Gaussianos para modelados basados en 
                # conglomerados, clasificación y estimación de densidades
library(ggplot2) # Paquete para confeccionar dibujos
library(gridExtra) # Paquete para acomodar gráficos simultáneos
library(corpcor) 
#Paquete que incluye una estimación eficiente de covarianza y correlación
library(Hotelling) # Paquete que implementa el test de Hotelling
library(car) # Paquete con funciones que acompañan regresión aplicada
library(mvnormtest) 
# Paquete que generaliza el test de Shapiro-Wil para el caso multivariado
library(biotools) 
# Paquete con herramientas para análisis de conglomerados y de discriminante
library(corrplot) # Paquete para la visualización gráfica de matrices
library(klaR) # Paquete con funciones para clasificación y visualización

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
# Función para obtener leyendas

data(banknote) # Base con la cual vamos a trabajar

bp.long=ggplot(data=banknote, aes(x=Status, y=Length, fill=Status)) +
  geom_boxplot(position='identity', alpha=0.5) +
  xlab("") +
  ylab("Longitud") +
  scale_fill_brewer(palette="Dark2", name="Estado",
                    breaks=c("counterfeit", "genuine"),
                    labels=c("apócrifo","genuino")) +
  theme(axis.text.x=element_blank(), axis.ticks=element_blank(),
        axis.text.y=element_text(size=6)) +
  theme(legend.position="bottom")
# Produce un boxplot para la longitud

bp.left=ggplot(data=banknote, aes(x=Status, y=Left, fill=Status)) +
  geom_boxplot(position='identity', alpha=0.5) +
  xlab("") +
  ylab("Izquierda") +
  scale_fill_brewer(palette="Dark2", name="Estado",
                    breaks=c("counterfeit", "genuine"),
                    labels=c("apócrifo","genuino")) +
  theme(axis.text.x=element_blank(), axis.ticks=element_blank(),
        axis.text.y=element_text(size=6)) +
  theme(legend.position="bottom")
# Produce un boxplot para la izquierda

bp.right=ggplot(data=banknote, aes(x=Status, y=Right, fill=Status)) +
  geom_boxplot(position='identity', alpha=0.5) +
  xlab("") +
  ylab("Derecha") +
  scale_fill_brewer(palette="Dark2", name="Estado",
                    breaks=c("counterfeit", "genuine"),
                    labels=c("apócrifo","genuino")) +
  theme(axis.text.x=element_blank(), axis.ticks=element_blank(),
        axis.text.y=element_text(size=6)) +
  theme(legend.position="bottom")
# Produce un boxplot para la derecha

bp.bot=ggplot(data=banknote, aes(x=Status, y=Bottom, fill=Status)) +
  geom_boxplot(position='identity', alpha=0.5) +
  xlab("") +
  ylab("Abajo") +
  scale_fill_brewer(palette="Dark2", name="Estado",
                    breaks=c("counterfeit", "genuine"),
                    labels=c("apócrifo","genuino")) +
  theme(axis.text.x=element_blank(), axis.ticks=element_blank(),
        axis.text.y=element_text(size=6)) +
  theme(legend.position="bottom")
# Produce un boxplot para abajo

bp.top=ggplot(data=banknote, aes(x=Status, y=Top, fill=Status)) +
  geom_boxplot(position='identity', alpha=0.5) +
  xlab("") +
  ylab("Arriba") +
  scale_fill_brewer(palette="Dark2", name="Estado",
                    breaks=c("counterfeit", "genuine"),
                    labels=c("apócrifo","genuino")) +
  theme(axis.text.x=element_blank(), axis.ticks=element_blank(),
        axis.text.y=element_text(size=6)) +
  theme(legend.position="bottom")
# Produce un boxplot para arriba


bp.diag=ggplot(data=banknote, aes(x=Status, y=Diagonal, fill=Status)) +
  geom_boxplot(position='identity', alpha=0.5) +
  xlab("") +
  ylab("Diagonal") +
  scale_fill_brewer(palette="Dark2",name="Estado",
                    breaks=c("counterfeit", "genuine"),
                    labels=c("apócrifo","genuino")) +
  theme(axis.text.x=element_blank(), axis.ticks=element_blank(),
        axis.text.y=element_text(size=6)) +
  theme(legend.position="bottom")
# Produce un boxplot para la diagonal

mylegend1=g_legend(bp.diag)
# Guarda una leyenda

grid.arrange(arrangeGrob(bp.long + theme(legend.position="none"),
                         bp.left + theme(legend.position="none"), 
                         bp.right + theme(legend.position="none"),
                         bp.bot + theme(legend.position="none"),
                         bp.top + theme(legend.position="none"),
                         bp.diag + theme(legend.position="none"), nrow=2),
             mylegend1, nrow=2, heights=c(10,3.5))
# Realiza un gráfico en simultáneo

fit=hotelling.test(.~Status, data=data.frame(banknote))
fit
# Realiza el test de Hotelling

qq.long=qqPlot(banknote$Length, xlab="Cuantiles normales", ylab="Longitud",
           col="green", pch=20, col.lines="royalblue", lwd=1)
# Produce un qq-plot para la longitud

qq.left=qqPlot(banknote$Left, xlab="Cuantiles normales", ylab="Izquierda",
               col="green", pch=20, col.lines="royalblue", lwd=1)
# Produce un qq-plot para la izquierda

qq.right=qqPlot(banknote$Right, xlab="Cuantiles normales", ylab="Derecha",
               col="green", pch=20, col.lines="royalblue", lwd=1)
# Produce un qq-plot para la derecha

qq.bot=qqPlot(banknote$Bottom, xlab="Cuantiles normales", ylab="Abajo",
               col="green", pch=20, col.lines="royalblue", lwd=1)
# Prodice un qq-plot para abajo

qq.top=qqPlot(banknote$Top, xlab="Cuantiles normales", ylab="Arriba",
               col="green", pch=20, col.lines="royalblue", lwd=1)
# Produce un qq-plot para arriba

qq.diag=qqPlot(banknote$Diagonal, xlab="Cuantiles normales", ylab="Diagonal",
               col="green", pch=20, col.lines="royalblue", lwd=1)
# Produce un qq-plot para la diagonal

C=t(banknote[,2:7])
mshapiro.test(C)
# Realiza el test de Shapiro-Wilk

boxM(data=banknote[,2:7], grouping=banknote[, 1])
# Realiza el test M de Box

genuino=cor(banknote[banknote$Status=='genuine',2:7])
apocrifo=cor(banknote[banknote$Status=='counterfeit',2:7])
par(mfrow=c(1,2))
corrplot(genuino, tl.cex=0.7, cl.cex=0.7, tl.col="royalblue")
corrplot(apocrifo, tl.cex=0.7, cl.cex=0.7, tl.col="royalblue")
# Visualiza las matrices de correlación

ADC=qda(formula=Status~Length+Left+Right+Bottom+Top+Diagonal, 
              data=banknote)
# Realiza el análisis discriminante cuadrático
predicciones=predict(object=ADC,banknote)
# Clasifica y calcula las probabilidades a posteriori
table(banknote$Status, predicciones$class, 
      dnn=c('Clase real','Clase predicha'))
# Compara las clasificaciones

set.seed(12349) # Fija una semilla
entrenamiento=sample(1:200,120) 
# Selecciona una muestra de entrenamiento de tamaño 120
modelo=qda(Status~Length+Left+Right+Bottom+Top +Diagonal, 
            data=banknote[entrenamiento,])
# Construye el modelo de predicción basados en la muestra de entrenamiento
pred=predict(modelo, newdata=banknote[-entrenamiento,])$class
# Clasifica con este modelo los datos de la muestra de validación
table(pred, banknote$Status[-entrenamiento])
# Comparamos las clasificaciones del conjunto de validación

colores=c("cadetblue1","plum2")
partimat(Status~., data=banknote, method='qda', image.colors=colores, 
         col.mean="royalblue", pch=18, gs=c(rep(1,100),rep(20,100)),
         nplots.vert=2,nplots.hor=2, main="")
# Produce un gráfico de partición de clases

########################################################################
# Análisis robusto

library(MASS) 
# Paquete con funciones y bases de datos para Estadística moderna aplicada

cov.gen=cov.rob(banknote[banknote$Status=="genuine",-1], method="mcd",
                nsamp="best")
cov.apo=cov.rob(banknote[banknote$Status=="counterfeit",-1], method="mcd",
                nsamp="best")
# Realiza las estimaciones robustas 

prom.gen=rep(cov.gen$center,100)
prom.apo=rep(cov.apo$center,100)
var.gen=as.matrix(cov.gen$cov)
var.apo=as.matrix(cov.apo$cov)
# Guarda las estimaciones

DR.gen=as.matrix(banknote[,-1]-prom.gen)%*%solve(var.gen)%*%
  t(as.matrix(banknote[,-1]-prom.gen))
DR.apo=as.matrix(banknote[,-1]-prom.apo)%*%solve(var.apo)%*%
  t(as.matrix(banknote[,-1]-prom.apo))
# Calcula las distancias de Mahalanobis robustas

clase=0 
for(i in 1:200) {
  ifelse(DR.gen[i]<DR.apo[i], clase[i]<-"Genuino", clase[i]<-"Apócrifo")}
# Clasifica con las distancias

table(banknote$Status, clase)
# Compara las clasificaciones originales con las robustas 


#################################################################

library(mclust) # Paquete con modelos Gaussianos para modelados basados en 
# conglomerados, clasificación y estimación de densidades
library(Hotelling) # Paquete que implementa el test de Hotelling

data(banknote) # Carga la base de datos
base=data.frame(banknote) # Convierte en un data.frame

fit=hotelling.test(.~Status, data=base) # Aplica el test de Hotteling
fit
