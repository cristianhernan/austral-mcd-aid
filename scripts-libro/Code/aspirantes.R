library(ggplot2) # Paquete para confeccionar dibujos
library(gridExtra) # Paquete para acomodar gráficos simultáneos
library(devtools) # Colecciónn de herramientas de desarrollo para paquetes
install_github("vqv/ggbiplot") # Instala paquete desde GitHub
library(ggbiplot) # Paquete para visualización de componentes principales
library(ggrepel) # Paquete que manipula etiquetas para gráficos
library(readxl) # Permite leer archivos xlsx

asp=read_excel("C:/.../aspirantes.xlsx")
# Importa la base con la cual se va a trabajar

asp.pca.cor=prcomp(asp[,2:16], center = TRUE, scale. = TRUE)
# Realiza el análisis de componentes principales para las variables estandarizadas

summary(asp.pca.cor) # Muestra la importancia de las componentes principales

ggscreeplot(asp.pca.cor, type = c("pev", "cev")) +
  xlab("Número de componentes principales") +
  ylab("Proporción de variabilidad explicada") +
  geom_line(colour='royalblue') +
  geom_point(colour='royalblue')
# Produce un gráfico de sedimentación

# Cálculo de cargas
c1=as.vector(round(asp.pca.cor$rotation[,1],4))
c2=as.vector(round(asp.pca.cor$rotation[,2],4))
c3=as.vector(round(asp.pca.cor$rotation[,3],4))
c4=as.vector(round(asp.pca.cor$rotation[,4],4))

criterio=factor(colnames(asp)[2:16])
datos=data.frame(criterio,c1,c2,c3,c4)
# Acomoda datos para gráfico

load1=ggplot(datos, aes(x=criterio, y=c1))+ 
  geom_bar(stat="identity", position="dodge",fill="royalblue",size=0.5)+
  ggtitle("Cargas de la primera componente") +
  xlab("") +
  ylab("") +
  theme(axis.text=element_text(size=5),
        plot.title=element_text(color="#666666", face="bold", size=9, 
                                hjust=0.5)) 
# Grafica las cargas de la primera componente principal

load2=ggplot(datos, aes(x=criterio, y=c2))+ 
  geom_bar(stat="identity", position="dodge",fill="royalblue",size=0.5)+
  ggtitle("Cargas de la segunda componente") +
  xlab("") +
  ylab("") +
  theme(axis.text=element_text(size=5),
        plot.title=element_text(color="#666666", face="bold", size=9, 
                                hjust=0.5)) 
# Grafica las segunda de la primera componente principal

load3=ggplot(datos, aes(x=criterio, y=c3))+ 
  geom_bar(stat="identity", position="dodge",fill="royalblue",size=0.5)+
  ggtitle("Cargas de la tercera componente") +
  xlab("") +
  ylab("") +
  theme(axis.text=element_text(size=5),
        plot.title=element_text(color="#666666", face="bold", size=9, 
                                hjust=0.5)) 
# Grafica las cargas de la tercera componente principal

load4=ggplot(datos, aes(x=criterio, y=c4))+ 
  geom_bar(stat="identity", position="dodge",fill="royalblue",size=0.5)+
  ggtitle("Cargas de la cuarta componente") +
  xlab("") +
  ylab("") +
  theme(axis.text=element_text(size=5),
        plot.title=element_text(color="#666666", face="bold", size=9, 
                                hjust=0.5)) 
# Grafica las cargas de la cuarta componente principal

grid.arrange(arrangeGrob(load1, load2, load3, load4, nrow=2))
# Realiza un gráfico en simultáneo


b12=ggbiplot(asp.pca.cor, obs.scale=1, choices=1:2)+
    geom_point(colour="royalblue") +
    geom_text_repel(aes(label=1:48), size=2) +
    theme(legend.position="none") +
    xlab("PC1 (50.1% de variabilidad explicada)") +
    ylab("PC2 (13.7% de variabilidad explicada)") +
    ggtitle("Biplot entre las componentes 1 y 2") +
    theme(axis.title=element_text(size=7),
          plot.title=element_text(color="#666666", face="bold", size=9, 
                                hjust=0.5)) 
# Genera un biplot entre las componentes 1 y 2

b34=ggbiplot(asp.pca.cor, obs.scale=1, choices=3:4)+
    geom_point(colour="royalblue") +
    geom_text_repel(aes(label=1:48), size=2) +
    theme(legend.position="none") +
    xlab("PC3 (9.7% de variabilidad explicada)") +
    ylab("PC4 (8.0% de variabilidad explicada)") +
    ggtitle("Biplot entre las componentes 3 y 4") +
    theme(axis.title=element_text(size=7),
          plot.title=element_text(color="#666666", face="bold", size=9, 
                                hjust=0.5)) 
# Genera un biplot entre las componentes 3 y 4

grid.arrange(arrangeGrob(b12, b34, nrow=1))
# Realiza un gráfico en simultáneo












