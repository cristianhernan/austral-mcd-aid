library(ggplot2) # Paquete para confeccionar dibujos
library(gridExtra) # Paquete para acomodar gráficos simultáneos
library(readxl) # Permite leer archivos xlsx
install_github("vqv/ggbiplot") # Instala paquete desde GitHub
library(ggbiplot) # Paquete para visualización de componentes principales
library(ggrepel) # Paquete que manipula etiquetas para gráficos

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
# Función para obtener leyendas

nad=read_excel("C:/.../nadadores.xlsx")
# Importa la base con la cual se va a trabajar
datos=data.frame(c(nad$tr1, nad$tr2, nad$tr3, nad$tr4),
                 c(rep("tr1",14), rep("tr2",14), rep("tr3",14), rep("tr4",14)))
# Arregla los datos
colnames(datos)=c("Tiempo","Tramo")

bp=ggplot(data=datos,aes(y=Tiempo),colour=factor(Tramo)) +
   geom_boxplot(aes(x=Tramo,fill=factor(Tramo))) +
   ggtitle("Tiempos por tramos con datos originales") +
   xlab("") +
   ylab("") +
   theme(axis.text.x=element_blank(), axis.ticks=element_blank(),
        axis.line=element_line(colour="royalblue", size=0.5, linetype="solid")) +
   scale_fill_brewer(palette="BuPu", name="Tramo",
                    breaks=c("tr1", "tr2", "tr3", "tr4"),
                    labels=c("1","2","3","4")) +
   theme(plot.title=element_text(color="#666666", face="bold", size=9, 
                                hjust=0.5)) +
   theme(legend.position="bottom")
# Genera un boxplot

nad.cont=rbind(nad,c(15,18,12,12,10),c(16,8,15,5,11),c(17,10,13,12,8))
# Agrega nuevos datos
nad.cont=nad.cont[,-1]
# Quita una columna

datos.cont=data.frame(grupo=c(rep("original",14), rep("nuevo",3)),
                       c(nad.cont$tr1,nad.cont$tr2,nad.cont$tr3,nad.cont$tr4),
                       c(rep("tr1",17), rep("tr2",17), rep("tr3",17), 
                         rep("tr4",17)))
colnames(datos.cont)=c("Grupo", "Tiempo", "Tramo")
# Acomoda los datos

bpcont=ggplot(data=datos.cont,aes(y=Tiempo),colour=factor(Tramo)) +
       geom_boxplot(aes(x=Tramo,fill=factor(Tramo))) +
       ggtitle("Tiempos por tramos con datos modificados") +
       xlab("") +
       ylab("") +
       theme(axis.text.x=element_blank(), axis.ticks=element_blank(),
             axis.line=element_line(colour="royalblue", size=0.5, 
                                    linetype="solid")) +
       scale_fill_brewer(palette="BuPu", name="Tramo",
                    breaks=c("tr1", "tr2", "tr3", "tr4"),
                    labels=c("1","2","3","4")) +
  theme(plot.title=element_text(color="#666666", face="bold", size=9, 
                                hjust=0.5)) +
  theme(legend.position="bottom")
# Genera un boxplot

mylegend1=g_legend(bpcont)
# Guarda una leyenda

grid.arrange(arrangeGrob(bp + theme(legend.position="none"),
                         bpcont + theme(legend.position="none"), nrow=1),
                mylegend1, nrow=2, heights=c(10, 2.5))
# Realiza un gráfico en simultáneo

datos.tr=split(datos.cont,datos.cont$Tramo)
data=data.frame(datos.tr)
tr1=datos.tr[[1]]
tr2=datos.tr[[2]]
tr3=datos.tr[[3]]
tr4=datos.tr[[4]]
# Acomoda datos para gráfico

p12=ggplot(data, aes(tr1$Tiempo, tr2$Tiempo))+
  geom_point(aes(colour=factor(tr1$Grupo))) +
  labs(x="Tramo 1", y="Tramo 2", color = "Datos\n") +
  scale_color_manual(labels=c("Agregado", "Original"), 
                     values=c("indianred3", "royalblue")) +
  theme(axis.title=element_text(size=8), 
        axis.text=element_text(size=7), 
        legend.position="bottom")
# Genera un diagrama de dispersión

p13=ggplot(data, aes(tr1$Tiempo, tr3$Tiempo))+
  geom_point(aes(colour=factor(tr1$Grupo))) +
  labs(x="Tramo 1", y="Tramo 3", color = "Datos\n") +
  scale_color_manual(labels=c("Agregado", "Original"), 
                     values=c("indianred3", "royalblue")) +
  theme(axis.title=element_text(size=8),
        axis.text=element_text(size=7),
        legend.position="bottom")
# Genera un diagrama de dispersión

p14=ggplot(data, aes(tr1$Tiempo, tr4$Tiempo))+
  geom_point(aes(colour=factor(tr1$Grupo))) +
  labs(x="Tramo 1", y="Tramo 4", color = "Datos\n") +
  scale_color_manual(labels=c("Agregado", "Original"), 
                     values=c("indianred3", "royalblue")) +
  theme(axis.title=element_text(size=8),
        axis.text=element_text(size=7),
        legend.position="bottom")
# Genera un diagrama de dispersión

p23=ggplot(data, aes(tr2$Tiempo, tr3$Tiempo))+
  geom_point(aes(colour=factor(tr2$Grupo))) +
  labs(x="Tramo 2", y="Tramo 3", color = "Datos\n") +
  scale_color_manual(labels=c("Agregado", "Original"), 
                     values=c("indianred3", "royalblue")) +
  theme(axis.title=element_text(size=8),
        axis.text=element_text(size=7),
        legend.position="bottom")
# Genera un diagrama de dispersión

mylegend2=g_legend(p23)
# Guarda una leyenda

grid.arrange(arrangeGrob(p12 + theme(legend.position="none"),
                         p13 + theme(legend.position="none"), 
                         p14 + theme(legend.position="none"),
                         p23 + theme(legend.position="none"), nrow=2),
             mylegend2, nrow=2, heights=c(10, 3.5))
# Realiza un gráfico en simultáneo

nad.pca=princomp(nad.cont,cor = TRUE, scores = TRUE)
# Calcula las componentes principales para los nadadores con los datos agregados
summary(nad.pca) # Muestra la importancia de las componentes principales

load1=nad.pca$loadings[,1]
load2=nad.pca$loadings[,2]
# Calcula las cargas de las componentes principales

dat=data.frame(cbind(load1,load2))
x=factor(c("Tramo 1", "Tramo 2", "Tramo 3", "Tramo 4"))
# acomoda datos para gráfico

p1=ggplot(dat, aes(x=x, y=load1))+ 
  geom_bar(stat="identity", position="dodge",fill="royalblue",size=0.5)+
  ggtitle("Cargas de la primera componente") +
  xlab("") +
  ylab("") +
  theme(plot.title=element_text(color="#666666", face="bold", size=9, 
                                hjust=0.5)) 
# Genera un gráfico de barras

p2=ggplot(dat, aes(x=x, y=load2))+ 
  geom_bar(stat="identity", position="dodge",fill="royalblue",size=0.5)+
  ggtitle("Cargas de la segunda componente") +
  xlab("") +
  ylab("") +
  theme(plot.title=element_text(color="#666666", face="bold", size=9, 
                                hjust=0.5)) 
# Genera un gráfico de barras
  
grid.arrange(arrangeGrob(p1, p2, nrow=1))
# Realiza un gráfico en simultáneo

ggscreeplot(nad.pca, type = c('pev', 'cev')) +
  xlab('Número de componentes principales') +
  ylab('Proporción de la variabilidad explicada') +
  geom_line(colour='royalblue') +
  geom_point(colour='royalblue')
# Produce un gráfico de sedimentación

ggbiplot(nad.pca, obs.scale=1) +
  geom_point(colour="royalblue") +
  geom_text_repel(aes(label=1:17)) +
  theme(legend.position="none") +
  xlab("PC1 (53.6% de variabilidad explicada)") +
  ylab("PC2 (25.7% de variabilidad explicada)") 
# Genera un biplot

  
  







