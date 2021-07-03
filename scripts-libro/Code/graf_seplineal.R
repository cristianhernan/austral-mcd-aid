library(readxl)
library(ggplot2)
library(ggpubr)

demoseparable <- read_excel("C:/Users/Debie/Dropbox/Libro Análisis de datos/Data/demoseparable.xlsx")
demonoseparable <- read_excel("C:/Users/Debie/Dropbox/Libro Análisis de datos/Data/demonoseparable.xlsx")
demoenvolvente <- read_excel("C:/Users/Debie/Dropbox/Libro Análisis de datos/Data/demoenvolvente.xlsx")

ggplot(demoseparable, aes(Antena, Pata)) + 
  geom_point(aes(colour=Grupo)) +
  geom_abline(intercept=0.565, slope=1, color = "black") +
  geom_abline(intercept=0.6, slope=1, linetype="dashed", color = "violet") +
  geom_abline(intercept=0.53, slope=1, linetype="dashed", color = "green") +
  xlab('Variable 1') +
  ylab('Varable 2')+
  annotate(geom="text", x=1.51, y=2.04, label="wz+b=0", color="black") +
  annotate(geom="text", x=1.45, y=1.9, label="wz+b=1", color="green") +
  annotate(geom="text", x=1.35, y=2, label="wz+b=-1", color="violet") 
  
ggplot(demonoseparable, aes(Antena, Pata)) + 
  geom_point(aes(colour=Grupo)) +
  xlab('Variable 1') +
  ylab('Varable 2')

ggplot(demoenvolvente, aes(x, y)) + 
  geom_point(aes(colour=Grupo)) +
  stat_ellipse(aes(x=x, y=y, color=Grupo), type = "norm") +
  xlab('Variable 1') +
  ylab('Varable 2') +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

##################################################################
# Mapeo

plot.original=ggplot(mpg, aes(displ,hwy)) +
  geom_point(colour=class) +
  geom_smooth(method=loess, se=FALSE) +
  xlab('desplazamiento en litros') +
  ylab('millas por galón')
# Realiza una partición de clases con función de separación

class1=1*(mpg$hwy>(mpg$displ-5)^2+19)+1
class2=1*(mpg$hwy>(-20/7)*(mpg$displ)+33.5)+1
# Se definen nuevas clases

plot.1=ggplot(mpg, aes(displ,hwy)) +
  geom_point(colour=class1) +
  geom_smooth(method=lm, se=FALSE)+
  xlab('desplazamiento en litros') +
  ylab('millas por galón')
# Realiza una partición de clases con función de separación

plot.2=ggplot(mpg, aes(displ, hwy)) +
  geom_point(colour=class2) +
  geom_smooth(method = lm, se = FALSE)+
  xlab('desplazamiento en litros') +
  ylab('millas por galón')
# Realiza una partición de clases con función de separación

ggarrange(plot.original,plot.1,plot.2,nrow=1,ncol=3)
# Produce un gráfico en simultáneo

