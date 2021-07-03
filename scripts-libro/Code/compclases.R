library(ggplot2)

y1=function(x) floor(10*log(x))
y2=function(x) floor(2*sqrt(x))
y3=function(x) floor(1+log(x)/log(2))

ggplot(data.frame(x=c(0, 500)), aes(x)) + 
  stat_function(fun=y1, geom="line", aes(colour="Dixon-Kronmal"), size=0.8) +
  stat_function(fun=y2, geom="line", aes(colour="Velleman"), size=0.8) +
  stat_function(fun=y3, geom="line", aes(colour="Sturges"), size=0.8) +
  xlab("Tamaño de la muestra") +
  ylab("Cantidad de intervalos") +
  labs(colour='Método') 

