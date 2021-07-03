library(ggplot2)

y1= function(t) dchisq(t,1)
y2= function(t) dchisq(t,2)
y3= function(t) dchisq(t,3)
y4= function(t) dchisq(t,4)
y5= function(t) dchisq(t,5)

ggplot(data.frame(t=c(0, 10)), aes(t)) + 
  stat_function(fun=y1, geom="line", aes(colour="k=1"), size=0.5) +
  stat_function(fun=y2, geom="line", aes(colour="k=2"), size=0.5) +
  stat_function(fun=y3, geom="line", aes(colour="k=3"), size=0.5) +
  stat_function(fun=y4, geom="line", aes(colour="k=4"), size=0.5) +
  stat_function(fun=y5, geom="line", aes(colour="k=5"), size=0.5) +
  xlab("") +
  ylab("") +
  labs(colour='g.l.') 

# Para ejemplo
ggplot(data.frame(t=c(0, 8)), aes(t)) +
  stat_function(fun=y1, geom="line", size=0.5, col="royalblue")+
  stat_function(fun=y1, xlim = c(3.841, 8),
                geom = "area", fill = "indianred") +
  xlab("Variable") +
  ylab("Densidad") +
  ylim(0,0.5)
  



