library(ggplot2) # Paquete para confeccionar dibujos

x=seq(-15,15,0.1)
p=exp(0.6*x)/(1+exp(0.6*x))
info=data.frame(x,p)

ggplot(info, aes(x,p)) +
  geom_line(color="royalblue", size=1) + 
  xlab(expression(paste(alpha+beta, x))) + ylab("p(x)")  
  
