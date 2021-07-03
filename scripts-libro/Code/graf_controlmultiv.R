library(MASS) # Paquete de funciones y datos

dat=mvrnorm(n=60,c(10,5), cbind(c(0.7,0.5),c(0.5,0.4)), tol=1e-6, 
            empirical=FALSE, EISPACK=FALSE)
# Genera los datos
datos=data.frame(dat)
# Arregla los datos

ggplot(datos, aes(x=X1,y=X2)) +
  geom_point(colour="royalblue") +
  geom_point(aes(x=11.6,y=3.3), colour="indianred3") +
  stat_ellipse(aes(x=X1, y=X2), colour="orchid3", type="norm") +
  geom_hline(yintercept=3, linetype="dashed", colour="forestgreen") +
  geom_hline(yintercept=7, linetype="dashed", colour="forestgreen") +
  geom_vline(xintercept=8, linetype="dashed", colour="forestgreen") +
  geom_vline(xintercept=12, linetype="dashed", colour="forestgreen") +
  xlab("") +
  ylab("")
# Produce un diagrama
  
  

