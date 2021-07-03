library(ggplot2) # Paquete para confeccionar dibujos

# Carga de datos
a=data.frame(group = "Simétrica", value = c(0,2,3,4,3,2,0))
b=data.frame(group = "Asimétrica a derecha", value = c(3,5,8,4,3,2,1))
c=data.frame(group = "Asimétrica a izquierda", value = c(4,5,3,1,2,0,3))

plot.data=rbind(a, b, c) # Junta filas

ggplot(plot.data, aes(x=group, y=value, fill=group)) +  
  geom_boxplot() + 
  xlab("") +
  ylab("Observaciones") +
  scale_fill_brewer(palette="Pastel1") +
  labs(fill='Tipo') 
# Produce boxplots
