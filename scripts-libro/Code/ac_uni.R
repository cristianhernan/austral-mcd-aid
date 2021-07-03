library(ggplot2) # Paquete para confeccionar dibujos
library(tcltk2) # Paquete que permite hacer caras de Chernoff
library(aplpack) # Paquete que permite hacer caras de Chernoff
library(scatterplot3d) # Paquete para generar gráficos en 3D
library(FactoMineR) # Paquete con métodos de análsis exploratorio de datos
library(factoextra) # Paquete para análisis multivariado de datos

# Cargamos los datos
Universidad=rep(c("UTN","UNC","UNL","UNS"),3)
actividad=c(rep("Docencia",4), rep("Investigación",4), rep("Extensión",4))
prop=c(0.33,0.07,0.14,0.08,0.06,0.2,0.86,0.08,0.61,0.73,0,0.83)
viajes=data.frame(cbind(Universidad, actividad, prop))

ggplot(data=viajes, aes(x=actividad, y=prop, group=Universidad, color=Universidad)) +
  geom_line(linetype="dashed")+
  geom_point(color="royalblue", size=1)+
  xlab("Tipo de actividad") +
  ylab("Proporción") 

universidades <- read_excel("C:/Users/Usuario/Dropbox/Libro Análisis de datos/Data/universidades.xlsx")
faces(universidades[,2:4], nrow.plot=1, ncol.plot=4, face.type=1,
      labels=universidades$Universidad) 
  
with(universidades, {
  s3d <- scatterplot3d(Docencia, Investigación, Extensión,        
                       color="royalblue", pch=16, box=FALSE, angle=25,
                       type="h", xlab="Docencia", ylab="Investigación",
                       zlab="Extensión")
  s3d.coords <- s3d$xyz.convert(Docencia, Investigación, Extensión) 
  text(s3d.coords$x, s3d.coords$y,             
       labels=universidades$Universidad,               
       cex=.6, pos=4)  
  })
# Realiza un diagrama de dispersión en 3D

with(universidades, {
  s3d <- scatterplot3d(Docencia, Investigación, Extensión,        
                       color="royalblue", pch=16, box=FALSE, angle=25)
  s3d.coords <- s3d$xyz.convert(Docencia,Investigación, Extensión) 
  text(s3d.coords$x, s3d.coords$y,             
       labels=universidades$Universidad,               
       cex=.6, pos=4)  
  fit <- lm(Extensión ~ Docencia + Investigación)
  s3d$plane3d(fit, col="indianred") # Agrega un plano
})

# Análisis de correspondencias

# Armamos la base de datos
base=as.matrix(universidades[1:4,2:4])
colnames(base)= c("Docencia", "Investigación", "Extensión")
row.names(base)= c("UTN","UNC","UNL","UNS")

univ.ac=CA(base, graph = FALSE) # Realiza el analisis de correspondencias 
summary(univ.ac) # Muestra el resultado del análisis de correspondencias

fviz_contrib(univ.ac, choice="row", axes=1,
             fill="royalblue", color = "black") +
  theme_gray() +
  theme(axis.text.x = element_text(angle=0)) +
  xlab('Universidad') +
  ylab('Contribuciones (%)') +
  ggtitle('') 
# Grafica las categorías de las filas   

fviz_contrib(univ.ac, choice="col", axes=1,
             fill="royalblue", color = "black") +
  theme_gray() +
  theme(axis.text.x = element_text(angle=0)) +
  xlab('Tipo de actividad') +
  ylab('Contribuciones (%)') +
  ggtitle('') 
# Grafica las categorías de las columnas 

fviz_ca_biplot(univ.ac, repel=TRUE, col.row="royalblue", 
               col.col="indianred") +
  theme_gray() +
  xlab('Dimensión 1 (86.7%)') +
  ylab('Dimensión 2 (13.3%)') +
  ggtitle('') 
# Realiza el biplot simétrico

