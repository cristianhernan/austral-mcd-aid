library(readxl) # Permite leer archivos xlsx
install_github("vqv/ggbiplot") # Instala paquete desde GitHub
library(ggbiplot) # Paquete para visualización de componentes principales
library(MASS)
library(ggrepel) # Paquete que manipula etiquetas para gráficos

nad=read_excel("C:/.../nadadores.xlsx")
# Importa la base con la cual se va a trabajar
nad.cont=rbind(nad,c(15,18,12,12,10),c(16,8,15,5,11),c(17,10,13,12,8))
# Agrega nuevos datos
nad.cont=nad.cont[,-1]
# Quita una columna

nad.rob.pca1=princomp(nad.cont, cor=TRUE, scores=TRUE,
                      covmat=MASS::cov.mcd(nad.cont))
summary(nad.rob.pca1)
# Análisis de componentes principles aplicando MCD

ggscreeplot(nad.rob.pca1, type = c('pev', 'cev')) +
  xlab('Número de componentes principales') +
  ylab('Proporción de la variabilidad explicada') +
  geom_line(colour='royalblue') +
  geom_point(colour='royalblue')
# Produce un gráfico de sedimentación

ggbiplot(nad.rob.pca1, choices = 1:2) +
  geom_point(colour="royalblue") +
  geom_text_repel(aes(label=1:17)) +
  theme(legend.position="none") +
  xlab("PC1 estandarizada (69.8% de variabilidad explicada)") +
  ylab("PC2 estandarizada (26.6% de variabilidad explicada)") +
  theme(axis.title=element_text(size=8))
# Genera un biplot

################################
# Otras alternativas robustas 
################################

nad.rob.pca2=princomp(nad.cont, cor=TRUE, scores=TRUE,
                      covmat=MASS::cov.rob(nad.cont))
summary(nad.rob.pca2)
# Análisis de componentes principles aplicando el estimador
# resistente de ubicación multivariada y dispersión

nad.rob.pca3=princomp(nad.cont, cor=TRUE, scores=TRUE,
                      covmat=MASS::cov.mve(nad.cont))
summary(nad.rob.pca3)
# Análisis de componentes principles aplicando MVE


