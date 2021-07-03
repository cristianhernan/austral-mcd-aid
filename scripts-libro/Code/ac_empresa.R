library(readxl) # Permite leer archivos xlsx
library(FactoMineR) # Paquete con métodos de análisis exploratorio de datos
library(factoextra) # Paquete para análisis multivariado de datos
library(ade4) # Paquete con herramientas para análisis multivariado de datos
library(anacor)	# Paquete para análisis de correspondencias simple y canónico

empresa=read_excel("C:/.../empresa.xlsx")
# Importa la base con la cual se va a trabajar

Género=factor(empresa$Género)
Antigüedad=factor(empresa$Antigüedad)
Ingresos=factor(empresa$Ingresos)
Categoría=factor(empresa$Categoría)
base=data.frame(Género,Ingresos,Categoría)
# Armamos la base de datos con las variables como factores

empresa.acm=MCA(base, quali.sup=1, graph=F)
# Realiza el analisis de correspondencias múltiple

fviz_contrib(empresa.acm, choice="var", axes=1,
             fill="royalblue", color = "black") +
  theme_gray() +
  xlab('') +
  ylab('Contribuciones (%)') +
  ggtitle('') 
# Grafica las contribuciones de las variables

fviz_contrib(empresa.acm, choice="ind", axes=1, top=5,
             fill="royalblue", color = "black") +
  theme_gray() +
  xlab('') +
  ylab('Contribuciones (%)') +
  ggtitle('') 
# Grafica las contribuciones de los individuos

fviz_mca_var(empresa.acm, repel = TRUE, col.var="royalblue") +
  theme_gray() +
  xlab('Dimensión 1 (38.9%)') +
  ylab('Dimensión 2 (33.3%)') +
  ggtitle('') 
# Realiza el biplot simétrico

fviz_mca_ind(empresa.acm, habillage=Género, addEllipses=TRUE, 
             repel=TRUE, legend.title = "Género") +
  theme_gray() +
  xlab('Dimensión 1 (38.9%)') +
  ylab('Dimensión 2 (33.3%)') +
  ggtitle('') +
  scale_color_brewer(palette="Paired") 
# Realiza un agrupamiento por género

acm.disjonctif(base)
# Calcula la matriz disyuntiva
burtTable(base)
# Calcula la matriz de Burt

acm.empresa=dudi.acm(base, scannf = FALSE)
summary(acm.empresa)
# Calcula las inercias
round(acm.empresa$c1,3)
# Calcula las coordenadas para representar 

