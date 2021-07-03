library(readxl) # Permite leer archivos xlsx
library(FactoMineR) # Paquete con métodos de análisis exploratorio de datos
library(factoextra) # Paquete para análisis multivariado de datos

personas=read_excel("C:/.../personas.xlsx")
# Importa la base con la cual se va a trabajar

base=data.frame(personas)
personas.acm=MCA(base[2:5],quali.sup=1, graph=F)
# Realiza el analisis de correspondencias múltiple

# las variables deben ser introducidas como factores}

fviz_contrib(personas.acm, choice="var", axes=1,
             fill="royalblue", color = "black") +
  theme_gray() +
  xlab('') +
  ylab('Contribuciones (%)') +
  ggtitle('') 
# Grafica las contribuciones de las variables

fviz_contrib(personas.acm, choice="ind", axes=1, top=5,
             fill="royalblue", color = "black") +
  theme_gray() +
  xlab('') +
  ylab('Contribuciones (%)') +
  ggtitle('') 
# Grafica las contribuciones de los individuos

fviz_mca_var(personas.acm, repel = TRUE, col.var="royalblue") +
  theme_gray() +
  xlab('Dimensión 1 (31.6%)') +
  ylab('Dimensión 2 (22.1%)') +
  ggtitle('') 
# Realiza el biplot simétrico

fviz_mca_ind(personas.acm, habillage=factor(personas$Género),
             addEllipses=TRUE, repel=TRUE, legend.title = "Género") +
  theme_gray() +
  xlab('Dimensión 1 (31.6%)') +
  ylab('Dimensión 2 (22.1%)') +
  ggtitle('') +
  scale_color_brewer(palette="Paired") 
# Realiza un agrupamiento por género
  

fviz_mca_ind(personas.acm, habillage=factor(personas$Estado),
             addEllipses=TRUE, repel=TRUE, legend.title = "Estado civil") +
  theme_gray() +
  xlab('Dimensión 1 (31.6%)') +
  ylab('Dimensión 2 (22.1%)') +
  ggtitle('') +
  scale_color_brewer(palette="Paired") 
# Realiza un agrupamiento por estado civil
                                            