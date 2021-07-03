library(readxl) # Permite leer archivos xlsx
library(magrittr) # Proporciona un mecanismo para encadenar comandos
library(tibble) # Proporciona un mejor formato para los datos
library(dendextend) # Ofrece un conjunto de funciones para dendogramas


futbol=read_excel("C:/.../futbol.xlsx")
# Importa la base con la cual se va a trabajar
futbol=as.data.frame(futbol) # Arregla los datos

futnum=futbol[,2:11] # Selecciona las variables numéricas
fut=na.omit(futnum) # Elimina los registros con datos faltantes

fut %>% scale %>% dist() %>% hclust(method="ward.D") %>% as.dendrogram() -> dend
# Aplica el criterio de Ward a las variables estandarizadas

par(mar=c(6,1,0.1,0.1)) # Establece márgenes

dend %>% 
  set("branches_k_color", value=c("blue","red","purple","darkgreen"), k=4) %>%
  # Personaliza las ramas
  set("labels_col", value=c("blue","red","purple","darkgreen"), k=4) %>% set("labels_cex", 0.6) %>%
  set("labels", futbol[,1]) %>%
  # Personaliza las etiquetas
  plot(axes=FALSE)
  # Produce un dendograma personalizado






