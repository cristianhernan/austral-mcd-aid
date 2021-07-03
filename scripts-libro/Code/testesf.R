library(readxl) # Permite leer archivos xlsx

nadadores=read_excel("C:/.../nadadores.xlsx")
# Importa la base con la cual se va a trabajar

pval=0 ; estad=0; gl=0 # Inicializa las variables
autoval=prcomp(nadadores, center = TRUE, scale. = TRUE)[[1]]
# Guarda los autovalores

p=4; n=14 # Asigna valores a los parámetros
for(m in 1:p){
  r=m+1; 	u=p-m
  estad[m]=n-(1-(2*p+11)/6)*(u*log(mean(autoval[r:4]))-sum(autoval[r:4])) 
  # Calcula el estadístico de contraste de cada paso
  gl[m]=(p-1)*p/2 # Calcula los grados de libertad
  pval[m]=1-pchisq(estad[m],gl[m])} # Calcula el p-valor de cada contraste
  
pval # Muestra los p-valores obtenidos

####


     