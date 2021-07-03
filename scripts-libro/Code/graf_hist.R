library(readxl) # Permite leer archivos xlsx

IMCinfantil=read_excel("C:/.../IMCinfantil.xlsx")
# Importa la base con la cual se va a trabajar
attach(IMCinfantil) # Se pone la base en la memoria


hist(PESO, col="paleturquoise3", border="royalblue", breaks=seq(0,85,5),
     density=20, angle=70, ylab="", main="") 
# Produce un histograma

pto.medio=seq(2.5,82.5,5) # Toma los puntos medios de las barras
alt.dens=hist(PESO, breaks=seq(0,85,5), plot=F)$counts 
# Busca la altura de las barras
points(pto.medio, alt.dens, type="l", lwd=2, col="mediumslateblue")
# Agrega el polígono de frecuencias
