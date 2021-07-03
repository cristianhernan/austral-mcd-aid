gar.no=c(258,	280) # Carga de datos
gar.si=c(184,	719)

mat=rbind(gar.no,gar.si) # Combina datos
colnames(mat)=c("No considera consumo", "Considera consumo")
# Pone nombre a las columnas
rownames(mat)=c("No considera garantía", "Considera garantía")
# Pone nombre a las filas

mosaicplot(mat,col=c("skyblue","royalblue"), cex.axis=0.8, main="")
# Produce un diagrama de mosaicos
