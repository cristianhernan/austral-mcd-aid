B=as.table(rbind(c(1,4), c(7,2)))
# Guarda los datos
dimnames(B)=list(Sexo=c('Mujer','Hombre'), Síntomas=c('Presente','Ausente'))
# Establece las categorías de estudio

Xsq=chisq.test(B) # Realiza el test Chi cuadrado
Xsq$expected # Calcula las frecuencias esperadas 

fisher.test(B) # Realiza el test de Fisher
