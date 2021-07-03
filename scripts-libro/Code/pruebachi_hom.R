M=as.table(rbind(c(12, 88), c(25, 25)))
# Guarda los datos
dimnames(M)=list(Fumador=c('SI', 'NO'), Enfermedad=c("Padece", "No Padece"))
# Establece las poblaciones (filas) y las categorías (columnas) de estudio

Xsq=chisq.test(M) # Realiza el test Chi cuadrado
Xsq$expected # Calcula las frecuencias esperadas   


