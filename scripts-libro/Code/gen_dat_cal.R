Sys.setenv(R_ZIPCMD= "C:/Rtools/bin/zip")  

library("openxlsx")

lab1=rnorm(20,4.05,0.25)
lab2=rnorm(20,4,0.5)
lab3=rnorm(20,3.9,0.15)
lab4=rnorm(20,4.1,0.55)
lab5=rnorm(20,3.95,0.3)
lab6=rnorm(20,4,0.4)
lab7=rnorm(20,3.8,0.45)

labo=c(lab1,lab2,lab3,lab4,lab5, lab6,lab7) 
firma=c(rep(1,20),rep(2,20),rep(3,20),rep(4,20),rep(5,20),rep(6,20),rep(7,20))
datos=data.frame(cbind(firma,labo))

colnames(datos)=c("Laboratorio","Calorías")

nombre=paste("C:/Users/Usuario/Dropbox/Libro Análisis de Datos/Data/kcalab.xlsx")

write.xlsx(datos, file=nombre)





