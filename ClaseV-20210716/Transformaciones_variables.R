#limpio la memoria
rm( list=ls() )
gc()



#install.packages ("Hmisc")
#install.packages ("classInt")


library(dplyr) #cuartiles
library(readxl) # leer archivos excel
library(Hmisc)
library(grid)
library(gridExtra)
library(classInt) # intervalos
library(questionr)
#library(dprep) #estandarizacion z score


setwd("C:/Users/valer/Documents/Maestria-Profesora/AID/2021/Clase V")


# Transformaciones de variables

salarios <- read_excel("Salarios.xls" )
summary(salarios)
freq.na(salarios)
head(salarios)
str(salarios)

#Variables cualitativas a factor

salarios <-mutate(salarios, estadocivil = as.factor(estadocivil),
                 ocupacion = as.factor(ocupacion),
                 sexo=as.factor(sexo))
summary(salarios)

# para ver los niveles de las variables cualitativas
levels(salarios$ocupacion)


#Variables cualitativas a numericas (dummies)
 
contrasts(salarios$ocupacion)
contrasts(salarios$sexo) 
contrasts(salarios$estadocivil) 


#Variables cuantitativas

#Discretizar una variable

#Particionamiento según Igual tamaño (cantidad de indivduos) (ejemplo deciles)
pal1 <- c("blue", "red3")

clases <- classIntervals(salarios$ingresos, 10, style="quantile")
names(clases)
clases
 
plot(classIntervals(salarios$ingresos, 10, style="quantile"), pal=pal1)

salarios <- mutate(salarios, cut(salarios$ingresos, breaks = clases$brks, labels =as.character(1:10)))

colnames(salarios)[9]<-"ingres_10"
freq <-freq(salarios$ingres_10)
freq
#Particionamiento según Igual ancho 
clases2 <- classIntervals(salarios$ingresos, 10, style="equal")
plot(classIntervals(salarios$ingresos, 10, style="equal"), pal=pal1)

salarios <- mutate(salarios, cut(salarios$ingresos, breaks = clases2$brks, labels=as.character(1:10)))
colnames(salarios)[10]<-"ingres_igual"
freq_igual <-freq(salarios$ingres_igual)
freq_igual
clases2


#Particion arbitraria
clases3 <- classIntervals(salarios$ingresos, n=4, style="fixed", fixedBreaks=c(0, 75000, 100000, 130000, 150000))
salarios <- mutate(salarios, cut(salarios$ingresos, breaks = clases3$brks, labels =as.character(1:4)))

colnames(salarios)[11]<-"ingres_arbit"
freq_arbit <-freq(salarios$ingres_arbit)
freq_arbit

plot(classIntervals(salarios$ingresos, n=4, style="fixed",
                fixedBreaks=c(0, 75000, 100000, 130000, 150000)), pal=pal1)

#Particion arbitraria

salarios <- mutate(salarios, ingres5 = as.character(ifelse(ingresos > 130000 , ">130000" , 
                                                        ifelse(ingresos > 99999, "100000 a 130000",
                                                               ifelse(ingresos > 74999, "75000 a 100000",
                                                                      ifelse(ingresos > 0, "0 a 75000", ingresos))))))
freq5 <-freq(salarios$ingres5)                                                              


# transformar distribución de la variable
salarios  <- mutate(salarios, ingresos_trans = sqrt(ingresos))

# histograma
plot1 <- qplot(salarios$ingresos, xlab="Ingresos (en pesos)")
plot2 <- qplot(salarios$ingresos_trans, xlab="Ingresos transformados (raíz cuadrada)")

grid.arrange(plot1, plot2, ncol=2)


#Metodos de estandarización 
#Estandarizacion mix-max

salarios  <- mutate(salarios, ingresos_norm = (ingresos-min(ingresos))/(max(ingresos)-min(ingresos)))

# histograma
plot3 <- qplot(salarios$ingresos, xlab="Ingresos (en pesos)")
plot4 <- qplot(salarios$ingresos_norm, xlab="Ingresos estandarizados (mix-max)")

grid.arrange(plot3, plot4, ncol=2)

#Puntuación z

salarios  <- mutate(salarios, ingresos_z = (ingresos-mean(ingresos))/(sd(ingresos)))

plot5 <- qplot(salarios$ingresos, xlab="Ingresos (en pesos)")
plot6 <- qplot(salarios$ingresos_z, xlab="Ingresos estandarizados (puntuación z)")

grid.arrange(plot5, plot6, ncol=2)

# otra forma de estandarizar


salarios <- mutate(salarios, ingresos_z2 =scale(ingresos))

