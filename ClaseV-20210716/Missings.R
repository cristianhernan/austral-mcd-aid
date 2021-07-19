
#install.packages("Amelia", dependencies = TRUE)
#install.packages("DMwR", dependencies = TRUE)
#install.packages("mice")
#install.packages("VIM")
#install.packages("questionr")
#install.packages("frequency")
# Missings

library(readxl) # leer archivos excel
library(dplyr) #cuartiles
library(ggplot2) 
library(ggExtra) #mejorar grafico
library(nortest) #normalidad
library(expss) 
library(TMB)
library(sjPlot)
library(gplots)
library(Amelia) #gr?fico missmap
library(DMwR) # imputar datos
library(Hmisc) # imputar datos
library(mice)
library(VIM) # paquete de imputacion
library(questionr) # para usar freq.na
library(dplyr)
library(frequency)
library(grid)
library(gridExtra)



#setear el directorio de trabajo  

#LA VARIABLE OBJETIVO NUNCA SE INPUTA PQ SE ENSUCIA DE VALORES QUIZAS SESGADOS


setwd("C:\\Users\\VEIGC901\\Google Drive\\Maestría\\2021\\Materias\\AID\\ClaseV-20210716")


# importar el archivo

Bweight <- read_excel("bweight_missing.xls" )
summary(Bweight)


Bweight <-mutate(Bweight, boy = as.factor(boy),
                          married = as.factor(married),
                          smoke = as.factor(smoke),
                          ed = as.factor(ed))

summary(Bweight)

# chequeemos smoke, hay un 9, pero como tiene un valor no lo toma como missing.
# vamos a convertirlo en missing

Bweight <- mutate(Bweight, smoke = as.factor(ifelse(smoke == "9", NA , smoke)))
summary(Bweight$smoke)

freq.na(Bweight)


# patron de missings:  Paquete Mice

md.pattern(Bweight, rotate.names = TRUE)

Bweight_plot <- aggr(Bweight, col=c('navyblue','red'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(Bweight), cex.axis=.7,
                    gap=3, ylab=c("Datos faltantes","Patron"))


# muestra por orden de los registros

missmap(Bweight, main = "Valores Missing vs. Observados")

# ver si encontramos relacion de los missing por alguna variable

marginplot(Bweight[c("weight","mom_age")], col = c("blue","red","green"))

marginplot(Bweight[c("m_wtgain","mom_age")], col = c("blue","red","green"))




#Quedarse solo con los completos
# si usamos en este caso solo los casos completos no seria muy util
Bweight2 <- Bweight[complete.cases(Bweight), ]


# eliminar visita en Bweight
Bweight <- select(Bweight, - visit )

# sustituir por un unico valor esta variable tiene pocos missing (solo 20 de 50000)

Bweight <- mutate(Bweight, wtgain_prom = impute(m_wtgain, mean)) # reemplazo por la media
Bweight <- mutate(Bweight, wtgain_med = impute(m_wtgain, median)) # reemplazo por la mediana
Bweight <- mutate(Bweight, wtgain_fijo = impute(m_wtgain, 2)) # reemplazo por un valor especifico

summary(Bweight)


plot1 <- qplot(Bweight$m_wtgain, xlab="Peso ganado en el embarazo", bins=40)
plot2 <- qplot(Bweight$wtgain_prom, xlab="Peso ganado en el embarazo (promedio)",  bins=40)
plot3 <- qplot(Bweight$wtgain_med, xlab="Peso ganado en el embarazo (mediana)",  bins=40)
plot4 <- qplot(Bweight$wtgain_fijo, xlab="Peso ganado en el embarazo (fijo)",  bins=40)

grid.arrange(plot1, plot2, plot3, plot4, ncol=4)

# sustituir por un unico valor esta variable con 513 missing (1%) 

Bweight <- mutate(Bweight, cigspe_prom = impute(cigsper, mean, bins=40)) # reemplazo por la media
Bweight <- mutate(Bweight, cigspe_med = impute(cigsper, median, bins=40)) # reemplazo por la mediana
Bweight <- mutate(Bweight, cigspe_fijo = impute(cigsper, 10, bins=40)) # reemplazo por un valor especifico

summary(Bweight)

# ver histograma por pares (queremos 1 fila 4 columnas y los margenes)
par(  mfrow=c(1,4),  mar=c(5,4,1,0))

hist(Bweight$cigsper, breaks=40 , xlim=c(0,60) , col=rgb(1,0,0,0.5) , xlab="Cigarrillos por dia" ,  main="" )
hist(Bweight$cigspe_prom, breaks=40 , xlim=c(0,60) , col=rgb(0,0,1,0.5) , xlab="Cigarrillos por dia (promedio)" , ylab="" , main="")
hist(Bweight$cigspe_med, breaks=40 , xlim=c(0,60) , col=rgb(1,0,0,0.5) , xlab="Cigarrillos por dia (mediana)" , ylab="" , main="")
hist(Bweight$cigspe_fijo, breaks=40 , xlim=c(0,60) , col=rgb(0,0,1,0.5) , xlab="Cigarrillos por dia (fijo)" , ylab="" , main="")


#Una posible forma para imputar la variable "cigarrillos por dia" con la moda en R ser?a:
summary(Bweight$cigsper) 
 
getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }

Bweight <-  mutate(Bweight, cigsper2 = ifelse(is.na(cigsper),getmode(Bweight$cigsper),cigsper))
                   
summary(Bweight$cigsper2) 

# imputar black
summary(Bweight$black) 
# primero paso los NA como una nueva categoria para ver si la  distribucion es aleatoria,  en este caso 
#le pondremos valor 2
Bweight <- mutate(Bweight, black_NA = ifelse(is.na(black), 2 , black))

summary(Bweight$black_NA) 

# Primero ver como se distribuyen los back entre los pesos


ggplot(Bweight, aes(x=as.factor(black_NA), y=weight))+
  geom_boxplot()

# imputacion por regresion peso subido de las mujeres 

freq.na(Bweight$m_wtgain)

#ordeno la base para que me queden los missing abajo 

Bweight <- arrange(Bweight, m_wtgain)

Bweight_imp <- lm( m_wtgain ~ mom_age + cigsper , data = Bweight[1: 49980,])
summary(Bweight_imp)

Bweight$imputado_g = 0.99667 -0.13609*Bweight$mom_age -0.14417*Bweight$cigsper 
summary(Bweight)

Bweight <- mutate(Bweight, m_wtgain_imp = ifelse(is.na(m_wtgain), imputado_g , m_wtgain))
summary(Bweight$m_wtgain_imp)


# imputar por el vecino m?s proximo


#creo un vector que defina solo las columnas de variables numericas

idx <- c(1,5,7,8)

# creo un subconjunto solo con las variables numericas

n_Bweight <- subset(Bweight, select = idx )

# debido a que una variable tiene un alto % de missing 
# la eliminamos para pensar en un futuro modelo


# k cantidad de vecinos cercanos para usar
# trace para que me muestre en la consola lo que va haciendo

?kNN

freq.na(n_Bweight)
n_Bweight3 <- kNN(n_Bweight, k = 10, trace = TRUE, imp_suffix ="imput")

