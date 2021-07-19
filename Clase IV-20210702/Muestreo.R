#limpio la memoria
rm( list=ls() )
gc()


#install.packages ("TeachingSampling")
#install.packages("sampling")
library(readxl) # leer archivos excel
library(dplyr) # manipular los datos
library(csv)  # leer archivos csv
library(expss) # tabla cruzada
library(WriteXLS) # importar a excel
library(sampling)
library(TeachingSampling)
library(oddsratio)


#Muestreo

setwd("C:/Users/valer/Documents/Maestria-Profesora/AID/2021/Clase IV")


bweight <- read_excel("bweight.xls")
dim(bweight)
summary(bweight)
bweight <-mutate(bweight, smoke = as.factor(smoke))

#Muestreo simple al azar sin reemplazo (sin semilla)
# si quiero con reeemplazo pongo "replace=TRUE"

# Defino el tamaño que deseo

# crea un vector con los numeros de registros que se seleccionaron al azar (sin reposicion).

bweight_3000 <- sample(1:nrow(bweight),size=3000,replace=FALSE)
bweight_3000

#Asignar los elementos de la muestra al data frame de datos
bweight_muestra <- bweight[bweight_3000, ]
summary(bweight_muestra$weight)



#Muestreo simple al azar sin reemplazo (con semilla) 

set.seed(12356)
weight_3000_s <- sample(1:nrow(bweight),size=3000,replace=FALSE)
weight_3000_s

#Asignar los elementos de la muestra al data frame de datos
weight_muestra_s <- bweight[weight_3000_s, ]

head(weight_muestra_s)

#Muestreo simple al azar con reemplazo (sin semilla) 

# crea un vector con los numeros de registros que se seleccionaron al azar. (con reposicion)

bweight_3000_r <- sample(1:nrow(bweight),size=3000,replace=TRUE)
bweight_3000_r

#Asignar los elementos de la muestra al data frame de datos
bweight_muestra_r <- bweight[bweight_3000_r, ]

head(bweight_muestra_r)

#Muestreo simple al azar con reemplazo (con semilla) 

set.seed(12356)
weight_3000_rs <- sample(1:nrow(bweight),size=3000,replace=TRUE)
weight_3000_rs

#Asignar los elementos de la muestra al data frame de datos
weight_muestra_rs <- bweight[weight_3000_rs, ]

head(weight_muestra_rs)


#Muestreo sistemático
#k = N/n =16.6

systematic <- function(n, N, initial=F){
  k <- floor(N/n)
  if(initial==F){
    initial <- sample(1:k,1)}
  cat("Interval=", k, " Starting value=", initial, "\n")
  # Put the origin in the value 'initial'
  shift <- (1:N) - initial
  # I search numbers who are multiple of number k
  # (equivalent to find the rest of a%%b=0)
  guy <- (1:N)[(shift %% k) == 0]
  return(guy)
}

systematic <- systematic(3000,50000, 7)
systematic

weight_muestra_sys <- bweight[systematic, ]

# Muestreo estratificado simple sin reposicion proporcional
bweight <-mutate(bweight, smoke = as.factor(smoke))
summary(bweight$smoke)
bweight <- mutate(bweight, smoke = ifelse((smoke == '9' ), '1' , smoke))

estratos <- strata(bweight, stratanames = c("smoke"), size = c(87,13), method = "srswor" )
bw.muestreado <- getdata( bweight, estratos )




