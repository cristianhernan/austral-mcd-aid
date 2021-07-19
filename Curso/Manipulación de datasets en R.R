
#limpio la memoria
rm( list=ls() )
gc()

#install.packages(tidyverse)
#install.packages(gtools)

###############################
#Manipular datasets en R
###############################



setwd("C:/Users/valer/Documents/Maestria-Profesora/AID/2021/ClaseI/Practica")

getwd() #chequeo en que carpeta estoy

library(tidyverse)
library(gtools)
library(summarytools) # para el uso de estadisticas descriptivas

###############################
#Unir dataframes en R
###############################



# UNIR DATAFRAMES 

#Agregar filas

#igual numero de columnas



Proveedor  <-  data.frame(ProveedorId = c(1:4), 
                          RazonSocial = c("SAS", "SPSS", "Microsoft", "Apple")                          )


Proveedor2  <-  data.frame(ProveedorId = c(5:7), 
                           RazonSocial = c("R", "Stata", "Python"))

Proveedorf <- rbind(Proveedor, Proveedor2)


# distinta cantidad de columnas

Proveedor_3c  <-  data.frame(ProveedorId = c(1:4), 
                          RazonSocial = c("SAS", "SPSS", "Microsoft", "Apple"),
                          Tipo = c("Pago", "Pago","Pago","Pago" ))


Proveedor_2c  <-  data.frame(ProveedorId = c(5:7), 
                           RazonSocial = c("R", "Stata", "Python"))

Proveedorf2 <- smartbind(Proveedor_3c, Proveedor_2c)



#Agregar columnas

Proveedoreg  <-  data.frame(Region = c("CABA", "CABA", "Santa Fe", "C?rdoba","","San Luis",""))


Proveedorc <- cbind(Proveedorf, Proveedoreg)



# UNIR DATAFRAMES CON al menos una COLUMNA EN COMUN
# Como ejemplo crearemos 2 dataframes

clientes = data.frame(ClienteId = c(1:4), 
                      RazonSocial = c("Cliente 1", "Cliente 2", "Cliente 3", "Cliente 4"),
                      Ciudad = c("CABA", "CABA", "Santa Fe",""))
clientes
View(clientes)
ventas = data.frame(ClienteId = c(1, 2, 3, 5, 6), 
                    Monto = c(110, 50, 60, 90, 89))
ventas

#Hay que notar que:

#Puede haber clientes sin ventas, por ejemplo Cliente 4
#A modo de ejemplo también tenemos ventas que no pertenecen a ningún cliente, 
#por ejemplo el registro # 4 que pertenecer??a a un Cliente 5 inexistente

# INNER JOIN
# Muestra solo los registros coincidentes entre ambos data.frame por by="ClienteId"

Union_coincidentes <- merge(x = clientes, y = ventas, by = "ClienteId")
Union_coincidentes 

#Notar que Cliente 4 no tiene ventas por eso no aparece, 
#y obviamente las ventas del cliente inexistente 5 tampoco aparecer?n


# LEFT JOIN
# Un cl?sico left join donde se muestra todos los registros del data.frame izquierdo (x) 
# y solo los coincidentes por ClienteId de la tabla derecha (y)

Union_izquierda <-merge(x = clientes, y = ventas, by = "ClienteId", all.x = TRUE)
Union_izquierda

# RIGHT JOIN
# En este caso mostraremos todos los registros del data.frame derecho (y = ventas) y solo los coincidentes del izquierdo (x = clientes) 
# y tambi?n reemplazamos los valores NA de las columnas que correspondan.

Union_derecha <-merge(x = clientes, y = ventas, by = "ClienteId", all.y = TRUE)
Union_derecha

# OUTER JOIN
# Al contrario del inner, el outer va a combinar todos los registros de ambos dataframes, 
# coincidan por ClienteId o no:

Union_todos <- merge(x = clientes, y = ventas, by = "ClienteId", all = TRUE)
Union_todos

#tambien puede pasar que el campo por el que se quiere unir no tiene exactamente el mismo nombre

ventas_2 = data.frame(Id = c(1, 2, 3, 5), 
                      Monto = c(30, 60, 50, 80))
ventas_2

Union_coincidentes2 <- merge(x = clientes, y = ventas_2, by.x = "ClienteId", by.y = "Id")
Union_coincidentes2 


##### REESTRUCTURAR BASES DE DATOS#######

Alumnos =data.frame( 
  Nombre=c("Juan","Clara","David","Mar?a","Pedro"),
  Nota_1=c(90,100,80,75,60),
  Nota_2=c(40,56,78,90,76),
  Nota_3=c(65,86,58,96,89))

View(Alumnos)
Alumnos_est  <- gather(Alumnos, Nota_1,Nota_2,Nota_3, key="N_Nota", value="Nota")


Alumnos2 <- spread(Alumnos_est, N_Nota, Nota)




#Vamos a trabajar con el archivo customer usado para importar un archivo

customer <- read.csv("Customer.csv")
summary(customer)
customer2 <- read.csv("Customer.csv", stringsAsFactors=TRUE) #si no es factor, no la toma como variabla cualitativa
summary(customer2)

# para ver estadisticas descriptivas de cada variable

summary(customer2)
summary(customer2$Age)
str(customer2)

library(dplyr)
# Filtrar la base para los pasajeros con edad >= 40 a?os

customer_fil <- filter(customer, Age >= 40)
customer_fil2 <- filter(customer, Age < 40 & Region == "Central")
customer_fil3 <- filter(customer, Age < 40 | Region == "Central")

# Seleccionar algunas columnas de un dataframe

customer_final <- select(customer, -(State:Region))
customer_final2 <- select(customer, State:Region)

#comando pipe %>%
# La sintaxis the pipe, permite expresar de forma clara una secuencia de m?ltiples operaciones. 
# Es una sintaxis en cadena, de forma que el operador %> % coge el output ('la salida') de una sentencia de c?digo y la convierte en el input ('el argumento') de una nueva sentencia.

customer_final3 <- customer%>% select(1:3)
customer_final4 <- customer%>% select(1, 3)

#ordenar registros

customer_orden <- arrange(customer,	Region, Age)

customer_orden_desc <- arrange(customer,	Region, desc(Age))

# Crear variables

customer <-mutate(customer, Age_cat = ifelse(Age > 60, "Old", "Young"))
customer <- mutate(customer, Age_month = Age*12)

#Frecuencia

table(customer$Age_cat)

#Proporci?n
prop.table(table(customer$Age_cat))

# Tabla de contingencia

table(customer$Age_cat,customer$Region)



# Agrupar datos


sum_Age <-  summarise(customer, media_Age = mean(Age,na.rm=TRUE))
sum_Age

aggregate(Age~Region, FUN = mean, customer)

Age_media <-  aggregate(Age~Region, FUN = mean, customer)
Age_media

#comando pipe %>%
#Atajo para el comando pipe en Windows Ctrl + Shift + m


media_edad_reg <- customer %>%
                    group_by(as.factor(Region)) %>%
                    summarise(media_edad = mean(Age, na.rm=TRUE))
                
media_edad_reg

#EXPORT
write.csv(media_edad_reg, "media_edad_reg.csv")

# reportes desciptivos

freq(customer$Region, plain.ascii = FALSE, style = "rmarkdown")

descr(customer)

view(dfSummary(customer))
