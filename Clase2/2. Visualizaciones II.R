#limpio la memoria
rm( list=ls() )
gc()


#install.packages("sf") #Lectura y escritura de datos especiales mas nueva


#https://www.indec.gob.ar/indec/web/Institucional-Indec-Codgeo

options(scipen = 999) ### turn off scientific notation


# Informacion geografica y mapas en R

setwd("C:/Users/valer/Documents/Maestria-Profesora/AID/2021/Clase II/Practica")
library(tidyverse)
library(sf)


# Importarlos con la libreria sf (simple features) - NUEVA LIBRERIA

Cbuenos_aires <- st_read("Datos_geo/cabaxrdatos.shp")
Centros_salud<- st_read("Datos_geo/centros-de-salud/centros-de-salud-y-accion-comunitaria.shp")
### GRAFICOS CON GGPLOT2

# grafiquemos este archivo


ggplot() + 
  geom_sf(data = Cbuenos_aires)



# cambiamos color

ggplot() + 
  geom_sf(data = Cbuenos_aires,color="red")

#Si tenemos alguna variable que mide algo por radio

ggplot() + 
  geom_sf(data = Cbuenos_aires,aes(fill=as.numeric(TOT_POB)))

# cambiar paleta de colores

ggplot() + 
  geom_sf(data = Cbuenos_aires,aes(fill=as.numeric(TOT_POB)),color = NA)+
  scale_fill_gradient(low = "#FF0000", high = "#132B43") 


ggplot() + 
  geom_sf(data = Cbuenos_aires, aes(fill=TOT_POB/HOGARES),color = NA)+
  scale_fill_gradient(low = "#56B1F7", high = "#132B43") 
  
#por comuna




Cbuenos_aires<-Cbuenos_aires%>%
  mutate(Comuna=paste("Comuna: ",substr(DEPTO,2,3),sep=""))
ggplot() + 
  geom_sf(data = Cbuenos_aires, aes(fill=Comuna), color=NA)


datos_comuna<-Cbuenos_aires%>%
  group_by(Comuna)%>%
  summarise(Poblacion=sum(TOT_POB,na.rm = TRUE),
            Hogares=sum(HOGARES, na.rm = TRUE),
            km2    =sum(AREA*0.000001,na.rm = TRUE))

ggplot() + 
  geom_sf(data = datos_comuna)

ggplot() + 
  geom_sf(data = datos_comuna, aes(fill=Poblacion),color = NA)+
  scale_fill_gradient(low = "#56B1F7", high = "#132B43") 

# palermo es la comuna con mas hogares con menos personas
ggplot() + 
  geom_sf(data = datos_comuna, aes(fill=Poblacion/Hogares),color = NA)+
  scale_fill_gradient(low = "#56B1F7", high = "#132B43") 

# densidad de poblacion

ggplot() + 
  geom_sf(data = datos_comuna, aes(fill=Poblacion/km2),color = NA)+
  #scale_fill_gradient(low = "#56B1F7", high = "#132B43") +
  xlab("Longitud") + ylab("Latitud")+
  ggtitle("Mapa de las comunas de la Ciudad Autónoma de Buenos Aires") +
  scale_fill_gradientn(colours = terrain.colors(6))
  

ggplot() + 
  geom_sf(data = datos_comuna, aes(fill=Poblacion/Hogares),color = NA)+
  scale_fill_gradient(low = "#3b83bd", high = "#56B1F7") +
  geom_sf(data=Centros_salud)


ggplot() + 
  geom_sf(data = datos_comuna, aes(fill=Poblacion/Hogares),color = NA)+
  scale_fill_gradient(low = "#8B0000", high = "#de9510") +
  geom_sf(data=Centros_salud)


