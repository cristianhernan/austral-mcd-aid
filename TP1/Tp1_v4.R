#deshabilitar la notacion cientifica
options(scipen=999)
#limpio memoria
rm(list=ls())
gc()
#cargo librerias
library(dplyr)
library(tidyr)
library(summarytools) # para el uso de estadisticas descriptivas
library(grid)
library(data.table)


#cargo los files
# reca_ene_raw <- read.csv("../../datasets/aid/tp1/RECA_CHAN_01_NEW.csv",header=TRUE, sep = ",")
# reca_feb_raw <- read.csv("../../datasets/aid/tp1/RECA_CHAN_02_NEW.csv",header=TRUE, sep = ",")
# reca_mar_raw <- read.csv("../../datasets/aid/tp1/RECA_CHAN_02_NEW.csv",header=TRUE, sep = ",")

#cargo los files parece que esta forma es mas rapida
reca_ene_raw <- fread("../../datasets/aid/tp1/RECA_CHAN_01_NEW.csv")
reca_feb_raw <- fread("../../datasets/aid/tp1/RECA_CHAN_02_NEW.csv")
reca_mar_raw <- fread("../../datasets/aid/tp1/RECA_CHAN_02_NEW.csv")

#uno los dataframes en solo uno
reca_data <-  rbind(reca_ene_raw,reca_feb_raw,reca_mar_raw) 

#elimino dataframes de memoria para limpiar espacio.
rm(reca_ene_raw,reca_feb_raw,reca_mar_raw)
gc()

#filtro aquellos con amount >0
reca_data <- filter(reca_data,PURCHASEAMOUNT > 0)

#identiico aquellos tecnologicos y manuales
reca_data <-mutate(reca_data, CHANNELIDENTIFIER = ifelse(grepl("EMG",CHANNELIDENTIFIER,fixed = TRUE),"Tecno", "Manual"))

#agroup por customerid y channelidentifier y obtengo la cantidad por cliente y canal
reca_data_gc <- reca_data %>%  group_by(CUSTOMERID,CHANNELIDENTIFIER) %>%  
  summarize(cantidad = n())

#agroup por customerid y channelidentifier y obtengo el monto por cliente y canal
reca_data_gs<- reca_data %>%  group_by(CUSTOMERID,CHANNELIDENTIFIER) %>%  
  summarize( monto = sum(PURCHASEAMOUNT))

rm(reca_data)
gc()

#asigno cero a todos los nulls encontrados (de todas formas no hay)
#reca_data_gs[is.na(reca_data_gs)] <- 0
#reca_data_gc[is.na(reca_data_gc)] <- 0

#PUNTO 1******************
#primero paso el las cantidades del channelidentifier de fila a columna
cant_data <- spread(data = reca_data_gc, key = CHANNELIDENTIFIER, value = cantidad)
#le asigno cero a los null que podria haber
cant_data[is.na(cant_data)] <- 0
#renombro las columnas
colnames(cant_data) <- c("ACCS_MTHD_CD","CANT_RMAN","CANT_RTEC")
#creo nueva columnas calculadas
cant_data <- mutate(cant_data,CANT_RECARGAS=CANT_RMAN+CANT_RTEC)

rm(reca_data_gc)
gc()

#luego paso el monto de los channnelidentifier de fila a columna
monto_data <- spread(data = reca_data_gs, key = CHANNELIDENTIFIER, value = monto)
#le asigno cero si hubiera na
monto_data[is.na(monto_data)] <- 0
#renombro las columnas
colnames(monto_data) <- c("ACCS_MTHD_CD","MONTO_TECNO","MONTO_MANUAL")
#creo columnas calculadas
monto_data <- mutate(monto_data,MONTO_TOTAL=MONTO_TECNO+MONTO_MANUAL)

rm(reca_data_gs)
gc()

#hago un join entre ammbos dataframes por la columna accs_mthd_cd
tabla <- merge(x=cant_data,y=monto_data,by="ACCS_MTHD_CD",all.x=TRUE)

#elimino ds temporales de momoria para liberar espacio
rm(monto_data,cant_data)
gc()

#creo las columnas de porcentaje
tabla <- tabla %>% 
          mutate(POR_TECNO = round((CANT_RTEC/CANT_RECARGAS)*100),2) %>% 
          mutate(POR_TECNO_M = round((MONTO_TECNO/MONTO_TOTAL)*100),2)

#creo el dataframe definitivo del punto uno junto a su nueva columna CL Tecno
tabla <- tabla %>% 
  mutate(CL_TECNO = if_else (CANT_RECARGAS >2, 
                              case_when(.$POR_TECNO > 70 ~ "1-Tecno",
                                .$POR_TECNO >= 40  & .$POR_TECNO <=70 ~ "2-Mix4070",
                                .$POR_TECNO > 0 & .$POR_TECNO < 40 ~ "3-MixH40",
                                 TRUE ~ "4-No Tecno"),"99-NOSEGM")) 

#cargo los datos del file de clientes.
reca_clientes <- read.csv("../../datasets/aid/tp1/DNA_03_NEW.csv",header=TRUE, sep = ",")

#filtro solo por los rejointer y aquellos que contengan ACTIVE
reca_clientes <- filter(reca_clientes,BASE_STAT_03  == "REJOINNER" | grepl("ACTIVE",BASE_STAT_03,fixed = TRUE))

#casteo el campo BASE_STAT_03 de char a factor
reca_clientes$BASE_STAT_03 <- as.factor(reca_clientes$BASE_STAT_03)

#reviso si tengo duplicados.
reca_clientes[duplicated(reca_clientes$ACCS_MTHD_CD),]

#chequeo si hay nulls
reca_clientes[is.na(reca_clientes)]

#hago un join con la tabla reca_clientes
Clientes_Mar21 <- left_join(reca_clientes,tabla,by="ACCS_MTHD_CD")

#elimino ds temporales de momoria para liberar espacio
rm(tabla,reca_clientes)
gc()

Clientes_Mar21 <- Clientes_Mar21 %>% select("ACCS_MTHD_CD","BASE_STAT_03","MONTO_TOTAL","MONTO_TECNO","POR_TECNO_M","CANT_RECARGAS","CANT_RTEC","POR_TECNO","CL_TECNO")

write.table(Clientes_Mar21, "../../datasets/aid/tp1/Clientes_Mar21.csv", sep=",", col.names=TRUE)
#paso a factor las cols CL_TECNO y ACCS_MTHD_CD para hacer un summary
Clientes_Mar21$CL_TECNO <- as.factor(Clientes_Mar21$CL_TECNO)
Clientes_Mar21$ACCS_MTHD_CD <- as.factor(Clientes_Mar21$ACCS_MTHD_CD)


#------------FIN PUNTO 1-----------------------------------

#-----------PUNTO 2----------------------------------------
#creo el dataframe usando un group by
Segmentacion_Mar21 <- Clientes_Mar21 %>% group_by(CL_TECNO) %>% 
                      summarise(Monto_total_promedio = round(mean(MONTO_TOTAL),2),
                      Monto_tecno_promedio = round(mean(MONTO_TECNO),2),
                      Cant_recargas_promedio = round(mean(CANT_RECARGAS),2),
                      Cant_rec_tecno_prom = round(mean(CANT_RTEC),2)) 


#guardo el csv del punto2
write.table(Segmentacion_Mar21, "../../datasets/aid/tp1/Segmentacion_Mar21.csv", sep=",", col.names=TRUE)

#este es el dataset del punto 1 pero sin n/a
Cli_mar21_sinNa <- filter(Clientes_Mar21, !is.na(CL_TECNO))

#este es el dataset del punto 2 pero sin n/a
seg_cli_sinNA <- filter(Segmentacion_Mar21, !is.na(CL_TECNO))


#------------FIN PUNTO 2-----------------------------------

#-----------PUNTO 3----------------------------------------
library(ggplot2)
library(plotly)


#TOMO UNA MUESTRA RANDOM DE 10k registros para poder hacer los graficos mas rapidos
set.seed(123)
cli_sample <- Cli_mar21_sinNa[sample(nrow(Cli_mar21_sinNa),10000,replace = FALSE),]

head(cli_sample)

#obtengo un resumen estadistico basico del dataset
summary(cli_sample)

#obtengo una tabla de frecuencias del CL_TECNO
freq(cli_sample$CL_TECNO, plain.ascii = FALSE, style = "rmarkdown")
dfSummary(cli_sample$CL_TECNO)

#--------------------------CONCLUCIONES:-------------------------------------
# 1- vemos que el segmento 4-No Tecno, es "la moda" dado acumula mas del 50% de las obscervacione
#-----------------------------------------------------------------------------

#grafico de barras basico por segmentacion
ggplot(cli_sample, aes(x=CL_TECNO, y=CANT_RECARGAS, fill=CL_TECNO))+
  geom_bar(stat="identity", width=0.4)+
  scale_fill_brewer(palette = "Set1")+
  labs(x="CL_TECNO",  y="RECARGAS",color="SEGMENTO")+
  ggtitle("Cantidad de recargas por segmantacion")

ggplot(cli_sample, aes(x=CL_TECNO, y=MONTO_TOTAL, fill = CL_TECNO ))+
  geom_bar(stat="identity", width=0.4)+
  scale_fill_brewer(palette = "Set1")+
  labs(x="CL_TECNO",  y="MONTO",color="SEGMENTO")+
  ggtitle("Monto por segmantacion")

#--------------------------CONCLUCIONES:-------------------------------------
# 2- tanto la mayoria de lo recaudado asi como tambien la cantidad de recargas
#  pertenecen al segmento "4-No Tecno" es decir, solo recargas manuales,


ggplot(cli_sample, aes(x=CL_TECNO, y=CANT_RECARGAS, fill = CL_TECNO)) + 
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", shape=2, size=3)+
  labs(x="Segmentacon",  y="Recargas ",color="Tipo")

#--------------------------CONCLUCIONES:-------------------------------------
# 3- Notamos outliers en 4 de los 5 segmentos, y un comportamiento similar
#en los segmentos 1, 2 y 4

ggplot(cli_sample, aes(x=MONTO_TOTAL, y=CANT_RECARGAS, color=CL_TECNO)) + 
  geom_point(size=4)
