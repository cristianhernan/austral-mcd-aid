#deshabilitar la notacion cientifica
options(scipen=999)
#limpio memoria
rm(list=ls())
gc()
#cargo librerias
library(tidyverse)
library(dplyr)
library(readr)

setwd("C:/Cursos/mcd/austral-mcd-aid/TP1")

getwd() #chequeo en que carpeta estoy

#cargo los files


reca_ene_raw <- read_csv("../../datasets/aid/tp1/RECA_CHAN_01_NEW.csv", 
                col_types = cols(X1 = col_skip(), RUNID = col_skip(), 
                CUSTOMERID = col_number(), PURCHASETIME = col_skip())) %>% 
                filter(PURCHASEAMOUNT >0)

reca_feb_raw <- read_csv("../../datasets/aid/tp1/RECA_CHAN_02_NEW.csv", 
                col_types = cols(X1 = col_skip(), RUNID = col_skip(), 
                CUSTOMERID = col_number(), PURCHASETIME = col_skip())) %>% 
                filter(PURCHASEAMOUNT >0)

reca_mar_raw <- read_csv("../../datasets/aid/tp1/RECA_CHAN_03_NEW.csv", 
                col_types = cols(X1 = col_skip(), RUNID = col_skip(), 
                CUSTOMERID = col_number(), PURCHASETIME = col_skip())) %>% 
                filter(PURCHASEAMOUNT >0)


#uno los dataframes en solo uno
reca_data <-  rbind(rbind(reca_ene_raw,reca_feb_raw),reca_mar_raw)

#elimino dataframes de memoria para limpiar espacio.
rm(reca_ene_raw,reca_feb_raw,reca_mar_raw)

#identiico aquellos tecnologicos y manuales
reca_data <-mutate(reca_data, ISTEC = ifelse(grepl("EMG",CHANNELIDENTIFIER),1, 0))

reca_data <- select(reca_data,-CHANNELIDENTIFIER)
#agroup por customerid y channelidentifier y obtengo la cantidad por cliente y canal

reca_grouped <- reca_data %>%  group_by(CUSTOMERID) %>%  
  summarize(CANT_RECARGAS = n()
           ,MONTO_TOTAL = sum(PURCHASEAMOUNT, TRUE)
           ,CANT_RTEC = sum(ISTEC, TRUE)
           ,MONTO_TECNO = sum(PURCHASEAMOUNT*ISTEC,TRUE))

#rm(reca_data)

#creo las columnas de porcentaje
reca_grouped$POR_TECNO <-  round(reca_grouped$CANT_RTEC/reca_grouped$CANT_RECARGAS*100,0)
reca_grouped$POR_TECNO_M <-  round(reca_grouped$MONTO_TECNO/reca_grouped$MONTO_TOTAL*100,0)

colnames(reca_grouped)[1] <- "ACCS_MTHD_CD"


Clientes_Mar21 <- left_join(reca_clientes,reca_grouped,by="ACCS_MTHD_CD")

summary(reca_grouped)

#head(filter(reca_data,grepl("EMG",CHANNELIDENTIFIER)),50)

#creo  nueva columna CL Tecno
aux_reca <- mutate(reca_grouped,CL_TECNO = 
                            ifelse(CANT_RECARGAS >2, 
                                   case_when((POR_TECNO > 70) ~ "1-Tecno",
                                      ((POR_TECNO >= 40) & (POR_TECNO <=70))~ "2-Mix4070",
                                      ((POR_TECNO > 0) & (POR_TECNO < 40)) ~ "3-MixH40",
                                      TRUE ~ "4-No Tecno")
                             ,"99-NOSEGM")) 

colnames(aux_reca)[1] <- "ACCS_MTHD_CD"

head(filter(aux_reca,POR_TECNO==0))

#cargo los datos del file de clientes
reca_clientes <- read_csv("../../datasets/aid/tp1/DNA_03_NEW.csv", 
                  col_types = cols(X1 = col_skip())) %>% 
                  filter(BASE_STAT_03  == "REJOINNER" | grepl("ACTIVE",BASE_STAT_03,fixed = TRUE))



#hago un join con la tabla reca_clientes
Clientes_Mar21 <- left_join(reca_clientes,aux_reca,by="ACCS_MTHD_CD")

#casteo el campo BASE_STAT_03 de char a factor
Clientes_Mar21$BASE_STAT_03 <- as.factor(Clientes_Mar21$BASE_STAT_03)
Clientes_Mar21$CL_TECNO <- as.factor(Clientes_Mar21$CL_TECNO)

summary(Clientes_Mar21)

rm(aux_reca,reca_clientes)

#reordeno columnas
Clientes_Mar21 <- select(Clientes_Mar21,"ACCS_MTHD_CD","BASE_STAT_03","MONTO_TOTAL","MONTO_TECNO","POR_TECNO_M","CANT_RECARGAS","CANT_RTEC","POR_TECNO","CL_TECNO")

write.table(Clientes_Mar21, "../../datasets/aid/tp1/Clientes_Mar21.csv", sep=",", col.names=TRUE)

#--------------------- --PUNTO 2----------------------------------------
#casteo el campo BASE_STAT_03 de char a factor
Clientes_Mar21$BASE_STAT_03 <- as.factor(Clientes_Mar21$BASE_STAT_03)

Clientes_Mar21$CL_TECNO <- as.factor(Clientes_Mar21$CL_TECNO)

summary(Clientes_Mar21)


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



#casteo el campo BASE_STAT_03 de char a factor
reca_clientes$BASE_STAT_03 <- as.factor(reca_clientes$BASE_STAT_03)

#reviso si tengo duplicados.
reca_clientes[duplicated(reca_clientes$ACCS_MTHD_CD),]

#chequeo si hay nulls
reca_clientes[is.na(reca_clientes)]



#elimino ds temporales de momoria para liberar espacio
rm(tabla,reca_clientes)
gc()

Clientes_Mar21 <- Clientes_Mar21 %>% select("ACCS_MTHD_CD","BASE_STAT_03","MONTO_TOTAL","MONTO_TECNO","POR_TECNO_M","CANT_RECARGAS","CANT_RTEC","POR_TECNO","CL_TECNO")

write.table(Clientes_Mar21, "../../datasets/aid/tp1/Clientes_Mar21.csv", sep=",", col.names=TRUE)
#paso a factor las cols CL_TECNO y ACCS_MTHD_CD para hacer un summary
Clientes_Mar21$CL_TECNO <- as.factor(Clientes_Mar21$CL_TECNO)
Clientes_Mar21$ACCS_MTHD_CD <- as.factor(Clientes_Mar21$ACCS_MTHD_CD)


library(ggplot2)
library(plotly)


#TOMO UNA MUESTRA RANDOM DE 10k registros para poder hacer los graficos mas rapidos
set.seed(123)
cli_sample <- Cli_mar21_sinNa[sample(nrow(Cli_mar21_sinNa),10000,replace = FALSE),]

head(cli_sample)

#obtengo un resumen estadistico basico del dataset
summary(Segmentacion_Mar21)

#MUESTRA.....obtengo una tabla de frecuencias del CL_TECNO
freq(cli_sample$CL_TECNO, plain.ascii = FALSE, style = "rmarkdown")
dfSummary(cli_sample$CL_TECNO)

#obtengo una tabla de frecuencias del CL_TECNO
#freq(Cli_mar21_sinNa$CL_TECNO, plain.ascii = FALSE, style = "rmarkdown")
#dfSummary(Cli_mar21_sinNa$CL_TECNO)

#--------------------------CONCLUCIONES:-------------------------------------
# 1- vemos que el segmento 4-No Tecno, es "la moda" dado acumula mas del 50% de las obscervacione
#-----------------------------------------------------------------------------

# MUESTRA grafico de barras basico por segmentacion
ggplot(cli_sample, aes(x=CL_TECNO, y=CANT_RECARGAS, fill=CL_TECNO))+
  geom_bar(stat="identity", width=0.4)+
  scale_fill_brewer(palette = "Set1")+
  labs(x="CL_TECNO",  y="RECARGAS",color="SEGMENTO")+
  ggtitle("Cantidad de recargas por segmantacion")


#MUESTRA grafico de barras basico por segmentacion
ggplot(cli_sample, aes(x=CL_TECNO, y=MONTO_TOTAL, fill = CL_TECNO ))+
  geom_bar(stat="identity", width=0.4)+
  scale_fill_brewer(palette = "Set1")+
  labs(x="CL_TECNO",  y="MONTO",color="SEGMENTO")+
  ggtitle("Monto por segmantacion")

#--------------------------CONCLUCIONES:-------------------------------------
# 2- tanto la mayoria de lo recaudado asi como tambien la cantidad de recargas
#  pertenecen al segmento "4-No Tecno" es decir, solo recargas manuales,

#MUESTRA
ggplot(cli_sample, aes(x=CL_TECNO, y=CANT_RECARGAS, fill = CL_TECNO)) + 
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", shape=2, size=3)+
  labs(x="Segmentacon",  y="Recargas ",color="Tipo")


