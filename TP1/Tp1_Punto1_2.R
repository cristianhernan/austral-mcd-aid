#**************TRABAJO PRACTICO N1 ANALISIS INTELIGENTE DEE DATOS**************
#*-----------------------------------------------------------------------------
#-VEIGA CRISTIAN
#------------------------------------------------------------------------------

#deshabilitar la notacion cientifica
options(scipen=999)
#limpio memoria
rm(list=ls())
gc()
#cargo librerias
library(tidyverse)
library(dplyr)
library(readr)
library(data.table)
library(ggplot2)
library(plotly)
library(summarytools)

#cargo los files RECA_CHAN_0x_NEW.csv
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


#*****************************PUNTO 1******************************************
#------------------------------------------------------------------------------

#junto los dataframes en solo uno
reca_data <-  rbind(reca_ene_raw,reca_feb_raw,reca_mar_raw)

#elimino dataframes de memoria para limpiar espacio.
rm(reca_ene_raw,reca_feb_raw,reca_mar_raw)

#identiico aquellos tecnologicos y manuales
reca_data <-mutate(reca_data, ISTEC = 
                      ifelse(grepl("EMG",CHANNELIDENTIFIER,fixed = TRUE),1, 0))

#agrupo por CUSTOMERID y creo los campos de agregacion.
reca_group <- reca_data %>%  group_by(CUSTOMERID) %>%  
  summarize(CANT_RECARGAS = n()
            ,MONTO_TOTAL = sum(PURCHASEAMOUNT)
            ,CANT_RTEC = sum(ISTEC)
            ,MONTO_TECNO = sum(PURCHASEAMOUNT*ISTEC))

#creo las columnas porcentuales
reca_group$POR_TECNO <- round(reca_group$CANT_RTEC/reca_group$CANT_RECARGAS*100,2)
reca_group$POR_TECNO_M <- round(reca_group$MONTO_TECNO/reca_group$MONTO_TOTAL*100,2)

#limpio espacio en memoria
rm(reca_data)

#creo  nueva columna CL Tecno con la segmentacion.
reca_cl <-  mutate(reca_group,CL_TECNO = if_else (CANT_RECARGAS >2, 
                    case_when(POR_TECNO > 70 ~ "1-Tecno",
                    ((POR_TECNO >= 40)  & (POR_TECNO <=70 ))~ "2-Mix4070",
                    ((POR_TECNO) > 0 & (POR_TECNO < 40 ))~ "3-MixH40",
                    TRUE ~ "4-No Tecno"),"99-NOSEGM")) 

#limpio espacio en memoria
rm(reca_group)

#casteo la columna CLTecno a factor.
reca_cl$CL_TECNO <- as.factor(reca_cl$CL_TECNO)

colnames(reca_cl)[1] <- "ACCS_MTHD_CD"

#cargo los datos del file de clientes
reca_clientes <- read_csv("../../datasets/aid/tp1/DNA_03_NEW.csv", 
          col_types = cols(X1 = col_skip())) %>% 
          filter(BASE_STAT_03  == "REJOINNER" | grepl("ACTIVE",BASE_STAT_03,fixed = TRUE))


#hago un left join con el file de clientes.
Clientes_Mar21 <- merge(x = reca_clientes, y =reca_cl,by.x='ACCS_MTHD_CD', by.y ='ACCS_MTHD_CD',all.x=TRUE)
#reordeno columnas
Clientes_Mar21 <- select(Clientes_Mar21,"ACCS_MTHD_CD","BASE_STAT_03","MONTO_TOTAL","MONTO_TECNO","POR_TECNO_M","CANT_RECARGAS","CANT_RTEC","POR_TECNO","CL_TECNO")

#limbio df de memoria.
rm(reca_clientes,reca_cl)

#LUEGO DEL LEFT JOIN ASUMO QUE AQUELLOS CLIENTES SIN RECARGAS SON 99-NOSEGM
Clientes_Mar21$CL_TECNO[is.na(Clientes_Mar21$CL_TECNO)] <- "99-NOSEGM"
Clientes_Mar21[is.na(Clientes_Mar21)] <- 0

#casteo el campo BASE_STAT_03  a factor
Clientes_Mar21$BASE_STAT_03 <- as.factor(Clientes_Mar21$BASE_STAT_03)

#guardo el archivo en disco.
write.table(Clientes_Mar21, "../../datasets/aid/tp1/Clientes_Mar21.csv", sep=",", col.names=TRUE)

#------------------------------------------------------------------------------
#*****************************PUNTO 2******************************************
#------------------------------------------------------------------------------

#creo el dataframe usando un group by
Segmentacion_Mar21 <- Clientes_Mar21 %>% group_by(CL_TECNO) %>% 
                      summarise(Monto_total_promedio = round(mean(MONTO_TOTAL, na.rm=TRUE),2),
                      Monto_tecno_promedio = round(mean(MONTO_TECNO, na.rm=TRUE),2),
                      Cant_recargas_promedio = round(mean(CANT_RECARGAS, na.rm=TRUE),2),
                      Cant_rec_tecno_prom = round(mean(CANT_RTEC, na.rm=TRUE),2)) 

#guardo el csv del punto2
write.table(Segmentacion_Mar21, "../../datasets/aid/tp1/Segmentacion_Mar21.csv", sep=",", col.names=TRUE)




