#deshabilitar la notacion cientifica
options(scipen=999)
#limpio memoria
rm(list=ls())
gc()
#cargo librerias
library(dplyr)
library(tidyr)
library(summarytools) # para el uso de estadisticas descriptivas


#cargo los files
reca_ene_raw <- read.csv("../../datasets/aid/tp1/RECA_CHAN_01_NEW.csv",header=TRUE, sep = ",")
reca_feb_raw <- read.csv("../../datasets/aid/tp1/RECA_CHAN_02_NEW.csv",header=TRUE, sep = ",")
reca_mar_raw <- read.csv("../../datasets/aid/tp1/RECA_CHAN_02_NEW.csv",header=TRUE, sep = ",")

#uno los dataframes en solo uno
reca_data <-  rbind(reca_ene_raw,reca_feb_raw,reca_mar_raw) 

#elimino dataframes de memoria para limpiar espacio.
rm(reca_ene_raw,reca_feb_raw,reca_mar_raw)

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

#asigno cero a todos los nulls encontrados (de todas formas no hay)
reca_data_gs[is.na(reca_data_gs)] <- 0
reca_data_gc[is.na(reca_data_gc)] <- 0

#PUNTO 1******************
#primero paso el las cantidades del channelidentifier de fila a columna
cant_data <- spread(data = reca_data_gc, key = CHANNELIDENTIFIER, value = cantidad)
#le asigno cero a los null que podria haber
cant_data[is.na(cant_data)] <- 0
#renombro las columnas
colnames(cant_data) <- c("ACCS_MTHD_CD","CANT_RMAN","CANT_RTEC")
#creo nueva columnas calculadas
cant_data <- mutate(cant_data,CANT_RECARGAS=CANT_RMAN+CANT_RTEC)

#luego paso el monto de los channnelidentifier de fila a columna
monto_data <- spread(data = reca_data_gs, key = CHANNELIDENTIFIER, value = monto)
#le asigno cero si hubiera na
monto_data[is.na(monto_data)] <- 0
#renombro las columnas
colnames(monto_data) <- c("ACCS_MTHD_CD","MONTO_TECNO","MONTO_MANUAL")
#creo columnas calculadas
monto_data <- mutate(monto_data,MONTO_TOTAL=MONTO_TECNO+MONTO_MANUAL)

#hago un join entre ammbos dataframes por la columna accs_mthd_cd
tabla <- merge(x=cant_data,y=monto_data,by="ACCS_MTHD_CD",all.x=TRUE)

#elimino ds temporales de momoria para liberar espacio
rm(reca_data_gc,reca_data_gs,monto_data,cant_data,reca_data)

#creo las columnas de porcentaje
tabla <- tabla %>% 
          mutate(POR_TECNO = round((CANT_RTEC/CANT_RECARGAS)*100)) %>% 
          mutate(POR_TECNO_M = round((MONTO_TECNO/MONTO_TOTAL)*100))

#creo el dataframe definitivo del punto uno junto a su nueva columna CL Tecno
tabla <- tabla %>% 
  mutate(CL_TECNO  = case_when(.$POR_TECNO > 70 ~ "1-Tecno",
                                .$POR_TECNO >= 40  & POR_TECNO <=70 ~ "2-Mix4070",
                                .$POR_TECNO < 40 ~ "3-MixH40",
                                .$POR_TECNO ==0 ~ "4-No Tecno",
                                .$CANT_RTEC <3 ~ "99-NOSEGM", 
                                 TRUE ~ "NONE"
  )) 

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

Clientes_Mar21 <- Clientes_Mar21 %>% select("ACCS_MTHD_CD","BASE_STAT_03","MONTO_TOTAL","MONTO_TECNO","POR_TECNO_M","CANT_RECARGAS","CANT_RTEC","POR_TECNO","CL_TECNO")

Clientes_Mar21$CL_TECNO <- as.factor(Clientes_Mar21$CL_TECNO)

Clientes_Mar21$ACCS_MTHD_CD <- as.factor(Clientes_Mar21$ACCS_MTHD_CD)

summary(Clientes_Mar21)
nrow(Clientes_Mar21)

dfSummary(reca_clientes_raw)

#summary(tabla)
#unique(cant_data$ACCS_MTHD_CD)
#duplicated(cant_data$ACCS_MTHD_CD)
#head(reca_data_g)


#cant_data[duplicated(cant_data$ACCS_MTHD_CD),]

#filter(cant_data,CUSTOMERID==120034936)

#setnames(cant_data , new = c("ACCS_MTHD_CD","CANT_RECARGAS","CANT_RTEC"))

#monto_data <- spread(data = group_data, key = CHANNELIDENTIFIER, value = monto)

  #spread(data = group_data, key = CHANNELIDENTIFIER, value = monto)

#tail(cant_data)

#group_data <- aux_data %>%  group_by(CUSTOMERID,CHANNELIDENTIFIER) %>% 
  #summarize(cantidad = n(),monto = sum(PURCHASEAMOUNT)) 

#gather(data = group_data, key = "cantidad", value = "monto", 2:4)




#head(group2)

#df_2$weight, by = list(df_2$feed, df_2$cat_var), FUN = sum

#group_data <- aggregate(aux_data$PURCHASEAMOUNT, by = list(aux_data$CUSTOMERID, aux_data$CHANNELIDENTIFIER), FUN=sum)


#reca_grupo <- aux_data %>%
#  group_by(CUSTOMERID) %>%
#  summarise(MONTO_TOTAL = sum(PURCHASEAMOUNT, na.rm=TRUE),
#            CANT_RECARGAS = n())

#head(reca_grupo)

#reca_clientes <- read.csv("../../datasets/aid/tp1/DNA_03_NEW.csv",header=TRUE, sep = ",")
#reca_clientes <- filter(reca_clientes,BASE_STAT_03  == "REJOINNER" | grepl("ACTIVE",BASE_STAT_03,fixed = TRUE))
#head(reca_clientes)








