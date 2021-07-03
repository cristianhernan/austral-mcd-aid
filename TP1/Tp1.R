#deshabilitar la notacion cientifica
options(scipen=999)
rm(list=ls())
gc()
library(dplyr)
library(tidyr)

reca_ene_raw <- read.csv("../../datasets/aid/tp1/RECA_CHAN_01_NEW.csv",header=TRUE, sep = ",")
reca_ene_raw <- select(reca_ene_raw,-RUNID)
reca_ene_filter <- filter(reca_ene_raw,PURCHASEAMOUNT > 0)

reca_feb_raw <- read.csv("../../datasets/aid/tp1/RECA_CHAN_02_NEW.csv",header=TRUE, sep = ",")
reca_feb_raw <- select(reca_feb_raw,-RUNID)
reca_feb_filter <- filter(reca_feb_raw,PURCHASEAMOUNT > 0)

reca_mar_raw <- read.csv("../../datasets/aid/tp1/RECA_CHAN_02_NEW.csv",header=TRUE, sep = ",")
reca_mar_raw <- select(reca_mar_raw,-RUNID)
reca_mar_filter <- filter(reca_mar_raw,PURCHASEAMOUNT > 0)

aux_data <-  rbind(reca_ene_filter,reca_feb_filter,reca_mar_filter)
aux_data <- select(aux_data,-PURCHASETIME)
rm(reca_ene_raw,reca_feb_raw,reca_mar_raw,reca_ene_filter,reca_feb_filter,reca_mar_filter)

aux_data <-mutate(aux_data, CHANNELIDENTIFIER = ifelse(grepl("EMG",CHANNELIDENTIFIER,fixed = TRUE),"Tecno", "Manual"))

group_data <- aux_data %>%  group_by(CUSTOMERID,CHANNELIDENTIFIER) %>% 
    summarize(cantidad = n(), monto = sum(PURCHASEAMOUNT))

cant_data <- spread(data = group_data, key = CHANNELIDENTIFIER, value = cantidad)
cant_data <- select(cant_data,-monto)
cant_data[is.na(cant_data)] <- 0
colnames(cant_data) <- c("ACCS_MTHD_CD","CANT_RMAN","CANT_RTEC")
cant_data <- mutate(cant_data,CANT_RECARGAS=CANT_RMAN+CANT_RTEC)


monto_data <- spread(data = group_data, key = CHANNELIDENTIFIER, value = monto)
monto_data <- select(monto_data,- cantidad)
monto_data[is.na(monto_data)] <- 0
colnames(monto_data) <- c("ACCS_MTHD_CD","MONTO_TECNO","MONTO_MANUAL")
monto_data <- mutate(monto_data,MONTO_TOTAL=MONTO_TECNO+MONTO_MANUAL)

nrow(monto_data)


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








