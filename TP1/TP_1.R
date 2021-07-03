# Materia: Análisis Inteligente de Datos - 2021
# Autor: Cristian De Blasis


#limpio la memoria
rm( list=ls() )
gc()

setwd("C:/Cursos/mcd/austral-mcd-aid/TP1")

getwd() #chequeo en que carpeta estoy

library(tidyverse)
library(gtools)
library(summarytools) # para el uso de estadisticas descriptivas
library(dplyr)
library(readr)

options(scipen = 999) ### turn off scientific notation

##################################################################################
# SE IMPORTAN LAS RECARGAS PARA LOS CLIENTES DE INTERES                          #
#                                                                                #
# IMPORTANTE: Para filtrar las recargas solo a los clientes de interes se        #
# utiliza un left join tal como se comentó en clase. En mi caso hubiera usado un #
# inner join debido a que el "Objetivo" es identificar los canales de recarga,   #
# es decir, si no recarga no tendría ningun interes el análisis. Por otro lado   #
# la columna POR_TECNO va a dar indeterminada 0/0 para aquellos casos que no se  #
# realizaron recargas                                                            #
##################################################################################


# Se importan las recargas de ENERO (sólo columnas necesarias a fin de optimizar memoria)
RECA_CHAN_01 <- read_csv("../../datasets/aid/tp1/RECA_CHAN_01_NEW.csv", 
                        col_types = cols(X1 = col_skip(), RUNID = col_skip(), 
                        CUSTOMERID = col_number(), PURCHASETIME = col_skip()))
#Se dejan solos los registros con PURCHASEAMOUNT > 0.
RECA_CHAN_01<-RECA_CHAN_01[RECA_CHAN_01$PURCHASEAMOUNT>0,]

# Se importan las recargas de FEBRERO (sólo columnas necesarias a fin de optimizar memoria)
RECA_CHAN_02 <- read_csv("../../datasets/aid/tp1/RECA_CHAN_02_NEW.csv", 
                         col_types = cols(X1 = col_skip(), RUNID = col_skip(), 
                                          CUSTOMERID = col_number(), PURCHASETIME = col_skip()))
#Se dejan solos los registros con PURCHASEAMOUNT > 0.
RECA_CHAN_02<-RECA_CHAN_02[RECA_CHAN_02$PURCHASEAMOUNT>0,]


# Se importan las recargas de FEBRERO (sólo columnas necesarias a fin de optimizar memoria)
RECA_CHAN_03 <- read_csv("../../datasets/aid/tp1/RECA_CHAN_03_NEW.csv", 
                         col_types = cols(X1 = col_skip(), RUNID = col_skip(), 
                                          CUSTOMERID = col_number(), PURCHASETIME = col_skip()))
#Se dejan solos los registros con PURCHASEAMOUNT > 0.
RECA_CHAN_03<-RECA_CHAN_03[RECA_CHAN_03$PURCHASEAMOUNT>0,]


RECA<-union_all(RECA_CHAN_01,RECA_CHAN_02)
RECA<-union_all(RECA,RECA_CHAN_03)

# Libera espacio
RECA_CHAN_01<-NULL
RECA_CHAN_02<-NULL
RECA_CHAN_03<-NULL

summary(RECA)


##################################################################################
# SE IMPORTAN LOS CLIENTES Y SE FILTRAN LOS QUE ESTAN EN 'ACTIVE' o 'REJOINNER'  #
##################################################################################
# Solo se important las columnas y en el formato necesario
DNA_03_NEW <- read_csv("../../datasets/aid/tp1/DNA_03_NEW.csv", 
                       col_types = cols(X1 = col_skip(), ACCS_MTHD_CD = col_number()))%>%
              filter(grepl("ACTIVE", BASE_STAT_03) | grepl("REJOINNER", BASE_STAT_03))


# Nos quedamos con las recargas de los clientes de interes
RECA <- merge(x = DNA_03_NEW, y =RECA,by.x='ACCS_MTHD_CD', by.y ='CUSTOMERID',all.x=TRUE)
DNA_03_NEW<-NULL #Libera espacio en memoria

#################################################################################
# 1) SE CREA LA TABLA AGRUPADA 'Clientes_Mar21' y se exporta                    #  
#################################################################################

#Se agrega una columna del tipo de canal normalizada a 1 (Tecno) y 0 (Manual)
RECA<-mutate(RECA, TIPO_CANAL = ifelse(grepl('EMG', RECA$CHANNELIDENTIFIER), 1, 0))

#Se realiza la agrupación calculandose algunas de las columnas solicitadas
Clientes_Mar21<-RECA %>%
  group_by(ACCS_MTHD_CD,BASE_STAT_03) %>%
  summarise(MONTO_TOTAL = sum(PURCHASEAMOUNT, na.rm=TRUE),
            MONTO_TECNO = sum(PURCHASEAMOUNT*TIPO_CANAL, na.rm=TRUE),
            CANT_RECARGAS = n(),
            CANT_RTEC = sum(TIPO_CANAL, na.rm=TRUE))

RECA<-NULL #Libera memoria


#Se calculan las columnas faltantes
Clientes_Mar21$POR_TECNO_M<-round(100*Clientes_Mar21$MONTO_TECNO/Clientes_Mar21$MONTO_TOTAL,0)
Clientes_Mar21$POR_TECNO<-round(100*Clientes_Mar21$CANT_RTEC/Clientes_Mar21$CANT_RECARGAS,0)

Clientes_Mar21$BASE_STAT_03 <- as.factor(Clientes_Mar21$BASE_STAT_03)

summary(Clientes_Mar21)

Clientes_Mar21<-mutate(Clientes_Mar21, CL_TECNO = case_when(
                                      (CANT_RECARGAS<3) ~ "99-NOSEGM",
                                      (POR_TECNO>=70) ~ "1-Tecno",
                                      ((POR_TECNO>=40) & (POR_TECNO<70)) ~ "2-Mix4070",
                                      ((POR_TECNO>0) & (POR_TECNO<40)) ~ "3-MixH40",
                                      (POR_TECNO==0) ~ "4-No Tecno"
                                    ))

#Se le da el orden correspondiente a las columnas
col_order <- c('ACCS_MTHD_CD','BASE_STAT_03','MONTO_TOTAL','MONTO_TECNO','POR_TECNO_M','CANT_RECARGAS',
'CANT_RTEC','POR_TECNO','CL_TECNO')
Clientes_Mar21 <- Clientes_Mar21[, col_order]
#colnames(Clientes_Mar21)

#Se exporta el archivo
write.csv(Clientes_Mar21, "Clientes_Mar21.csv")

#################################################################################
# 2) SE CREA LA TABLA AGRUPADA 'Segmentacion_Mar21' y se exporta                #  
#################################################################################

Segmentacion_Mar21<-Clientes_Mar21 %>%
                    group_by(CL_TECNO) %>%
                    summarise(Monto_total_promedio = mean(MONTO_TOTAL, na.rm=TRUE),
                              Monto_tecno_promedio = mean(MONTO_TECNO, na.rm=TRUE),
                              Cant_recargas_promedio = mean(CANT_RECARGAS, na.rm=TRUE),
                              Cant_rec_tecno_prom = mean(CANT_RTEC, na.rm=TRUE))

write.csv(Segmentacion_Mar21, "Segmentacion_Mar21.csv")

#################################################################################
# 3) ESTADISTICAS DESCRIPTIVAS y GRAFICOS                                       #  
#################################################################################

# DATOS
# Se convierten las variables categoricas en factores
Clientes_Mar21 <- read.csv("../teco_db/Clientes_Mar21.csv", stringsAsFactors=TRUE)
# Elimino columnas innecesarias para la grafica
Clientes_Mar21$X<-NULL
Clientes_Mar21$ACCS_MTHD_CD<-NULL

# DESCRIPCIÓN GENERAL
# Descripción de los datos
summary(Clientes_Mar21)


# VARIABLES CUALITATIVAS
# Se realiza una estadística descriptiva
view(freq(Clientes_Mar21$BASE_STAT_03, plain.ascii = FALSE, style = "rmarkdown"))
view(freq(Clientes_Mar21$CL_TECNO, plain.ascii = FALSE, style = "rmarkdown"))

#Se observa que:
# * mayoritariamente son AVTIVE BASE (+ 96%)
# * hay cierta distribución en CL_TECNO

# Por lo tanto, se toma la variable CL_TECNO como base

data<-Clientes_Mar21 %>%
  group_by(CL_TECNO,BASE_STAT_03) %>%
  summarise(valor = n())

ggplot(data, aes(x=CL_TECNO,y=valor, fill=BASE_STAT_03))+
  geom_bar(stat="identity",color='blue')+
  scale_fill_brewer(palette='Paired')+
  coord_flip()+
  labs(x="Grupos",  y="Cantidad de usuarios")+
  ggtitle("Cantidad de usuarios por grupo")

# VARIABLES CUANTITATIVAS
view(descr(Clientes_Mar21)) 
# Acá tambien aparecen los valores indeterminados de POR_TECNO cambiando el 
# valor del tercer cuartil principalmente

# Distribución del monto total (continua)
# Se agrupan los datos a fin de poderlos comparar
df_monto<-gather(Clientes_Mar21, MONTO_TOTAL,MONTO_TECNO, key="MONTO", value="valor")[,
                                                                    c('MONTO','valor')]
df_monto%>%filter( valor<100 ) %>%
ggplot(aes(x=valor, group=MONTO, fill=MONTO)) + 
  facet_wrap(~MONTO) +
  geom_density(alpha=0.6,show.legend=FALSE)+
  labs(y = "Densidad",x = "") + ylim(0, 0.15)

# Histograma de la cantidad de cargas (discreta)
df_cant<-gather(Clientes_Mar21, CANT_RECARGAS,CANT_RTEC, key="CANT", value="valor")[,
                                                                    c('CANT','valor')]
df_cant%>%filter( valor<25) %>% 
  ggplot(aes(x=valor,fill=CANT,color=CANT,y=stat(count)/sum(stat(count))))+
  facet_wrap(~CANT) +
  scale_y_continuous(labels = scales::percent)+
  geom_histogram( binwidth=1,show.legend=FALSE, alpha=0.9)+
  labs(y = "% de Cargas",x = "")
#Se observa que hay muchas recargas tecnologicas en cero, esto quiere decir que muchas 
#de las recargas son manuales

# Realacion entre el monto y la cantidad
library(ggExtra)
N<-1000 # Se realiza un sampleo aleatorio debido a que consume mucha memoria
p<-ggplot(Clientes_Mar21[sample(nrow(Clientes_Mar21), N), ], aes(x=POR_TECNO, y=POR_TECNO_M)) + 
  geom_point() + 
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE)+ 
  labs(y = "% monto tecnologica",x = "% cantidad tecnological")
ggMarginal(p, type="density")

#Se libera memoria
p<-NULL
df_monto<-NULL
df_cant<-NULL


#################################################################################
# 4) COMPORTAMIENTO POR MONTO Y POR DESCARGA                                    #  
#################################################################################

# MONTO por segmento
Clientes_Mar21%>%filter( MONTO_TOTAL<200 & CL_TECNO!='99-NOSEGM' ) %>% 
  ggplot(aes(x=MONTO_TOTAL,fill=CL_TECNO,color=CL_TECNO))+
  facet_wrap(~CL_TECNO) +
  geom_histogram( binwidth=1,show.legend=FALSE, alpha=0.9)+
  labs(y = "Montos",x = "")

Clientes_Mar21%>%filter( CL_TECNO!='99-NOSEGM' ) %>% 
ggplot(aes(x=CL_TECNO, y=MONTO_TOTAL, fill=CL_TECNO)) + # fill=name allow to automatically dedicate a color for each group
  geom_violin()+
  labs(y = "Montos",x = "")

Clientes_Mar21%>%filter(CL_TECNO!='99-NOSEGM' ) %>% 
  ggplot(aes(x=CL_TECNO, y=MONTO_TOTAL, fill=CL_TECNO)) + # fill=name allow to automatically dedicate a color for each group
  geom_boxplot()+
  labs(y = "Montos",x = "")

# CARGAS por segmento
Clientes_Mar21%>%filter( CANT_RECARGAS<25 & CL_TECNO!='99-NOSEGM' ) %>% 
  ggplot(aes(x=CANT_RECARGAS,fill=CL_TECNO,color=CL_TECNO))+
  facet_wrap(~CL_TECNO) +
  geom_histogram( binwidth=1,show.legend=FALSE, alpha=0.9)+
  labs(y = "Cargas",x = "")

Clientes_Mar21%>%filter(CL_TECNO!='99-NOSEGM' ) %>% 
  ggplot(aes(x=CL_TECNO, y=CANT_RECARGAS, fill=CL_TECNO)) + # fill=name allow to automatically dedicate a color for each group
  geom_violin()+
  labs(y = "Cargas",x = "")

Clientes_Mar21%>%filter(CL_TECNO!='99-NOSEGM' ) %>% 
  ggplot(aes(x=CL_TECNO, y=CANT_RECARGAS, fill=CL_TECNO)) + # fill=name allow to automatically dedicate a color for each group
  geom_boxplot()+
  labs(y = "Cargas",x = "")
