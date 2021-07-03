#**************TRABAJO PRACTICO N1 ANALISIS INTELIGENTE DEE DATOS**************
#*-----------------------------------------------------------------------------
#-VEIGA CRISTIAN - COMISION B
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
library(treemap)

setwd("C:/Cursos/mcd/austral-mcd-aid/TP1")

getwd()

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

#------------------------------------------------------------------------------
#*****************************PUNTO 1******************************************
#------------------------------------------------------------------------------

#uno los dataframes en solo uno
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
write.table(Clientes_Mar21, "../../datasets/aid/tp1/Clientes_Mar21.csv", sep=",", col.names=TRUE,row.name=FALSE)

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
write.table(Segmentacion_Mar21, "../../datasets/aid/tp1/Segmentacion_Mar21.csv", sep=",", row.names = FALSE,col.names=TRUE)

#------------------------------------------------------------------------------
#*****************************PUNTO 3******************************************
#------------------------------------------------------------------------------

#Realizamos un resumen estadístico del dataframe

summary(Clientes_Mar21)

# Del resumen estadístico, podemos notar:
# -Presencia de ouliers en todas variables cualitativas.

# -Vemos que la media siempre es mayor a la mediana, los que nos sugiere
#  una distribución asimétrica.

freq(Clientes_Mar21$BASE_STAT_03, plain.ascii = FALSE, style = "rmarkdown")

freq(Clientes_Mar21$CL_TECNO, plain.ascii = FALSE, style = "rmarkdown")

# De las frecuencias podemos inferir:
# -Vemos una marcada mayoría de clientes ACTIVE BASE con mas del 96% sobre
# los clientes REJOINER
# 
# -tenemos un 38.5% en el segmento 99-NOSEGM
# esto nos dice que existen 498.446 clientes ACTIVE/ REJOINER que realizaron
# menos de 3 recargas entre enero y marzo.

dfSummary(Clientes_Mar21$CL_TECNO)

descr(select(Clientes_Mar21,-ACCS_MTHD_CD))

#-------------------------GRAFICOS---------------------------------------------
# VARIABLES CUALITATIVAS
#-----------------------

#Cantidad de clientes por segmento y grupo
aux_data<-Clientes_Mar21 %>% group_by(CL_TECNO,BASE_STAT_03) %>%
  summarise(cant = n())

ggplot(aux_data, aes(x=CL_TECNO,y=cant, fill=BASE_STAT_03))+
  geom_bar(stat="identity",color='black')+
  scale_fill_brewer(palette='Set2')+
  coord_flip()+
  labs(x="Segmentos",  y="Cantidad de clientes")+
  ggtitle("Cantidad de clientes por segmento y grupo")

#Arbol clientes por segmento
treemap(aux_data,
        title = 'Clientes por Segmento',
        index="CL_TECNO",
        vSize="cant",
        type="index", 
        palette = "Set1",
        fontsize.labels=c(10),              
        fontface.labels=c(2),                  
        bg.labels=c("transparent")             
)


# -De los graficos anteriores podemos notar que la mayoría de los clientes REJOINER
# realizaron menos de TRES recargas en los últimos tres meses.
# Sin embargo, para el resto de los segmentos, es notable la mayoría de los 
# clientes ACTIVE BASE

#Cantidad de recargas por segm y grupo
aux2 <- aggregate(CANT_RECARGAS ~ CL_TECNO + BASE_STAT_03, data = Clientes_Mar21, FUN = sum)
ggplot(aux2, aes(x=CL_TECNO,y=CANT_RECARGAS, fill=BASE_STAT_03))+
  geom_bar(stat="identity",color='black')+
  scale_fill_brewer(palette='Set2')+
  coord_flip()+
  labs(x="Segmentos",  y="Cantidad de recargas")+
  ggtitle("Cantidad de recargas por segm y grupo")

#Monto de recargas por segmento y grupo
aux3 <- aggregate(MONTO_TOTAL ~ CL_TECNO + BASE_STAT_03, data = Clientes_Mar21, FUN = sum)
ggplot(aux3, aes(x=CL_TECNO,y=MONTO_TOTAL, fill=BASE_STAT_03))+
  geom_bar(stat="identity",color='black')+
  scale_fill_brewer(palette='Set2')+
  coord_flip()+
  labs(x="Segmentos",  y="Monto de recargas")+
  ggtitle("Monto de recargas por segm y grupo")

# -Podemos ver que el segmento dominante tanto en cantidad y en monto
# -es 4-No Tecno, es decir, que los clientes siguen prefiriendo la recarga
#  manual
# 
# VARIABLES CUANTITATIVAS
#-----------------------
# *POR UN TEMA DE RENDIMINETO DE LA PC
# *para los siguientes gráficos tomo una muestra de 10000 registros

n <- 10000
cli_sample <- Clientes_Mar21[sample(nrow(Clientes_Mar21),n,replace = FALSE),]


ggplot(data=filter(cli_sample, CL_TECNO!='99-NOSEGM' ))+
  geom_histogram(aes(x=MONTO_TOTAL), fill="blue", alpha=.3,bins=90)

ggplot(data=filter(cli_sample, CL_TECNO!='99-NOSEGM' ))+
  geom_histogram(aes(x=CANT_RECARGAS), fill="red", alpha=.3,bins=90)

#Notamos que tanto el monto como la cantidad de recargas tienen, en principio
#una distribucion asimetrica hacias la derecha.

#sin embargo, tambien podemos evaluar la posibilidad de que existan dos picos
#es decir que sea bimodal


# MONTO por segmento
cli_sample%>%filter( CL_TECNO!='99-NOSEGM' ) %>% 
  ggplot(aes(x=MONTO_TOTAL,fill=CL_TECNO,color=CL_TECNO))+
  facet_wrap(~CL_TECNO) +
  geom_histogram( binwidth=1,show.legend=FALSE, alpha=0.9)+
  ggtitle("Monto por Segmento")+
  labs(y = "Montos",x = "")

# RECARGAS por segmento
cli_sample%>%filter( CL_TECNO!='99-NOSEGM' ) %>% 
  ggplot(aes(x=CANT_RECARGAS,fill=CL_TECNO,color=CL_TECNO))+
  facet_wrap(~CL_TECNO) +
  geom_histogram( binwidth=1,show.legend=FALSE, alpha=0.9)+
  ggtitle("Recargas por Segmento")+
  labs(y = "Recargas",x = "")


# RECARGAS por grupo de cliente
cli_sample %>% 
  ggplot(aes(x=CANT_RECARGAS,fill=BASE_STAT_03,color=BASE_STAT_03))+
  facet_wrap(~BASE_STAT_03) +
  geom_histogram( binwidth=1,show.legend=FALSE, alpha=0.9)+
  ggtitle("Recargas por grupo")+
  labs(y = "Recargas",x = "")

#dispersión entre cantidad de recargas y monto

gd <- filter(cli_sample, CL_TECNO!='99-NOSEGM') %>%
  ggplot( aes(MONTO_TOTAL, CANT_RECARGAS, labels = BASE_STAT_03, color=CL_TECNO)) +
  geom_point(size = 2,alpha=.5) +
  labs(x="Monto",  y="Recargas ",color="Segmento")+
  ggtitle("Monto y Cantidad por Segmento")+
  theme_bw()
ggplotly(gd)

ggplot(data= filter(cli_sample, CL_TECNO!='99-NOSEGM'),aes(x=MONTO_TOTAL,y=CANT_RECARGAS))+
  geom_point(color="red",size = 1,alpha=.5)+
  geom_smooth(method=lm,se=FALSE,size = .5)+
  facet_wrap(~CL_TECNO)+
  theme_bw()

#-según los graficos de dispersión, vemos una relación lineal positiva entre cantidad de 
#recargas y cantidad


#En los gráficos de box plot, tanto para MONTO y CANTIDAD
ggplot(filter(cli_sample, CL_TECNO!='99-NOSEGM'), aes(x=CL_TECNO, y=CANT_RECARGAS, fill = CL_TECNO)) + 
  geom_boxplot()+
  stat_summary(fun=mean, geom="point", shape=2, size=3)+
  labs(x="Segmentacon",  y="Recargas ",color="Tipo")


ggplot(filter(cli_sample, CL_TECNO!='99-NOSEGM'), aes(x=CL_TECNO, y=MONTO_TOTAL, fill = CL_TECNO)) + 
  geom_boxplot()+
  stat_summary(fun=mean, geom="point", shape=2, size=3)+
  labs(x="Segmentacon",  y="Monto",color="Tipo")

# -Notamos gran cantidad de valores atípicos (outliers), mas allá de los casos
# extremos leves
# 
# -vemos que los 4 segmentos estudiados poseen una distribución asimétrica dado
# no coinciden la mediana y la media, a su vez, diferencias en tamaño de los "Bigotes" 
#
#-Vemos que todos los segmentos estan relativamente alineados, por lo tanto
# decimos que el segmento no tiene gran influencia en las recargas y el monto
#

#------------------------------------------------------------------------------
#--------------------------PUNTO 4:-------------------------------------

# Como conclusión sobre el comportamiento de los segmentos, respecto al monto 
# y a la cantidad de recargas. Podríamos decir:
#   
# 1.	Más allá del segmento que se estudie, una relación lineal positiva
#     entre las variables MONTO_TOTAL y CANT_RECARGAS
# 
# 2.	EL 21.6% de los clientes el canal tecnológico para realizar recargas, 
# está lejos del 40% de clientes que no lo uso nunca, lo cual podría ser 
# una dificultad a la hora de plantear una migración. 
# 
# 3.	En los meses estudiados, el segmento predominante, tanto en recargas como 
# en monto, fue el “4-No Tecno”. Esto nos dice que casi el 40% de los clientes
# NO UTILIZA canales de recargas tecnológicos.
# 
# 4.	Hay un 38.5% de clientes ACTIVE BASE o REJOINER que no realizaron cargas 
# significativas (menos de 3) en los meses estudiados.
#-----------------------------------------------------------------------------



