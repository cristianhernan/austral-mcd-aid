#**************TRABAJO PRACTICO N2 ANALISIS INTELIGENTE DEE DATOS**************
#*-----------------------------------------------------------------------------
#-VEIGA CRISTIAN - COMISION B
#------------------------------------------------------------------------------

#deshabilitar la notación científica
options(scipen=999)
#limpio memoria
rm(list=ls())
gc()
#cargo librerias
library(readxl)
library(tidyverse)
library(summarytools)
library(ggplot2)
library(plotly)
library(treemap)
library(mice)

#library(dplyr)
#library(data.table)

#setwd("C:/Cursos/mcd/austral-mcd-aid/TP2")
#C:\Users\VEIGC901\Documents\code\cursos\austral\AID\austral-mcd-aid\TP2
data_cli <- read_excel("dataset.xls") #%>% select(-ID)

#compruebo si no tengo clientes duplicados
length(unique(data_cli$ID)) == nrow(data_cli)
#remuevo la columna ID
data_cli <- select(data_cli,-ID)

#------------------------------------------------------------------------------
#*****************************PUNTO 1******************************************
#------------------------------------------------------------------------------

#VARIABLES CUALITATIVAS:

# COLOR: representa la categoría del cliente a nivel crediticio.
# TIENE_PIN: indica si el cliente tiene o no acceso a “home banking”.
# VT: indica si el cliente está vinculado transaccionalmente con el banco, 
#     es un criterio de negocio que incluye parámetros de cantidad de adhesiones de débitos automáticos, 
#     cantidad de movimientos en cuenta, cantidad de productos contratados, 
#     de nuevo es un indicador de la “vinculación” del cliente con la empresa.
# CLIENTE_PAS: indica si el cliente acredita su sueldo en el banco.
# CHURN: que indica si el cliente se dio de baja en alguno de los 2 meses siguientes al de análisis.


#VARIABLES CUANTITATIVAS:
# EDAD: indica la edad del cliente.
# ANTIGÜEDAD: representa la antigüedad del cliente con la entidad financiera medida en meses.
# CONSUMO_TC: representa el total de consumo con tarjeta de crédito en el mes de análisis para el cliente.
# MOV90_CTA: representa la cantidad de movimientos realizados en la cuenta en los últimos 90 días,
# SUELDO: que representa el sueldo que cobra el cliente por su actividad laboral.
#         Esta variable contiene nulos para los casos que no son “clientes pas” ya que de estos no
#         sabemos cuál es su sueldo real, porque lo acredita en otro banco.

#------------------------------------------------------------------------------
#*****************************PUNTO 2******************************************
#------------------------------------------------------------------------------

#Casteo de variables char a factor
data_cli$COLOR <- as.factor(data_cli$COLOR)
data_cli$VT <- as.factor(data_cli$VT)
data_cli$CLIENTE_PAS <- as.factor(data_cli$CLIENTE_PAS)
data_cli$CHURN <- as.factor(data_cli$CHURN)
data_cli$TIENE_PIN <- as.factor(data_cli$TIENE_PIN)

#Analisis descriptivo
summary(data_cli)
view(dfSummary(data_cli))
view(freq(data_cli, plain.ascii = FALSE, style = "rmarkdown"))
view(descr(data_cli))


#TIENE_PIN: notamos que esta variables contiene un error de carga
#           hay 65 registros con el valor "2" cuando se trata de una variables "si/no"

#SUELDO: es la única variables con valores missing en mas del 72% de los registros.
#        tambien vemos posibles outliers en los valores maximos y minimos

#EDAD: Vemos valores outliers en ambos extremos, valores como 900 y 12 suponen
#       error de carga


ggplot(data_cli, aes(x=COLOR, fill=COLOR )) +  
  geom_bar(stat="count", width=0.4, ) +
  scale_fill_manual(values = c("blue","red","green", "violet") ) +
  theme(legend.position="none")+
  labs(x="Cat Crediticia",  y="Cantidad Clientes")+
  ggtitle("Cantidad de clientes por categoria")

ggplot(data_cli, aes(x=COLOR,y=MOV90_CTA ,fill=COLOR )) +  
  geom_bar(stat="identity", width=0.4 ,position = "dodge") +
  scale_fill_manual(values = c("blue","red","green", "violet") ) +
  theme(legend.position="none")+
  labs(x="Cat Crediticia",  y="Mov cta 90d")+
  ggtitle("Movimientos de cuenta por categoria")





ggplot(data_cli, aes(x=TIENE_PIN, y=MOV90_CTA, fill = CLIENTE_PAS ))+
  geom_bar(stat="identity", width=0.7, position = "dodge")+
  labs(x="Tiene pin",  y="mov cta 90d")+
  ggtitle("Cantidad de paises por region ")


treemap(data_cli, index=c("COLOR","VT"), vSize="MOV90_CTA", type="index",
        fontsize.labels=c(15,9),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
        fontcolor.labels=c("white","black"),     # Color of labels
        fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
        bg.labels=c("transparent"),              # Background color of labels
        align.labels=list(
          c("center", "center"), 
          c("right", "bottom")
        ),                                   # Where to place labels in the rectangle?
        overlap.labels=0.5,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
        inflate.labels=F,                        # If true, labels are bigger when rectangle is bigger.
        
)


plot(data_cli$MOV90_CTA,data_cli$Med_cada1000)


summary(filter(data_cli,EDAD > 99))







