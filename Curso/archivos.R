


setwd("C:/Users/valer/Documents/Maestria-Profesora/AID/2021/ClaseI/archivos_practica1")

getwd() #chequeo en que carpeta estoy

library(tidyverse)
library(gtools)
library(summarytools) # para el uso de estadisticas descriptivas


DNA <- read.csv("DNA_03.csv")
DNA <- mutate(DNA, ACCS_MTHD_CD=ACCS_MTHD_CD*2)


write.csv(DNA, "DNA_03_NEW.csv")


RECA_CHAN_01 <- read.csv("RECA_CHAN_01.csv")
RECA_CHAN_01 <- mutate(RECA_CHAN_01, CUSTOMERID=CUSTOMERID*2)
write.csv(RECA_CHAN_01, "RECA_CHAN_01_NEW.csv")

RECA_CHAN_02 <- read.csv("RECA_CHAN_02.csv")
RECA_CHAN_02 <- mutate(RECA_CHAN_02, CUSTOMERID=CUSTOMERID*2)
write.csv(RECA_CHAN_02, "RECA_CHAN_02_NEW.csv")


RECA_CHAN_03 <- read.csv("RECA_CHAN_03.csv")
RECA_CHAN_03 <- mutate(RECA_CHAN_03, CUSTOMERID=CUSTOMERID*2)
write.csv(RECA_CHAN_03, "RECA_CHAN_03_NEW.csv")