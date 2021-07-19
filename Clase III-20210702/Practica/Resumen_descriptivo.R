

#Resumen estadisticos descriptivos



#limpio la memoria
rm( list=ls() )
gc()

#install.packages("treemap")

options(scipen = 999) ### turn off scientific notation
library(tidyverse)
library(readxl)
library(treemap)
library(questionr)
library(corrplot)
library(GGally)
library(plotly)
library(summarytools) # para el uso de estadisticas descriptivas
library(dplyr)
library(sjPlot)

#univariado



Health_w <- read_excel("Health_systems02.xlsx", sheet = "Salud")


View(Health_w)

# Convierto las variables cualitativas como factor
Health_w <- mutate(Health_w, region = as.factor(region))
Health_w <- mutate(Health_w, Categ_desarrollo = as.factor(Categ_desarrollo))

summary(Health_w)


# reportes descriptivos

view(freq(Health_w$region, plain.ascii = FALSE, style = "rmarkdown"))

view(descr(Health_w))




#agrupar datos

#variable cuantitativa por una cualitativa

#comando pipe %>%
#Atajo para el comando pipe en Windows Ctrl + Shift + m

group_by(Health_w, region) %>% 
  summarise(
    count = n(), 
    mean = mean(Med_cada1000, na.rm = TRUE),
    sd = sd(Med_cada1000, na.rm = TRUE),
    min=min(Med_cada1000,na.rm = TRUE), 
    median=median(Med_cada1000,na.rm = TRUE),
    max=max(Med_cada1000,na.rm = TRUE),
  )


sum_Med_cada1000 <-  summarise(Health_w, media_Med_cada1000 = mean(Med_cada1000,na.rm=TRUE))
sum_Med_cada1000

aggregate(Med_cada1000~region, FUN = mean, Health_w)


media_Med_cada1000 <- Health_w %>%
  group_by(as.factor(region)) %>%
  summarise(media_Health_w = mean(Health_w, na.rm=TRUE))

media_Med_cada1000


decil <- seq(0, 1, 0.01)
quantile(Health_w$Med_cada1000, decil,na.rm = TRUE)

#para agregarlo al dataset

Health_w <- mutate(Health_w,decil = ntile(Med_cada1000, n = 10)) 


#dos variables cualitativas

sjt.xtab(Health_w$region, Health_w$Categ_desarrollo, show.row.prc = TRUE )



#tablas pivote

library(pivottabler)
# arguments:  qhpvt(dataFrame, rows, columns, calculations, ...)
qhpvt(Health_w, "region", "Categ_desarrollo", "n()") 


qhpvt(Health_w, "region", "Categ_desarrollo", 
      c("Cantidad" = "n()","media medicos"="mean(Med_cada1000, na.rm=TRUE)", "Std Dev medicos"="sd(Med_cada1000, na.rm=TRUE)","median medicos"="median(Med_cada1000, na.rm=TRUE)"), 
      formats=list("%.0f", "%.1f"), totals=list("region"="Total Region", "Categ_desarrollo"="Total Desarrollo"))



#EXPORT
write.csv(media_Med_cada1000, "media_Med_cada1000.csv")

#
