

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

#Visualizaciones!!!

#https://www.data-to-viz.com/#explore

# https://plotly.com/r/


#setwd("C:/Users/valer/Documents/Maestria-Profesora/AID/2021/Clase II/Practica")


Health_w <- read_excel("Health_systems02.xlsx", sheet = "Salud")


View(Health_w)

# Convierto las variables cualitativas como factor
Health_w <- mutate(Health_w, region = as.factor(region))
Health_w <- mutate(Health_w, Categ_desarrollo = as.factor(Categ_desarrollo))

summary(Health_w)


# reportes descriptivos

view(freq(Health_w$region, plain.ascii = FALSE, style = "rmarkdown"))

view(describe(Health_w))

view(dfSummary(Health_w))

     

### GRAFICOS PARA VARIABLES CUALITATIVAS

# Datos, estetica y geometria es el minimo que necesito para un grafico en ggplot2.

## UNA VARIABLE

# Grafico de barras

ggplot(Health_w, aes(x=region))+
    geom_bar(stat="count", width=0.4, fill="steelblue")+
  labs(x="Regi?n",  y="Cantidad de paises",color="Tipo")+
  ggtitle("Cantidad de paises por region")



#Grafico circular

data <- data.frame(group=c("Africa","America", "Asia", "Europa","Europa-Asia", "Oceania"),
                  value=c(52,35,40,43,7,13))


###data <- data.frame(freq(Health_w$region, valid =F))

ggplot(data, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void() # remueve el fondo, las etiquetas de variables y la grilla


USPersonalExpenditure <- data.frame("Categorie"=rownames(USPersonalExpenditure), USPersonalExpenditure)
data <- USPersonalExpenditure[,c('Categorie', 'X1960')]

fig <- plot_ly(data, labels = ~group, values = ~value, type = 'pie')
fig <- fig %>% layout(title = 'Paises por region',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig



#Mapa de arboles

treemap(data, index="group",
        vSize="value",
        type="index")

# arbol mejorado

treemap(data,
        index="group",
        vSize="value",
        type="index", 
        palette = "Set1",
        fontsize.labels=c(11),                 # Tama?o de etiqueta. 
        fontcolor.labels=c("white"),           # Color de etiqueta
        fontface.labels=c(2),                  # Dise?o de etiqueta: 1,2,3,4 para normal, bold, italic, bold-italic
        bg.labels=c("transparent")             # Color de fondo de las etiquetas
)


# Dos variables cualitativas

regionxdesar <- data.frame(table(Health_w$region,Health_w$Categ_desarrollo))

colnames(regionxdesar)[2]<-"Economia"
colnames(regionxdesar)[1]<-"region"

regionxdesar <- mutate(regionxdesar, 
                       Economia = ifelse(Economia =="Economia emergente y en desarrrollo", "Emerg. y desarrollo",
                                  ifelse(Economia == "Economia avanzada", "Avanzada", "NA" )))


ggplot(regionxdesar, aes(x=region, y=Freq, fill = Economia ))+
  geom_bar(stat="identity", width=0.7, colour="blue")+
  scale_fill_brewer(palette = "Paired")+
  labs(x="Regi?n",  y="Frecuencia",color="Tipo")+
  ggtitle("Cantidad de paises por region ")



#Barras adyacentes


ggplot(regionxdesar, aes(x=region, y=Freq, fill = Economia ))+
  geom_bar(stat="identity", width=0.7, colour="blue", position = "dodge")+
  scale_fill_brewer(palette = "Paired")+
  labs(x="Regi?n",  y="Frecuencia",color="Tipo")+
  ggtitle("Cantidad de paises por region ")



#Treemap anidados

treemap(regionxdesar, index=c("region","Economia"),     vSize="Freq", type="index",
        
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


### GRAFICOS CON VARIABLES CUANTITATIVAS

# UNA VARIABLE CUANTITATIVA

ggplot(data=Health_w)+
  geom_histogram(aes(x=Gasto_saludPIBpor))
  

# Eligiendo cantidad de barras

ggplot(data=Health_w)+
  geom_histogram(aes(x=Gasto_saludPIBpor),bins=15)

ggplot(data=Health_w)+
  geom_histogram(aes(x=Gasto_saludPIBpor),bins=40)

# alpha es un parametro para suavizar el color va de 0 a 1

ggplot(data=Health_w)+
  geom_histogram(aes(x=Gasto_saludPIBpor), fill="blue", alpha=.4,bins=40)



ggplot(data=Health_w)+
  geom_histogram(aes(x=Gasto_saludPIBpor), fill="red", alpha=.3,bins=40)




#Graficos de dispersi?n (dos variables cuantitativas - scatterplot)

plot(Health_w$Gasto_saludPIBpor,Health_w$Med_cada1000)

library(ggplot2)
ggplot(data=Health_w, aes(x=Gasto_saludPIBpor,y=Med_cada1000))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE)



p <- Health_w %>%
  ggplot( aes(Gasto_saludPIBpor, Med_cada1000, labels = country_name, color=region)) +
  geom_point() +
  theme_bw()

ggplotly(p)

# aumentar los puntos y cambiar color

ggplot(data=Health_w)+
  geom_point(aes(x=Gasto_saludPIBpor,y=Med_cada1000), size = 3, color="blue")

#color por region 

ggplot(data=Health_w)+
  geom_point(aes(x=Gasto_saludPIBpor,y=Med_cada1000,color = region), size = 3)

#graficos por region


ggplot(data=Health_w)+
  geom_point(aes(x=Gasto_saludPIBpor,y=Med_cada1000), 
             color="blue", size = 3)+
  facet_wrap(~region)

#usar distintas formas de los puntos

ggplot(data=Health_w)+
  geom_point(aes(x=Gasto_saludPIBpor,y=Med_cada1000), 
             color="blue", size = 3, shape = 18)+
  facet_wrap(~region)

# agregar titulos

ggplot(data=Health_w)+
  geom_point(aes(x=Gasto_saludPIBpor,y=Med_cada1000), 
             color="red", size = 3, shape = 16)+
  facet_wrap(~region)+
  labs(x="Gasto en Salud",  y="Medicos cada 1000 habitantes",color="Tipo")


#correlogramas

#crear nombre de filas para usarlo en los graficos

row.names(Health_w) <- Health_w$country_name 

#selecciono solo las variables numericas
idx <- which(sapply(Health_w, class) %in% c("numeric","integer"))

# creo un subconjunto solo con las variables numericas
n_Health_world <- subset(Health_w, select = idx )

# graficos de dispersion conjuntos

pairs(~nacimiento_reg_por+Gasto_saludPIBpor+Med_cada1000+Enferme_cada1000,data=n_Health_world, 
      main="Matriz de dispersi?n")



#matriz de correlacion (por defecto usa Pearson, pero puede hacer spearman o kendall)
cor_health <- cor(n_Health_world, use = "pairwise.complete.obs")
#cor.test <- cor.test(x =, use = "pairwise.complete.obs")
#Correlacion solo dos variable
#cor(Health_world$Gasto_saludpor, Health_world$nacimiento_reg_por, "pairwise.complete.obs")

corrplot(cor_health)

corrplot(cor_health, method = "shade",shade.col = NA ,  tl.col = "black" , tl.srt = 45, addCoef.col = "black")




### GRAFICOS PARA VARIABLES CUANTITATIVAS VS CUALITATIVAS

# BOXPLOTS

ggplot(Health_w, aes(x=Gasto_saludPIBpor, y=region)) + 
  geom_boxplot()


ggplot(Health_w, aes(x=region, y=Gasto_saludPIBpor)) + 
    geom_boxplot()+
    stat_summary(fun.y=mean, geom="point", shape=23, size=3)


ggplot(Health_w, aes(x=region, y=Gasto_saludPIBpor, color = region)) + 
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", shape=2, size=3)

ggplot(Health_w, aes(x=region, y=Gasto_saludPIBpor, fill = region)) + 
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", shape=2, size=3)


ggplot(Health_w, aes(x=region, y=Gasto_saludPIBpor, fill = region)) + 
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", shape=2, size=3)+
  labs(x="Region",  y="Gastos en Salud en % del PBI",color="Tipo")



pp <- Health_w %>%
  ggplot( aes(x=region, y=Gasto_saludPIBpor, labels = country_name, color=region)) +
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", shape=2, size=2)+
  labs(x="Region",  y="Gastos en Salud en % del PBI",color="Tipo")
  theme_bw()

ggplotly(pp)


# incluyendo 2 variables cualitativas

ggplot(Health_w, aes(x=region, y=Gasto_saludPIBpor, fill = Categ_desarrollo)) + 
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", shape=2, size=3)+
  labs(x="Region",  y="Gastos en Salud en % del PBI",color="Tipo")

library(plotly)

USPersonalExpenditure <- data.frame("Categorie"=rownames(USPersonalExpenditure), USPersonalExpenditure)
data <- USPersonalExpenditure[,c('Categorie', 'X1960')]

fig <- plot_ly(data, labels = ~Categorie, values = ~X1960, type = 'pie')
fig <- fig %>% layout(title = 'United States Personal Expenditures by Categories in 1960',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig
