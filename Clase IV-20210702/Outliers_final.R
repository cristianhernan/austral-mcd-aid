
#limpio la memoria
rm( list=ls() )
gc()


library(readxl) # leer archivos excel
library(dplyr) #cuartiles
library(ggplot2) 
library(ggExtra) #mejorar grafico
library(nortest) #normalidad
library(expss) 
library(TMB)
library(plotly)
library(tidyverse)
library(readxl)
library(treemap)
library(questionr)
library(corrplot)
library(GGally)
library(plotly)
library(MASS)
library(DMwR2)


#Diagnostico de Outliers

winDialog("okcancel","Seleccionar el archivo de pesos de niños al nacer")
bweight <- read_excel(file.choose())
dim(bweight)
summary(bweight)


setwd("C:/Users/valer/Documents/Maestria-Profesora/AID/2021/Clase IV")
bweight <- read_excel("bweight.XLS")
summary(bweight)
# los valores de weight menores a 600 grms son outliers pero por error
# se deben eliminar solamente en el caso en que hay outliers es mi variable objetivo y son claramente por error

# outliers por error (SOLO si es error pueden ser eliminados de la base)

# deciles, cuartiles, centiles

centil <- seq(0, 1, 0.01)
quantile(bweight$weight, centil,na.rm = TRUE)



bweight <- mutate(bweight, weight = ifelse((weight < 600), NA, weight))


# Contar los missing
sum(is.na(bweight$weight))



bweight <- mutate(bweight, married = ifelse((married == 9 ), NA , married))
bweight <- mutate(bweight, smoke = ifelse((smoke == 9 ), NA , smoke))

summary(bweight)

pp <- bweight %>%
  ggplot( aes(x=as.factor(boy), y=weight, color=as.factor(boy))) +
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", shape=2, size=2)+
  labs(x="Genero",  y="Peso de ninios al nacer",color="Tipo")
theme_bw()

ggplotly(pp)


centil <- seq(0, 1, 0.01)
quantile(bweight$weight, centil,na.rm = TRUE)


# Ver outliers de forma unidimensional 

ggplot(data=bweight)+
  geom_histogram(aes(x=weight))

ggplot(data=bweight)+
  geom_histogram(aes(x=weight),bins=40)



# Distancia de Gauss

# scale es estandarizar la variable

# Generar un vector boleano indicando los valores que esten a una distancia de más de 2 desviaciones estándar de la media
bweight<- mutate(bweight,weight_abs = abs(scale(bweight$weight)))
bweight <- mutate(bweight,m_tgain_abs = abs(scale(bweight$m_wtgain)))

# Visualizar el gráfico con los datos destacando sus outlier 

bweight2 <- bweight %>%
  rownames_to_column(var="id")


ggplot(data=bweight2, aes(x=weight,y=m_wtgain))+
  geom_point()+
  geom_label(data=bweight2 %>% filter(weight_abs > 2.5 | m_tgain_abs > 2.5), # Filter data first
             aes(label=id))


ggplot(data=bweight2, aes(x=weight,y=weight_abs))+
  geom_point()+
  geom_label(data=bweight2 %>% filter(weight_abs > 2.5), # Filter data first
             aes(label=id))

ggplot(data=bweight2, aes(x=m_wtgain,y=m_tgain_abs))+
  geom_point()+
  geom_label(data=bweight2 %>% filter(m_tgain_abs > 2.5), # Filter data first
             aes(label=id))



gc <- ggplot(bweight, aes(x=weight, y=cigsper)) + 
  geom_point(aes(col=as.factor(boy))) 
labs(subtitle="Peso vs Edad de la madre (estandarizada)", 
     y="Cantidad de cigarrillos por dia", 
     x="Peso", 
     title="Scatterplot", 
     caption = "Source: midwest")
#ggMarginal(ggc, type = "histogram", fill="transparent")
ggMarginal(ggc, type = "boxplot", fill="transparent")


plot(gc)

g <- ggplot(bweight, aes(weight, cigsper)) + 
  geom_count() + 
  geom_smooth(method="lm", se=F)

ggMarginal(g, type = "histogram", fill="transparent")
ggMarginal(g, type = "boxplot", fill="transparent")





# si ordenamos la base bweight2 por las variables estandarizadas vemos los puntos de corte 


#Distancia de Mahalanobis

# solo tenemos que usar variables cuantitativas

#selecciono solo las variables numericas
idx <-  c(1,5,7,8)

# creo un subconjunto solo con las variables numericas
bweight_num <- subset(bweight, select = idx )

bweight_num <- bweight_num[complete.cases(bweight_num), ]

#selecciono solo 500 registros

bweight_500 <- sample(1:nrow(bweight_num),size=500,replace=FALSE)
bweight_muestra <- bweight_num[bweight_500, ]

cov <- cov.rob(bweight_muestra, method = "classical", nsamp = "best")

center1 <- apply(bweight_muestra, 2,mean)

center2 <- apply(bweight_muestra, 2,median)

# Iniciamos iteraction

dcov = 0

for(i in 1:65){
  dcov[i]=mahalanobis(bweight_muestra[i,],cov$center, cov$cov, inverted =FALSE)
}
#local outlier factors using the LOF algorithm

distancia.outliers = lofactor(bweight_muestra , k=5)
print(distancia.outliers)

outliers=order(distancia.outliers, decreasing = T)[1:5]

print(outliers)

bweight_muestra2 <- cbind(bweight_muestra,distancia.outliers)
bweight_muestra2 <- mutate(bweight_muestra2, id=as.numeric(rownames(bweight_muestra2)))

ggplot(bweight_muestra2, aes(x=id, y=distancia.outliers)) +
  geom_point() + 
  geom_segment( aes(x=id, xend=id, y=0, yend=distancia.outliers))+
  geom_label(data=bweight_muestra2 %>% filter(distancia.outliers > 2), # Filter data first
             aes(label=id))


# distancia de Cook
rg<-lm(weight~cigsper,data=bweight_muestra)
summary(rg)
cooksd <- cooks.distance(rg)


plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels




# Test de normalidad de una variable

#shapiro.test(bweight$weight) # < 5000 registros


lillie.test(x = bweight$weight)


# Test de homogeneidad de variancia

var.test(weight ~ boy, alternative='two.sided', conf.level=.95, 
         data=bweight)

# Test de comparacion de muestras

t.test(weight~boy, alternative='two.sided', conf.level=.95, 
       var.equal=TRUE, data=bweight)


# Test de Kruskall Wallis

kruskal.test(weight~boy, data = bweight) 


# Test chi cuadrado

bweight <- mutate(bweight, weight_cat = ifelse((weight < 2200 ), "bajo peso" , "peso normal"))

sjt.xtab(bweight$weight_cat, bweight$boy, show.row.prc = TRUE,show.col.prc = TRUE)










