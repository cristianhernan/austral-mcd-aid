library(readxl) # Permite leer archivos xlsx
library(ggplot2) # Paquete para confeccionar dibujos

prostata=read_excel("C:/.../prostata.xlsx")
# Importa la base con la cual se va a trabajar

prostata$Rotura=as.factor(prostata$Rotura)

ggplot(prostata, aes(x=Rotura, y=PSA, fill=Rotura)) +
  geom_boxplot() +
  xlab("") +
  scale_fill_brewer(palette="Pastel1", name="Rotura capsular",
                    breaks=c("0", "1"), labels=c("no","sí")) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
# Produce boxplots  

modelo_logistico=glm(Rotura~PSA, data=prostata, family="binomial")
# Construiye el modelo logístico con respuesta binomial
plot(prostata$PSA, prostata$Rotura, col="royalblue", xlab="PSA",
        ylab = "Probabilidad de rotura capsular")
curve(predict(modelo_logistico, data.frame(PSA=x), type="response"),
        add=TRUE, col="violet", lwd=2.5)
# Produce un gráfico con las variables de interés y la curva predictiva
summary(modelo_logistico)
# Realiza una síntesis del modelo

predicciones=ifelse(test=modelo_logistico$fitted.values>0.5, yes=1, no=0)
# Establece como punto de corte 0.5
matriz_confusion=table(prostata$Rotura, predicciones,
                      dnn = c("Observaciones", "Predicciones"))
# Calcula la matriz de confusión
mean(prostata$Rotura==predicciones) 
# Calcula la tasa de buena clasificación

ggplot(prostata, aes(x=Rotura, y=Gleason, fill=Rotura)) +
  geom_boxplot() +
  xlab("") +
  scale_fill_brewer(palette="Pastel1", name="Rotura capsular",
                    breaks=c("0", "1"), labels=c("no","sí")) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
# Produce boxplots  

# Réplica para la variable Gleason
modelo_logist=glm(Rotura~Gleason, data=prostata, family="binomial")
summary(modelo_logist)

predic=ifelse(test= modelo_logist$fitted.values>0.5, yes=1, no=0)
matriz_confus=table(prostata$Rotura, predic, 
                    dnn=c("Observaciones", "Predicciones"))
mean(prostata$Rotura==predic)

# Réplica para ambas variables en conjunto
modelo_logconjunto=glm(Rotura~PSA+Gleason, data=prostata, family = "binomial")
summary(modelo_logconjunto)
prediccionesconj=ifelse(test=modelo_logconjunto$fitted.values>0.5, yes=1, 
                        no=0)
matriz_confconjunta=table(prostata$Rotura, prediccionesconj,
                            dnn = c("Observaciones", "Predicciones"))
mean(prostata$Rotura==prediccionesconj)
