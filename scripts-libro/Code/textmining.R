library(tm) # Entorno para análisis de text mining
library(wordcloud) # Permite visualizar nube de palabras
library(wordcloud2) # Permite más opciones para nubes de palabras
library(devtools) # Colección de herramientas de desarrollo para paquetes
library(magrittr) # Proporciona un mecanismo para encadenar comandos
library(ggplot2) # Paquete para confeccionar dibujos
library(dendextend) # Ofrece un conjunto de funciones para dendogramas

texto=readLines('C:/Users/Usuario-PC/Dropbox/LibroAnalisisdatos/Data/poemas.txt', 
                skip=0, n=-1)
# Lee el texto completo pues n=-1
str(texto)
# Muestra la estructura interna

## Eliminamos caracteres especiales en español
texto=gsub("á", "a", texto) 
texto=gsub("é", "e", texto)
texto=gsub("í", "i", texto)
texto=gsub("ó", "o", texto)
texto=gsub("ú", "u", texto)
texto=gsub("ñ", "ni", texto)

docs=Corpus(VectorSource(texto)) 
# Crea el corpus; es decir, el acervo de documentos a analizar
inspect(docs)
# Muestra el documento

tab=content_transformer(function (x, pattern) gsub(pattern, " ", x))
# Función definida para reemplar caracteres especiales por espacios

## Limpiamos el documento
docs=tm_map(docs, tab, "¡")
docs=tm_map(docs, tab, "¿")
docs=tm_map(docs, content_transformer(tolower))
# Convierte todo a minúscula
docs=tm_map(docs, removeNumbers)
# Elimina números
docs=tm_map(docs, removeWords, stopwords("spanish"))
# Elimina palabras con poco valor para el análisis como preposiciones o interjecciones
docs=tm_map(docs, removePunctuation)
# Elimina signos de puntuación
docs=tm_map(docs, stripWhitespace)
# Elimina los espacios vacíos excesivos

## Calculamos valores de los términos del documento
dtm=TermDocumentMatrix(docs)
m=as.matrix(dtm)
v=sort(rowSums(m), decreasing=TRUE)
d=data.frame(word=names(v), freq=v)
head(d, 10)

## Armamos la nube de palabras
set.seed(1234) # Fija una semilla
wordcloud(words=d$word, freq=d$freq, min.freq=1, max.words=200, 
          random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Set1"))

## Mejoramos la nube removiendo palabras que no son relevantes y
## unificando palabras con la misma relavancia

docs.nuevo=tm_map(docs, removeWords, c("alla","alli","aqui","asi","aun",
                                       "hace","hacia","tan","todas","veces",
                                       "vez","voy"))
docs.nuevo=tm_map(docs.nuevo, content_transformer(function(x) 
  gsub(x, pattern="olas", replacement="ola")))
docs.nuevo=tm_map(docs.nuevo, content_transformer(function(x) 
  gsub(x, pattern="triste", replacement="tristeza")))
docs.nuevo=tm_map(docs.nuevo, content_transformer(function(x) 
  gsub(x, pattern="tristes", replacement="tristeza")))
docs.nuevo=tm_map(docs.nuevo, content_transformer(function(x) 
  gsub(x, pattern="solo", replacement="solo")))

## Generamos la nueva nube de palabras
dtm.nuevo=TermDocumentMatrix(docs.nuevo)
m.nuevo=as.matrix(dtm.nuevo)
v.nuevo=sort(rowSums(m.nuevo), decreasing=TRUE)
d.nuevo=data.frame(word=names(v.nuevo), freq=v.nuevo)
set.seed(4321) # Fija una semilla
wordcloud(words=d.nuevo$word, freq=d.nuevo$freq, min.freq=1, max.words=80, 
          random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Set1")) 

wordcloud2(data=d.nuevo, color="random-light", backgroundColor="black") 
# Produce una nube de palabras animada

## Construimos una nueva nube con argumentos distintos
m.nuevo <- m.nuevo %>% rowSums(m.nuevo) %>% sort(decreasing=TRUE)
# Proporciona las sumas de renglones odenadas de mayor a menor
m.nuevo=data.frame(palabra=names(m.nuevo), frec=m.nuevo)
# Arreglo con la frecuencia de cada palabra
m.nuevo=m.nuevo[m.nuevo$frec>4,]
# Selecciona las palabras con frecuencia superior a 4

## Armamos un histograma de frecuencias
m.nuevo[1:20, ] %>%
  ggplot(aes(palabra, frec)) +
  geom_bar(stat='identity', color='royalblue', fill='lightblue1') +
  coord_flip() +
  geom_text(aes(hjust=1.3, label=frec), size=3, color='royalblue') +
  labs(x='Palabra', y='Número de usos')

## Generamos un dendograma
dtm.clus=removeSparseTerms(dtm.nuevo, sparse=.98)
# Remueve palabras dispersas
dtm.clus %>% scale %>% dist() %>% hclust(method="complete") %>% as.dendrogram() -> dend                                     
# Aplica el criterio completo a las variables estandarizadas
dend %>% 
  set("branches_k_color", k=12) %>%
  # Personaliza las ramas
  set("labels_col", k=12) %>% set("labels_cex", 0.8) %>%
  # Personaliza las etiquetas
  plot(axes=TRUE)
# Produce un dendograma personalizado

## Otras funciones
findFreqTerms(dtm.nuevo, lowfreq=15) 
# Muestra las palabras con frecuencia superior a 15
findAssocs(dtm.nuevo, terms=c("alma","ojos","cuerpo","amo","corazon",
                                   "noche", "viento"), corlimit=.3)
# Calcula la asociación entre términos frecuentes