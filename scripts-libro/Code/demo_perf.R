library(readxl)
demo <- read_excel("C:/Users/Usuario/Dropbox/Libro Análisis de datos/Data/demo.xlsx")

ggplot(demo, aes(x = Variables, y = Medias, colour = Grupo)) + 
  geom_line() 
  