library (readxl) # Permite leer archivos xlsx
library(dplyr) # Paquete para manipular datos
library(plotrix) # Paquete para manipular dibujos

IMCinfantil=read_excel("C:/.../IMCinfantil.xlsx")
# Importa la base con la cual se va a trabajar 

CatPeso <- IMCinfantil %>%
           pull(CatPeso) %>%
           plyr::mapvalues(c("D", "N", "OB", "SO"), 
            c("Deficiente", "Normal", "Obeso","Sobrepeso"))
# Cambia el nombre a campos categóricos de la variable CatPeso       
SEXO <- IMCinfantil %>%
        pull(SEXO) %>%
        plyr::mapvalues(c("M", "F"), c("Masc", "Fem"))
# Cambia el nombre a campos categóricos de la variable SEXO
          
IMCinfantil$CatPeso=CatPeso
IMCinfantil$SEXO=SEXO

interior <- IMCinfantil %>% group_by_(.dots=c("CatPeso")) %>%
            tally() %>% 
            mutate(porcent_abs=round(n/sum(n)*100, 2))
# Produce la tabla para la torta interior

exterior <- IMCinfantil %>% group_by_(.dots=c("CatPeso","SEXO")) %>%
            tally() %>% 
            mutate(porcent_rel=round(n/sum(n)*100, 2))%>%
            ungroup() %>% 
            mutate(porcent_abs=round(n/sum(n)*100, 2))
# Produce la tabla para la torta exterior

porcent_abs_ext=exterior$porcent_abs
tabla=table(exterior$CatPeso)[order(unique(exterior$CatPeso))]

colores=c("palegreen4", "paleturquoise4", "palevioletred4", "salmon3")
col_int=rep_len(colores, length(int_data$CatPeso))
col_ext=lapply(Map(rep, colores[seq_along(tabla)], tabla), 
               function(porcent_abs_ext) {
  al <- head(seq(0, 1, length.out = length(porcent_abs_ext)+2L)[-1L],-1L)
  Vectorize(adjustcolor)(porcent_abs_ext, alpha.f = al)}
) # Establece los colores

plot.new() # Borra gráficos anteriores

torta_ext=floating.pie(0.5,0.5, exterior$porcent_abs, radius=0.25, 
                     border="gray45",col=unlist(col_ext))
torta_int=floating.pie(0.5,0.5, interior$porcent_abs, radius=0.2, 
                       border="white",col=col_int)
# Produce los diagramas de tortas

pie.labels(x=0.5, y=0.5, torta_ext, paste0(exterior$SEXO, "\n",
           exterior$porcent_rel, " % - ", exterior$n, " ind."),
           minangle=0.2, radius=0.27, cex=0.6, font=1)
pie.labels(x=0.5, y=0.5, torta_int, paste0(interior$CatPeso, "\n",
           interior$porcent_abs, " % - ", interior$n, " ind."),
           minangle=0.2, radius=0.09, cex=0.6, font=1)
# Etiqueta las regiones
