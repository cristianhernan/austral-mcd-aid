autos=mtcars[1:9,] # Toma las primeras nueve marcas de la base
row.names(autos)=c("Mazda", "Mazda Wag", "Datsun", "Hornet D", "Hornet S", 
                   "Valiant", "Duster", "Merc D", "Merc")
# Coloca etiquetas

stars(autos, full=F, cex=0.8, flip.labels=T, len=0.9, col.stars=cm.colors(9))
# Produce un diagrama de estrellas

