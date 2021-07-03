par(mfrow=c(1,3)) # Permite realizar diagramas conjuntos

hist(iris$Sepal.Length, nclass=4, prob=TRUE, ylab="Densidad", 
     col="lightsteelblue", border="lightsteelblue4",
     xlab="Longitud del sépalo", main="4 clases") 
    
hist(iris$Sepal.Length, nclass=30, prob=TRUE, ylab="Densidad",
     col="lightsteelblue", border="lightsteelblue4",
     xlab="Longitud del sépalo", main="30 clases") 
    
hist(iris$Sepal.Length, breaks='FD', prob=TRUE, ylab="Densidad",
     col="lightsteelblue", border="lightsteelblue4",
     xlab="Longitud del sépalo", main="Freedman-Diaconis") 
