fun=function(x,y) exp(-x^2-y^2)
# Define la funcion de distribución Normal Bivariada con ro=0

x=seq(-3,3,0.1)
y=x
# Asigna valores a las variables

persp(x, y, outer(x,y,fun), theta = -15, phi = 30, r = sqrt(3), d = 3,
      col="deepskyblue1", xlab = "x", ylab = "y",
      zlab ="z")
# Produce un dibujo de la Normal Bivariada

filled.contour(outer(x,y,fun), axes=TRUE, color.palette = topo.colors,
               frame.plot=FALSE, plot.axes=FALSE)
# Grafica las curvas de nivel de la Normal Bivariada
