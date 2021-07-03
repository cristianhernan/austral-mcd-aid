# Guardamos los datos
A1=c(10,12,20)
A2=c(5,6,10)
A3=c(8,4,5)

# Calculamos los perfiles-fila
perf1=A1/sum(A1) 
perf2=A2/sum(A2)
perf3=A3/sum(A3)

totcol=A1+A2+A3 # guarda los totales por columna

# Calculamos las distancias Chi cuadrado entre perfiles fila
d12=sum((perf1-perf2)*totcol)
d13=sum((perf1-perf3)*totcol) 
d23=sum((perf2-perf3)*totcol)

# Colapsamos las dos primeras filas
AI=A1+A2
AII=A3
perfI=AI/sum(AI)
perfII=AII/sum(AII)
D12=sum((perfI-perfII)*totcol)
