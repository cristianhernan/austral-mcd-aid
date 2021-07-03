A=matrix(c(1,2,-1,1,0,1,3,1,0,0,2,0,0,0,1,-1),nrow=4,ncol=4,byrow=T)
# Ingresa una matriz de 4x4
A # Muestra la matriz

eigen(A)$values # Calcula los autovalores de A

## Comparar los siguientes cálculos:
sum(diag(A)) # Calcula la traza de A
sum(eigen(A)$values) # Calcula la suma de los autovalores de A

## Comparar los siguientes cálculos:
det(A) # Calcula el determinante de A
prod(eigen(A)$values) # Calcula el producto de los autovalores de A

t(A) # Calcula la traspuesta de A
sum(diag(t(A))) # Observar que las trazas de una matriz y su traspuesta son iguales
det(t(A)) # Observar que los determinantes de una matriz y su traspuesta son iguales
eigen(t(A))$values # Observar que los autovalores de una matriz y su traspuesta son los mismos

solve(A) # Calcula la inversa de A
A%*%solve(A) # verifica que son inversas
det(solve(A)) # Observar que los determinantes de una matriz y su inversa son inversos multiplicativos
eigen(solve(A))$values # Observar que los autovalores de una matriz y su traspuesta son inversos multiplicativos


eigen(A)$vectors # Calcula los autovectores de A
eigen(A)$vectors[,1] # Muestra el primer autovector
## Verifiquemos que es autovector de autovalor 2:
A%*%eigen(A)$vectors[,1]
2*eigen(A)$vectors[,1]

sqrt(sum(eigen(A)$vectors[,1]^2)) # Calcula la norma del primer autovector dado

