#Crear matriz A de dimensiones 4x5 y con valores del 1 al 20 (ambos incluidos)
A = matrix(1:20, 4, 5, TRUE)

#Extraer los elementos A[4,3],A[3,4],A[2,5] utilizando una matriz de indices
idx = array(c(4,3,2,3,4,5), dim=c(3,2))
A[idx]

#Reemplazar dichos elementos con 0
A[idx] = 0

#Crear la matriz identidad 5×5
I = diag(nrow=5,ncol=5)         #Alternativamente I = diag(1,5,5)

#Convertir la matriz A anterior en una matriz cuadrada B añadiendo al ﬁnal una ﬁla de unos (funcion rbind())
B = rbind(A,1)  

#Calcular la inversa de la matriz B con la funcion solve()
InvB = solve(B)

#Multiplicar B por su inversa B−1. 
B%*%InvB                          

#Comprobar si el resultado es exactamente la matriz identidad I.
sum((B%*%InvB) == I)== length(I)

#En caso contrario, calcular el “error” o “precision” de la operacioon
error = sum(abs(B%*%InvB - I))/length(B)
