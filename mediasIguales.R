# Escribir una funcion mediaColumnas que calcule la media de las columnas de un data.frame que se pasa como argumento 
# usando un bucle y las devuelva en un vector. La funcion debe comprobar que el argumento es un data.frame.
mediaColumnas <- function (df)
{
  if(class(df)=="data.frame")
  {
    medias <- numeric(ncol(df))
    names(medias) <- names(df)
    for(i in 1:ncol(df))
      medias[i] <- mean(df[,i])
    return (medias)
  }
  else
    print("Error. El argumento de la funcion ha de ser un data frame")
  
}

#Cargar el ﬁchero random.data en R
dframe <- read.table("random.data")

#Utilizar la funcion anterior para calcular la media de las columnas de los datos del ﬁchero.
medias1 <- mediaColumnas(dframe)

#Hacer la misma operacion anterior pero ahora con la funcion sapply.
medias2 <- sapply(dframe, mean)

#Comprobar que los resultados de los dos procedimientos anteriores son el mismo, y coincide tambien 
#con el resultado de usar la funcion colMeans.
error1 <- sum(abs(medias1 - medias2))/length(medias1)
medias3 <- colMeans(dframe)
error2 <- sum(abs(medias1 - medias3))/length(medias1)

error1
error2

#Tiempo ejecucion
system.time(mediaColumnas(dframe))
system.time(sapply(dframe, mean))
system.time(colMeans(dframe))