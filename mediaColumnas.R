# El siguiente script contiene tres funciones que reciben como argumento un dataframe. Se calcula la media de las columnas del
# dataframe de tres formas diferentes (mediante un bucle, mediante la funcion sapply y mediante la funcion colMeans) y se
# comprueba que las tres operaciones dan el mismo resultado y se comparan los tiempos de ejecucion en realizar el computo de 
# las medias de las tres formas.


  # Funcion mediaCols
  # Toma como argumento un data frame, comprueba si es de tipo dataframe y, si lo es, calcula la media de sus columnas 
  # mediante un bucle y las devuelve en un vector. Si no lo es, muestra por pantalla un mensaje de error
  mediaCols <- function (df)
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
  # Funcion tiempoEjecucion
  # Toma como argumento un dataframe y muestra por pantalla el tiempo de ejecucion en calcular la media de sus columnas de 
  # tres formas diferentes: mediante la funcion mediaCols, la funcion sappy y la funcion colMeans
  tiempoEjecucion<- function(df)
  {
    print("Las dimensiones del data frame son:")
    print(dim(df))
    print("El tiempo de ejecucion de calcular la media de las columnas de un dataframe usando un bucle es:")
    print (system.time(mediaCols(df)))
    print("El tiempo de ejecucion de calcular la media de las columnas de un dataframe usando la funcion 'sapply' es:")
    print(system.time(sapply(df, mean)))
    print("El tiempo de ejecucion de calcular la media de las columnas de un dataframe usando la funcion 'colMeans' es:")
    print(system.time(colMeans(dataframe)))
  }
  
  # Funcion mediasIguales 
  # Compara las medias devueltas por las tres funciones (mediaCols, sapply y colMeans) e informa de si son iguales 
  mediasIguales <- function(df)
  {
    comp <- (mediaCols(df)==sapply(df, mean))&(mediaCols(df)==colMeans(df)) 
    if (sum(comp)==ncol(df))
      print("Las medias son iguales")
    else
      print("Las medias no son iguales")
  }

