#**************************************
#*        titanic.csv                 *
#**************************************

#Leemos el fichero como un dataframe
titanic.df <- read.table("titanic.csv", header = TRUE, sep = ',', row.names = "PassengerId")
head(titanic.df)

#Calculamos el porcentaje de pasajeros que sobrevivio
mean(titanic.df$Survived) * 100
 
#Calcular el porcentaje de missing values en cada uno de los atributos
colMeans(is.na(titanic.df)) * 100

#Eliminamos la variable cabin del dataframe
titanic.df$Cabin <- NULL
head(titanic.df)

#Completamos los missing values del atributo age con la mediana del resto de datos
titanic.df <- mutate(titanic.df, Age = ifelse(is.na(Age), median(Age, na.rm = T), Age))
head(titanic.df)

# Calcular la probabilidad de supervivencia en base al genero
titanic.df %>% group_by(Sex) %>% summarise(mean(Survived))
#Con aggregate
aggregate(titanic.df$Survived, list(titanic.df$Sex), mean)
#Conclusion: sobrevivieron muchas mas mujeres que hombres. Entiendo que se debe al hecho de que las
#mujeres tenian prioridad para acceder a los botes salvavidas y los hombres se dedicaban a labores de 
#rescate.

#Calcular la probabilidad de supervivencia en base a la edad
titanic.df %>% group_by(Age) %>% summarise(mean(Survived))
#Los resultados presentados de esta forma no son facilmente interpretables porque aparece por 
#cada edad diferente. Seria mejor representarlo por intervalos de edad.

#Crea una nueva variable decade en el dataframe que contenga la decada de la edad de los pasajeros y 
#repite el analisis del apartado g) sobre esta nueva variable
titanic.df <- titanic.df %>% mutate(Decade = 10 * floor(Age / 10))
head(titanic.df)
titanic.df %>% group_by(Decade) %>% summarise(mean(Survived))
count(titanic.df, Decade)
#Conclusion: la tasa de supervivientes es más o menos igual independientemente de la edad, salvo en 
#el caso de niños pequeños y los mayores de setenta. En el caso de los niños me imagino que tambien 
#tendrian prioridad para acceder a los botes. Entre los mayores de setenta no sobrevivio ninguno
#excepto el único pasajero con edad octagenaria.

#Calcula la probabilidad de supervivencia en base a la clase del billete del pasajero (pclass)
titanic.df %>% group_by(Pclass) %>% summarise(mean(Survived))
#Conclusion: la tasa de supervivencia fue mayor cuanto mejor era la clase. Los camarotes de las clases
#altas estan localizadas en la parte alta del barco. Ademas, tendrian prioridad a la hora de ser
#colocados en los botes salvavidas.

#Combina en una tabla el an´alisis de la probabilidad de supervivencia en base al sexo y clase 
#del billete del pasajero
titanic.df %>% group_by(Sex,Pclass) %>% summarise(mean(Survived))
#Conclusion: las mujeres en primera y segunda clase sobrevivieron la gran mayoria y casi la mitad de
#de las mujeres en tercera clase. En cuanto a hombres, hubo más del doble de supervivientes entre
#los que viajaban en primera que en segunda o tercera, pero aun asi, muy inferior al porcentaje
#mujeres.

#**********************************************************
#*             diabetes.data                              *
#**********************************************************

#Cargamos y eliminamos missing values.
diabetes.df <- read.table("diabetes.data", header = T, na.strings = "-9999.0")
diabetes.df <- diabetes.df[complete.cases(diabetes.df),]
head(diabetes.df)

#Vemos el tipo de cada una de las variables
str(diabetes.df)

#Hacemos analisis estadistico
apply(diabetes.df[-c(1,2,5,10,11)], 2, summary)
apply(diabetes.df[-c(1,2,5,10,11)], 2, var)
#Conclusion: las variables tienen rangos muy diferentes, asi como la dispersion de los datos es
#tambien muy distinta. Hay variables que parecen tener outliers y otras no.

#Hacemos un grafico de cajas
boxplot(diabetes.df$BMI)
boxplot(diabetes.df$BP)
boxplot(diabetes.df$S2)
boxplot(diabetes.df$S3)
boxplot(diabetes.df$S4)
boxplot(diabetes.df$S5)

# Calcular la media para las ﬁlas que tienen SEX=M y la media para las ﬁlas que tienen SEX=F, 
#utilizando la funcion tapply
factor <- diabetes.df$SEX
sapply(diabetes.df[,-2], function (x) tapply(x, factor, mean))

#Correlacion de variables numericas con la variable Y
cor(diabetes.df[,-c(1,2,5,10,11)], diabetes.df$Y)

#Dispersion
plot(diabetes.df$Y, diabetes.df$BMI) #alta correlacion
plot(diabetes.df$Y, diabetes.df$S2) #baja correlacion
#Para dos vectores con correlacion uno, la grafica de dispersion seria una linea recta de puntos

#Transformacion de variable SEX en numerica
diabetes.df <- diabetes.df %>% mutate(SEX = ifelse((SEX == "M"), 1, 2))
head(diabetes.df)

#Eliminamos outliers
medianas <- apply(diabetes.df[-c(1,2,5,10,11)], 2, median)
desviaciones <- apply(diabetes.df[-c(1,2,5,10,11)], 2, mad)
outliers <- apply(diabetes.df[-c(1,2,5,10,11)], 1, 
                  function(x)
                    sum(((x > (medianas + 3 * desviaciones)) | (x < (medianas - 3 * desviaciones)))))
diabetes.df <- diabetes.df[-which(outliers != 0),]
nrow(diabetes.df)

#Separamos en dos subconjuntos disjuntos grande (70%) y pequeño(30%)
n <- nrow(diabetes.df) * 0.7
grande <- sample_n(diabetes.df, n)
index <- as.character(sort(as.numeric(row.names(grande))))
grande <- grande[index,]
pequeño <- diabetes.df[-which(row.names(diabetes.df) %in% index),]
head(grande)
nrow(grande)
head(pequeño)
nrow(pequeño)

#Escalamos los datos
escalado <- apply(diabetes.df[-c(1,2,5,10,11)], 2, function(x) (x-mean(x))/sd(x))
diabetes.df[c(3,4,6,7,8,9)] <- escalado
head(diabetes.df)

#Escalamos el subset pequeño con la media y la desviacion tipica del subset grande

for(i in c(3,4,6,7,8,9)){
  media.grande <- mean(grande[,i])
  desv.grande <- sd(grande[,i])
  pequeño[,i] <- (pequeño[,i] - media.grande)/desv.grande
  
}
head(pequeño)

