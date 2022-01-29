library(ISLR)

#Ejercicio3
data(OJ)
nrow(OJ)
##Validacion cruzda
entrenamiento <- sample(1:nrow(OJ), 800)
validacion <- which(!1:nrow(OJ) %in% entrenamiento)

#Ejercicio4


#Ejercicio5
data(USArrests)
