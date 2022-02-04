library(forecast)
library(ISLR)
library(e1071)
#Ejercicio3
data(OJ)
nrow(OJ)
##Validacion cruzda
entrenamiento <- sample(1:nrow(OJ), 800)
validacion <- which(!1:nrow(OJ) %in% entrenamiento)
##Columnas de interes
columnas_interes <- which(!names(OJ) %in% c('StoreID'))
oj <- OJ[, columnas_interes]

##Ajuste clasificador de soporte vecotrial
svm_1 <- svm(Purchase~., cost=0.1, scale=FALSE, kernel='linear', subset=entrenamiento, data=oj)
summary(svm_1)


##tasas de error entranmiento y pruba
valores_entrenamiento1 <- svm_1$fitted
valores_validacion1 <- predict(svm_1, newdata=oj[validacion,])

error_entrenamiento1 <- sum(valores_entrenamiento1!=oj[entrenamiento,'Purchase'])/length(entrenamiento)
error_validacion1 <- sum(valores_validacion1!=oj[validacion,'Purchase'])/length(validacion)

##Tune
set.seed(123)
tune.out1 <- tune(svm, Purchase~., data=oj[entrenamiento,],kernel= "linear", ranges=list(cost=c(seq(0.001,10, by=0.5), 10)))
summary(tune.out1)

mejor_modelo1 <- tune.out$best.model
summary(mejor_modelo1)

##Errores de entrenamiento y prueba con nuevo modelo
valores_entrenamiento1 <- mejor_modelo1$fitted
valores_validacion1 <- predict(mejor_modelo1, newdata=oj[validacion,])

error_entrenamiento1 <- sum(valores_entrenamiento!=oj[entrenamiento,'Purchase'])/length(entrenamiento)
error_validacion1 <- sum(valores_validacion!=oj[validacion,'Purchase'])/length(validacion)

##Ajuste maquina de soporte vectorial con kernel radial
##Ajuste clasificador de soporte vecotrial
svm_2 <- svm(Purchase~., cost=0.1, scale=FALSE, kernel='Radial', subset=entrenamiento, data=oj)
summary(svm_2)


##tasas de error entranmiento y pruba
valores_entrenamiento2 <- svm_2$fitted
valores_validacion2 <- predict(svm_2, newdata=oj[validacion,])

error_entrenamiento2 <- sum(valores_entrenamiento2!=oj[entrenamiento,'Purchase'])/length(entrenamiento)
error_validacion2 <- sum(valores_validacion2!=oj[validacion,'Purchase'])/length(validacion)

##Tune
set.seed(123)
tune.out <- tune(svm, Purchase~., data=oj[entrenamiento,],kernel= "Radial", ranges=list(cost=c(seq(0.001,10, by=1), 10)))
summary(tune.out2)

mejor_modelo2 <- tune.out2$best.model
summary(mejor_modelo2)

##Errores de entrenamiento y prueba con nuevo modelo
valores_entrenamiento2 <- mejor_modelo2$fitted
valores_validacion2 <- predict(mejor_modelo2, newdata=oj[validacion,])

error_entrenamiento2 <- sum(valores_entrenamiento!=oj[entrenamiento,'Purchase'])/length(entrenamiento)
error_validacion2 <- sum(valores_validacion!=oj[validacion,'Purchase'])/length(validacion)

##Ajuste maquina de soporte vectorial con kernel polinomial
svm_3 <- svm(Purchase~., cost=0.1, scale=FALSE, kernel='polynomial', degree=2, subset=entrenamiento, data=oj)
summary(svm_1)


##tasas de error entranmiento y prueba
valores_entrenamiento3 <- svm_3$fitted
valores_validacion3 <- predict(svm_3, newdata=oj[validacion,])

error_entrenamiento3 <- sum(valores_entrenamiento3!=oj[entrenamiento,'Purchase'])/length(entrenamiento)
error_validacion3 <- sum(valores_validacion3!=oj[validacion,'Purchase'])/length(validacion)
##Tune utilizando varios ordenes de magnitud 
set.seed(123)
tune.out3 <- tune(svm, Purchase~., data=oj[entrenamiento,], scale=FALSE,  kernel='polynomial', degree=2, ranges=list(cost=c(0.01,0.1,1.0,5.0,10)))
summary(tune.out)

mejor_modelo3 <- tune.out3$best.model
summary(mejor_modelo3)
##Errores de entrenamiento y prueba
valores_entrenamiento3 <- mejor_modelo3$fitted
valores_validacion3 <- predict(mejor_modelo3, newdata=oj[validacion,])

error_entrenamiento3 <- sum(valores_entrenamiento3!=oj[entrenamiento,'Purchase'])/length(entrenamiento)
error_validacion3 <- sum(valores_validacion3!=oj[validacion,'Purchase'])/length(validacion)

