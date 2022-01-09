#Lectura de librerías
library(ISLR)
library(corrplot)
library(pls)


#Lectura de datos
data(College)

#Validacion cruzada
set.seed(123)
size = ceiling(nrow(College)*0.8)
training = sample(1:nrow(College), size)
test = which(!1:nrow(College) %in% training)

#Graficas descriptivas
cor = cor(training[2:18])
corrplot(cor)

#Ajuste modelo PLS
pls.fit <- plsr(formula = Apps ~ .,
                data = College,
                subset = training,
                scale = TRUE,
                validation = 'CV')

#Grafica de validacion
validationplot(pls.fit, val.type = 'RMSEP')

#Error de test:

#Ajuste modelo PCR
pcr.fit <- pcr(formula = Apps ~ .,
                data = College,
                subset = training,
                scale = TRUE,
                validation = 'CV')

#Grafica de validacion
validationplot(pcr.fit, val.type = 'RMSEP')

#Error de test:
