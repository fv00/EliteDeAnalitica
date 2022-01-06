#Lectura de librerías
library(ISLR)
library(corrplot)

#Lectura de datos
data(College)

#Validacion cruzada
set.seed(123)
size = ceiling(nrow(College)*0.8)
training = sample(1:nrow(College), size)
test = College[!1:nrow(College) %in% training,]
training = College[training,]

#Graficas descriptivas
cor = cor(training[2:18])
corrplot(cor)
plot(PhD)
