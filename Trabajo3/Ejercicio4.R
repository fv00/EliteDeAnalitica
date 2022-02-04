library(ggplot2)
library(class)
set.seed(123)
#Ejercicio4
## Generando los tres conjuntos de datos
d1 <- matrix(rnorm(20*50, sd= 0.8), ncol =50) + 0.5
d2 <- matrix(rnorm(20*50, sd=1), ncol =50)
d3 <- matrix(rnorm(20*50, sd= 1), ncol =50) - 0.6

datos <- rbind(d1, d2, d3)

##Creamos un vector de etiquetas para los datos
tag <- c(rep('A', 20), rep('B', 20), rep('C', 20))
tag <- as.factor(tag)

##Realizacion de PCA en las primeras dos componentes principales:
componentes <- prcomp(datos)
summary(componentes)
## Se observa que con las dos componentes sólo se logra explicar hasta el 28% de la variabialidad en los datos
componentes <- as.data.frame(componentes$x)

p <- ggplot(data=componentes, aes(x=PC1, y=PC2, col=tag))
p + geom_point()

##K-medias con k=3
k3 <- kmeans(datos, centers = 3)
resultados_k3 <- k3$cluster
comparacion <- table(tag, resultados_k3)
comparacion

p_k3 <- ggplot(data=componentes, aes(x=PC1, y=PC2, col=as.factor(resultados_k3)))
p_k3 + geom_point()


##K-medias con k=2
k2 <- kmeans(datos, centers = 2)
resultados_k2 <- k2$cluster
comparacion <- table(tag, resultados_k2)
comparacion

p_k2 <- ggplot(data=componentes, aes(x=PC1, y=PC2, col=as.factor(resultados_k2)))
p_k2 + geom_point()
# Se observa un "agrupamiento de los valores con media positiva y los valores con media negativa"

##k-medias con k=4
k4 <- kmeans(datos, centers = 4)
resultados_k4 <- k4$cluster
comparacion <- table(tag, resultados_k4)
comparacion

p_k4 <- ggplot(data=componentes, aes(x=PC1, y=PC2, col=as.factor(resultados_k4)))
p_k4 + geom_point()
##Se observa que el grupo compuesto de aquellas observaciones con media cero fue distribuido en 2 grupos

##K-medias con K=3 en las dos primerezas componentes Z1 y Z2
k_pca <- kmeans(componentes[,1:2], centers = 3)
resultados_kpca <- k_pca$cluster
comparacion <- table(tag, resultados_kpca)
comparacion

p_pca <- ggplot(data=componentes, aes(x=PC1, y=PC2, col=as.factor(resultados_kpca)))
p_pca + geom_point()
## Se observa un buen ajuste entre las clases ya que las componentes principales recogen más del de informacion

##kmeans con datos escalados:
## Escalamos los datos para que se tenga desviación estandar igual a 1:
datos_escalados <- scale(datos, center = FALSE, scale=TRUE)
## Se segmentan los datos mediante el algoritmo de kmeans
k_scaled <- kmeans(datos_escalados, centers = 3)
resultados_kscaled <- k_scaled$cluster
comparacion <- table(tag, resultados_kscaled)
comparacion

p_escaled <- ggplot(data=componentes, aes(x=PC1, y=PC2, col=as.factor(resultados_kscaled)))
p_escaled + geom_point()
## Se obtienen resultados diferentes a los obtenidos por el algoritmo de kmeans al segmentar los datos sin escalarlos