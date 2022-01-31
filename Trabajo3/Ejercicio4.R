library(ggplot2)
#Ejercicio4
d1 <- matrix(rnorm(20*50, mean = 15, sd=5), ncol =50)
d2 <- matrix(runif(20*50, min=0, max=10), ncol = 50)
d3 <- matrix(rnorm(20*50, mean=25, sd=4), ncol = 50)

datos <- rbind(d1, d2, d3)
tag <- c(rep('A', 20), rep('B', 20), rep('C', 20))
tag <- as.factor(tag)

componentes <- prcomp(datos)
summary(componentes)

componentes <- as.data.frame(componentes$x)
p <- ggplot(data=componentes, aes(x=PC1, y=PC2, col=tag))
p + geom_point()

