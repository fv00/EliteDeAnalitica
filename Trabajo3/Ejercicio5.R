#Ejercicio5
data(USArrests)

#Agrupamiento de datos
distancias_euclideanas <- dist(USArrests, method = 'euclidean')
clusters_completos <- hclust(distancias_euclideanas, method = 'complete')

plot(clusters_completos)

#Escalamiento de datos:
USArrests_escalado <- scale(USArrests, center = TRUE, scale = TRUE)

#Agrupamiento de datos:
distancias_escaladas <- dist(USArrests_escalado, method = 'euclidean')
clusters_escalados <- hclust(distancias_escaladas, method = 'complete')
#Grafica resultados
plot(clusters_escalados)

#Comparacion resultados
par(mfrow=c(1,2))
plot(clusters_completos)
plot(clusters_escalados)
