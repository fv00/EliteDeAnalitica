#Ejercicio5
data(USArrests)

#Agrupamiento de datos

distancias_euclideanas <- dist(USArrests, method = 'euclidean')
clusters_completos <- hclust(distancias_euclideanas, method = 'complete')

plot(clusters_completos)

cortes_completos <- cutree(clusters_completos, k=3)
##Estados pertenecientes al grupo1:
cortes_completos[cortes_completos==1]

##Estados pertenecientes al grupo2:
cortes_completos[cortes_completos==2]

##Estados pertenecientes al grupo3:
cortes_completos[cortes_completos==3]

#Escalamiento de datos:
USArrests_escalado <- scale(USArrests, center = FALSE, scale = TRUE)

#Agrupamiento de datos:
distancias_escaladas <- dist(USArrests_escalado, method = 'euclidean')
clusters_escalados <- hclust(distancias_escaladas, method = 'complete')
#Grafica resultados
plot(clusters_escalados)

#Comparacion resultados
par(mfrow=c(1,2))
plot(clusters_completos)
plot(clusters_escalados)
