# Ejercicio entregable 3 --------------------------------------------------
# Falta por hacer: Diferentes distancias para el jerarquico (min, max, avg, ward, etc.)
# Diferentes tecnicas de clustering, kmeans, hierarchical, ¿basado en densidad?

data <- read.csv("IndiceDelitos.csv", header=T, sep=" ")

# Para cada disimilaridad (distancia euclidea/ distancia correlación) utilizar tecnicas de clustering para agrupar municipios. Describe los argumentos.
distEuclidea <- dist(data, method="euclidean")

correlacion <- cor(t(data), method="pearson")
distCorrelacion <- as.dist(sqrt(1 - correlacion))

# Clustering jerárquico con la distancia euclídea
# TODO: Cambiar distancias, minima, complete, average, ward, etc.
hc_euclidea <- hclust(distEuclidea, method = "complete")

# Clustering jerárquico con la distancia basada en la correlación
hc_correlacion <- hclust(distCorrelacion, method = "complete")

# Dibujar los dendrogramas
plot(hc_euclidea, main = "Dendrograma con distancia Euclídea")
abline(h = 300, col = "red", lwd = 2)  # Línea horizontal para 2 clusters (ajustar el valor según el dendrograma)

plot(hc_correlacion, main = "Dendrograma con distancia Correlación")
abline(h = 0.225, col = "blue", lwd = 1)  # Línea horizontal para 8 clusters (ajustar el valor según el dendrograma)

library(cluster)

# Definir número de clusters a probar
num_clusters <- 2:20

# Matriz para almacenar los promedios del valor silhouette para cada número de clusters
silhouette_avg_euclidea <- numeric(length(num_clusters))
silhouette_avg_correlacion <- numeric(length(num_clusters))

# Calcular silhouette para clustering jerárquico con distancia euclídea
for (k in num_clusters) {
  cluster_assignments <- cutree(hc_euclidea, k = k)
  sil_euclidea <- silhouette(cluster_assignments, distEuclidea)
  silhouette_avg_euclidea[k - 1] <- mean(sil_euclidea[, 3])  # Promedio del valor silhouette
}

# Calcular silhouette para clustering jerárquico con distancia basada en la correlación
for (k in num_clusters) {
  cluster_assignments <- cutree(hc_correlacion, k = k)
  sil_correlacion <- silhouette(cluster_assignments, distCorrelacion)
  silhouette_avg_correlacion[k - 1] <- mean(sil_correlacion[, 3])  # Promedio del valor silhouette
}

# Graficar los resultados
library(ggplot2)
df_silhouette <- data.frame(
  num_clusters = rep(num_clusters, 2),
  avg_silhouette = c(silhouette_avg_euclidea, silhouette_avg_correlacion),
  distancia = rep(c("Euclidea", "Correlación"), each = length(num_clusters))
)

ggplot(df_silhouette, aes(x = num_clusters, y = avg_silhouette, color = distancia)) +
  geom_line() +
  geom_point(size = 3, shape = 21, fill = "white") +  # Puntos con borde negro y relleno transparente
  labs(title = "Promedio del Valor Silhouette para Distancias Euclídea y de Correlación",
       x = "Número de Clusters",
       y = "Valor Silhouette Promedio") +
  theme_minimal()
max(silhouette_avg_euclidea) # k=2
max(silhouette_avg_correlacion)
silhouette_avg_correlacion # k= 8

# Dibujar los dendrogramas
# Cortar en k=2 y plotear las de euclideas
# Cortar en k=8 y plotear las de correlacion
plot(hc_euclidea, main = "Dendrograma con distancia Euclídea")
abline(h = 300, col = "red", lwd = 2)  # Línea horizontal para 2 clusters (ajustar el valor según el dendrograma)

plot(hc_correlacion, main = "Dendrograma con distancia Correlación")
abline(h = 0.225, col = "blue", lwd = 1)  # Línea horizontal para 8 clusters (ajustar el valor según el dendrograma)

# Clustering K-means con k=3, por ejemplo
kmeans_euclidea <- kmeans(data, centers = 3, nstart = 10)
kmeans_correlacion <- kmeans(as.matrix(distCorrelacion), centers = 3, nstart=10)

# Reducción de dimensionalidad con PCA
pca_euclidea <- prcomp(data, scale. = TRUE)
plot(pca_euclidea$x[,1:2], col = kmeans_euclidea$cluster, main = "PCA - Distancia Euclídea")

pca_correlacion <- prcomp(as.matrix(distCorrelacion), scale. = TRUE)
plot(pca_correlacion$x[,1:2], col = kmeans_correlacion$cluster, main = "PCA - Distancia Correlación")
