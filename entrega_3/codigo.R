# Ejercicio entregable 3 --------------------------------------------------

library(cluster)
library(ggplot2)

data <- read.csv("IndiceDelitos.csv", header=T, sep=" ")

distEuclidea <- dist(data, method="euclidean")
correlacion <- cor(t(data), method="pearson")
distCorrelacion <- as.dist(sqrt(1 - correlacion))

# (a) Clustering para agrupar municipios.
# Clustering jerárquico 
hc_euclidea_single <- hclust(distEuclidea, method = "single")
hc_euclidea_complete <- hclust(distEuclidea, method = "complete")
hc_euclidea_average <- hclust(distEuclidea, method = "average")
hc_euclidea_ward <- hclust(distEuclidea, method = "ward.D")

hc_correlacion_single <- hclust(distCorrelacion, method = "single")
hc_correlacion_complete <- hclust(distCorrelacion, method = "complete")
hc_correlacion_average <- hclust(distCorrelacion, method = "average")
hc_correlacion_ward <- hclust(distCorrelacion, method = "ward.D")

# Dibujar los dendrogramas
plot(hc_euclidea_single, main = "Dendrograma con distancia Euclídea y clustering jerarquico single", cex=0.9)
# abline(h = 300, col = "red", lwd = 2) # Solo si queremos dibujar la linea para 'separar' los clusters.
plot(hc_euclidea_complete, main = "Dendrograma con distancia Euclídea y clustering jerarquico complete", cex=0.9)
plot(hc_euclidea_average, main = "Dendrograma con distancia Euclídea y clustering jerarquico average", cex=0.9)
plot(hc_euclidea_ward, main = "Dendrograma con distancia Euclídea y clustering jerarquico ward", cex=0.9)

plot(hc_correlacion_single, main = "Dendrograma con distancia Correlación y clustering jerarquico single", cex=0.9)
plot(hc_correlacion_complete, main = "Dendrograma con distancia Correlación y clustering jerarquico complete", cex=0.9)
plot(hc_correlacion_average, main = "Dendrograma con distancia Correlación y clustering jerarquico average", cex=0.9)
plot(hc_correlacion_ward, main = "Dendrograma con distancia Correlación y clustering jerarquico ward", cex=0.9)

funcion_silhouette <- function(distEuclidea, distCorrelacion, hc_euclidea, hc_correlacion){
  num_clusters <- 2:20
  
  silhouette_avg_euclidea <- numeric(length(num_clusters))
  silhouette_avg_correlacion <- numeric(length(num_clusters))
  
  for (k in num_clusters) {
    cluster_assignments <- cutree(hc_euclidea, k = k)
    sil_euclidea <- silhouette(cluster_assignments, distEuclidea)
    silhouette_avg_euclidea[k - 1] <- mean(sil_euclidea[, 3])  
  }
  
  for (k in num_clusters) {
    cluster_assignments <- cutree(hc_correlacion, k = k)
    sil_correlacion <- silhouette(cluster_assignments, distCorrelacion)
    silhouette_avg_correlacion[k - 1] <- mean(sil_correlacion[, 3])  
  }
  
  df_silhouette <- data.frame(
    num_clusters = rep(num_clusters, 2),
    avg_silhouette = c(silhouette_avg_euclidea, silhouette_avg_correlacion),
    distancia = rep(c("Euclidea", "Correlación"), each = length(num_clusters))
  )
  df_silhouette
} 
 
s_max <- function(df_s, metodo){
  max_euclidea <- df_s[df_s$distancia == "Euclidea", ][which.max(df_s[df_s$distancia == "Euclidea", ]$avg_silhouette), ]
  max_correlacion <- df_s[df_s$distancia == "Correlación", ][which.max(df_s[df_s$distancia == "Correlación", ]$avg_silhouette), ]
  df_max <- rbind(max_euclidea, max_correlacion)
  df_max$metodo <- metodo
  df_max
}

df_s <- funcion_silhouette(distEuclidea, distCorrelacion, hc_euclidea_single, hc_correlacion_single)
df_s <- s_max(df_s, "single")
df_s

df_c <- funcion_silhouette(distEuclidea, distCorrelacion, hc_euclidea_complete, hc_correlacion_complete)
df_c <- s_max(df_c, "complete")
df_c

df_a <- funcion_silhouette(distEuclidea, distCorrelacion, hc_euclidea_average, hc_correlacion_average)
df_a <- s_max(df_c, "average")
df_a

df_w <- funcion_silhouette(distEuclidea, distCorrelacion, hc_euclidea_ward, hc_correlacion_ward)
df_w <- s_max(df_w, "ward")
df_w
#--------------------------------------------------------------------------
prE <- pam(distEuclidea, 2)
str(si1 <- silhouette(prE))
plot(si1) 
plot(si1, col = c("red", "green"))

prC <- pam(distCorrelacion, 9)
str(si2 <- silhouette(prC))
plot(si2, cex=0.7) 
plot(si2, col = c("red", "green", "blue", "lightblue", "yellow", "purple", "black", "white", "grey"))
#--------------------------------------------------------------------------
# Sacar tabla de a que cluster agrupan la euclidea de jerarquica average y euclidea pam
clusters_hclustE <- cutree(hc_euclidea_average, k = 2)
clusters_pamE <- prE$clustering

agrupamiento_df <- data.frame(
  Cluster_Hierarquico = clusters_hclustE,
  Cluster_PAM = clusters_pamE
)

# Visualizar la matriz de agrupamiento
print(agrupamiento_df)

table_cluster_comparison <- table(agrupamiento_df$Cluster_Hierarquico, agrupamiento_df$Cluster_PAM)
print(table_cluster_comparison)

# Cambiar: Sacar tabla de a que cluster agrupan la correlacion de jerarquica complete y correlacion pam
clusters_hclustC <- cutree(hc_correlacion_complete, k = 9)
clusters_pamC <- prC$clustering

agrupamiento_df2 <- data.frame(
  Cluster_Hierarquico = clusters_hclustC,
  Cluster_PAM = clusters_pamC
)

matriz_cluster_comparison <- matrix(0, nrow = 9, ncol = 9)

for (i in 1:nrow(agrupamiento_df2)) {
  hclust_cluster <- agrupamiento_df2$Cluster_Hierarquico[i]
  pam_cluster <- agrupamiento_df2$Cluster_PAM[i]
  
  matriz_cluster_comparison[hclust_cluster, pam_cluster] <- 
    matriz_cluster_comparison[hclust_cluster, pam_cluster] + 1
}
matriz_cluster_comparison_df <- as.data.frame(matriz_cluster_comparison)
rownames(matriz_cluster_comparison_df) <- paste("HClust", 1:9)
colnames(matriz_cluster_comparison_df) <- paste("PAM", 1:9)
print(matriz_cluster_comparison_df)
#--------------------------------------------------------------------------
# (b) Clustering K-means con k=2, por ejemplo
kmeans_euclidea <- kmeans(data, centers = 2, nstart = 20)

# Reducción de dimensionalidad con PCA.
pca_euclidea <- cmdscale(distEuclidea, k=2)
plot(pca_euclidea, col = kmeans_euclidea$cluster, main = "PCA - Distancia Euclídea")
silhouette()
# Clustering K-means con k=6, por ejemplo
kmeans_correlacion <- kmeans(as.matrix(distCorrelacion), centers = 6, nstart=1000)

# Reducción de dimensionalidad con MDS.
pca_correlacion <- prcomp(as.matrix(distCorrelacion), scale. = TRUE)
plot(pca_correlacion$x[,1:2], col = kmeans_correlacion$cluster, main = "PCA - Distancia Correlación")
