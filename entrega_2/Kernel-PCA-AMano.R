# Ejercicio entregable 2 --------------------------------------------------
# En d estan las distancias de los datos para el entrenamiento 
# En dnuevos las distancias de los datos para el test (hay que asignarles una clase)
# clase tiene la clase de d.

load("objetos/objetos.RData")

#kernel-componentes

# Aplicar KPCA
library(ggplot2)
library(gridExtra)

# Primer KPCA
sigma1 <- 0.05
q <- 2

K_gaussian <- function(d, sigma) {
  exp(-d^2 / (2 * sigma^2))
}

K1 <- K_gaussian(d, sigma1)
K1F <- prcomp(K1, center=T)

df1 <- data.frame(PC1 = K1F$x[,1], PC2 = K1F$x[,2], Clase = clase)

plot1 <- ggplot(df1, aes(x = PC1, y = PC2, color = Clase)) +
  geom_point() +
  ggtitle("KPCA Resultados (sigma = 0.05)") +
  xlab("Componente Principal 1") +
  ylab("Componente Principal 2")

# Segundo KPCA
sigma2 <- 0.5
K2 <- K_gaussian(d, sigma2)
K2F <- prcomp(K2, center=T)

df2 <- data.frame(PC1 = K2F$x[,1], PC2 = K2F$x[,2], Clase = clase)

plot2 <- ggplot(df2, aes(x = PC1, y = PC2, color = Clase)) +
  geom_point() +
  ggtitle("KPCA Resultados (sigma = 0.5)") +
  xlab("Componente Principal 1") +
  ylab("Componente Principal 2")

# Mostrar los gráficos uno al lado del otro
grid.arrange(plot1, plot2, ncol = 2)

# Sacar las proyecciones
Aq1 <- K1F$rotation[, 1:2]
Aq2 <- K2F$rotation[, 1:2]

K_centrado <- function(dnuevos, K_original, sigma){
  K_nuevo <- K_gaussian(dnuevos, sigma)
  K_original_mean <- mean(K_original)
  row_means_original <- rowMeans(K_original)
  col_means_original <- colMeans(K_original)
  K_new_centered <- matrix(0, nrow = nrow(K_nuevo), ncol = ncol(K_nuevo))
  
  for (i in 1:nrow(K_nuevo)) {
    for (j in 1:ncol(K_nuevo)) {
      K_new_centered[i, j] <- K_nuevo[i, j] -
        mean(K_nuevo[i, ]) - 
        row_means_original[j] + 
        K_original_mean
    }
  }
  K_new_centered
} 

KC1 <- K_centrado(dnuevos, K1, sigma1)
KC2 <- K_centrado(dnuevos, K2, sigma2)

x1 <- KC1 %*% Aq1
x2 <- KC2 %*% Aq2

emb1 <- as.data.frame(x1)
colnames(emb1) <- c("PC1", "PC2")
emb1$Clase <- "nuevos"

emb2 <- as.data.frame(x2)
colnames(emb2) <- c("PC1", "PC2")
emb2$Clase <- "nuevos"
# Graficos con d y dnuevos
df1_comb <- rbind(df1, emb1)

plot1 <- ggplot(df1_comb, aes(x = PC1, y = PC2, color = Clase)) +
  geom_point(data = df1, aes(x = PC1, y = PC2, color = Clase)) +  # Puntos originales
  geom_point(data = emb1, aes(x = PC1, y = PC2, color = Clase), shape = 20, size = 2, stroke = 1) +  # Puntos de predicción
  ggtitle("KPCA Resultados (sigma = 0.05)") +
  xlab("Componente Principal 1") +
  ylab("Componente Principal 2")

df2_comb <- rbind(df2, emb2)

plot2 <- ggplot(df2_comb, aes(x = PC1, y = PC2, color = Clase)) +
  geom_point(data = df2, aes(x = PC1, y = PC2, color = Clase)) +
  geom_point(data = emb2, aes(x = PC1, y = PC2, color = Clase), shape = 20, size = 2, stroke = 1) +  # Puntos de predicción
  ggtitle("KPCA Resultados (sigma = 0.5)") +
  xlab("Componente Principal 1") +
  ylab("Componente Principal 2")
grid.arrange(plot1, plot2, ncol = 2)

# Medidas de bondad
# Medida global: Correlación entre distancias originales y la distancia en el espacio de dimension reducido del nuevo al resto.
# Calcular la distancia
distancias1 <- as.matrix(dist(df1_comb[,1:2]))
medidaG1 <- cor(dnuevos, distancias1[212:239,1:211])
mean(medidaG1)

distancias2 <- as.matrix(dist(df2_comb[, 1:2]))
medidaG2 <- cor(dnuevos, distancias2[212:239, 1:211])
mean(medidaG2)

# Medida local (K=5)
# De los datos nuevos que puntos estan mas cerca y sacar sus clases (Con dnuevos y las de dimension reducida).
# Sacar el porcentaje en que coinciden.
k <- 5
Obtener_vecinos <- function(distancias){
  # La distancias de los nuevos a los datos de d.
  matriz_resultado <- matrix(NA, nrow = 28, ncol = k)
  
  for (i in 1:nrow(distancias)) {
    indices_pequenos <- order(distancias[i, ])[1:k]
    matriz_resultado[i, ] <- indices_pequenos
  }
  matriz_resultado
}

ml1er <- Obtener_vecinos(distancias1[212:239,1:211])
ml1dn <- Obtener_vecinos(dnuevos)
ml1er
ml1dn

ml2er <- Obtener_vecinos(distancias2[212:239,1:211])
ml2dn <- Obtener_vecinos(dnuevos)
ml2er
ml2dn

medida_local <- function(mler, mldn){
  comp <- numeric(28)
  for (i in 1:nrow(mler)) {
    coincidencias <- sum(mler[i, ] %in% mldn[i, ])
    comp[i] <- (coincidencias / ncol(mler)) * 100
  }
  comp
}

#Medias de la medida_local
vmedia1 <- medida_local(ml1er, ml1dn)
mean(vmedia1)

vmedia2 <- medida_local(ml2er, ml2dn)
mean(vmedia2)

vmedia1
vmedia2

