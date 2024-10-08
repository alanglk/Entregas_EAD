#########################################################################
#                Análisis de corrdenadas principales                    #
#########################################################################
# Representación según las q coordenadas principales de las observaciones 
# originales, con breve justificación para la elección del valor q. 
# Proyección de las nuevas muestras y su prepresentación conjunta en el 
# espacio anterior.

# Cargamos los datos
# d -> matriz de distancias entre las 211 muestras
# dnuevos -> matriz de distancias de los 28 nuevos 
#            elementos a los 211 anteriores
load("entrega_2/data/objetos.RData")
summary(d)

# Las distancias están comprendidas entre 0 y 1
# Realizamos el análisis de coordenadas principales
# Para este análisis, se supone que las distancias d son euclideas
d.mds <- cmdscale(d, k = 2, eig = TRUE, x.ret = TRUE)

# Mostramos los valores propios para decidir el número de coordenadas
d.mds$eig
plot(d.mds$eig, type="b", col="darkorchid3")
abline(h = 0, col = "blue")

# En el gráfico se puede deducir que las 6 primeras componentes son las más
# representativas, teniendo las 2 primeras la mayor variabilidad.
# q = 2 -> GOF = 0.918088
# q = 3 -> GOF = 0.9613737
# q = 4 -> GOF = 0.9837884
# q = 5 -> GOF = 0.9954371
# q = 6 -> GOF = 1
d.mds$GOF

# De esta manera, se va a considerar una reducción a 2 dimensiones, q = 2, ya
# que se consigue una buena representación de los datos y es sencillo realizar
# gráficos donde la tercera componente es el nivel de materia seca
q <-  2
Hn <- function(n){
  # Matriz de centrado (dim. n)
  unos <- matrix(rep(1, n), ncol=1)
  H <- diag(1, n) - unos%*%t(unos)/n
  return(H)
}
A <- (-1/2) * d ^ 2
B <- Hn(length(A[, 1])) %*% A %*% Hn(length(A[, 1]))
B <- -1/2 * d.mds$x # Otra forma más sencilla
B.eigen <- eigen(B)

# Proyección de las muestras originales
L <- diag(B.eigen$values)
X <- B.eigen$vector %*% sqrt(L)

# Representación de las muestras originales proyectadas sobre las q coordenadas
# principales
library(ggplot2)
df_mds <- as.data.frame(X[, 1:q])
df_mds$nivel <- clase
colnames(df_mds) <- c("X", "Y", "nivel")
ggplot(df_mds, aes(x = X, y = Y, color = nivel)) +
    geom_point(size = 3) +
    labs(title = "Proyección de datos originales", x = "Q1", y = "Q2") +
    theme_minimal() + theme_bw(base_size = 20)

# Proyección de las nuevas muestras
# x = 1/2 Λ^-1 X' (b -  δ^2)

proyeccion <- function(x_new){
  1/2 * diag(1 / diag(L[, 1:q])) %*% t(X[, 1:q]) %*% (diag(B) - x_new ^ 2)
}
new_x <- t(apply(dnuevos, 1, proyeccion))

# Representación de las muestras originales y las nuevas
new_x_df <- as.data.frame(new_x)
colnames(new_x_df) <- c("X", "Y")  
new_x_df$nivel <- NaN
df_new <- rbind(df_mds, new_x_df)

ggplot(df_new, aes(x = X, y = Y, color = nivel)) +
  geom_point(size = 3) +
  labs(title = "Proyección de datos originales y nuevos", x = "Q1", y = "Q2") +
  theme_minimal() + theme_bw(base_size = 20)


############# MEDIDAS DE BONDAD #############
# Medida global: Correlacion entre las distancias originales y distancias
#                en el espacio reducido
#
# 1. Tomamos la matriz de distancias de los nuevos elementos
# 2. Calculamos la matriz de distancias de los nuevos elementos proyectados
#    en el espacio reducido
# 3. Calculamos la correlación entre las dos matrices
# 4. Consideramos la media de las correlaciones como una medida de bondad
aux <- as.matrix(dist(df_new[, c("X", "Y")]))
dproy <- aux[212:239,1:211]
medida_global <- cor(dnuevos, dproy)
mean(medida_global)

# Medida local: fijado K para la vecindad del nuevo individuo, porcentaje de indivuos 
#               que coinciden en las vecindades marcadas por las distancias originales 
#               y por las distancias en el espacio de dimensión reducida.
#
# 1. Obtener los K primeros vecinos de los nuevos elementos
# 2. Obtener los K primeros vecinos de los nuevos elementos proyectados
# 3. Calcular el número de coincidencias entre los vecinos originales y los de las
#    proyecciones
# 4. Calcular la bondad local como coincidencias / K
# 5. Considerar la media de la bonanza local para cada uno de los elementos

obtener_vecinos <- function(dist_matrix, K){
  # Devuelve una matriz con los indices de los K vecinos más cercanos
  apply(dist_matrix, 1, function(fila){ order(fila)[1:K] })
}

K <- 5
vecinos_dnuevos <- obtener_vecinos(dnuevos, K)
vecinos_dproy <- obtener_vecinos(dproy, K)

bondad_local <- sapply(1:nrow(dnuevos), function(i) {
  # Calculamos las coincidencias por cada uno de los vecinos
  coincidencias <- length(intersect(vecinos_dnuevos[, i], vecinos_dproy[, i]))
  coincidencias / K
})
mean(bondad_local)

