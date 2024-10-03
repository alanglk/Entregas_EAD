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
# B <- -1/2 * d.mds$x # Otra forma más sencilla
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
    theme_minimal()

# Proyección de las nuevas muestras
# x = 1/2 Λ^-1 X' (b -  δ^2)

proyeccion <- function(x_new){
  1/2 * diag(1 / diag(L[, 1:q])) %*% t(X[, 1:q]) %*% (diag(B) - x_new ^ 2)
}
new_x <-apply(dnuevos, 1, proyeccion)

# Representación de las muestras originales y las nuevas
new_x_df <- as.data.frame(new_x)
colnames(new_x_df) <- c("X", "Y")  
new_x_df$nivel <- NaN
df_new <- rbind(df_mds, new_x_df)

ggplot(df_new, aes(x = X, y = Y, color = nivel)) +
  geom_point(size = 3) +
  labs(title = "Proyección de datos originales y nuevos 2", x = "Q1", y = "Q2") +
  theme_minimal()



