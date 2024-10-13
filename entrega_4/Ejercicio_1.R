#########################################################################
#                     Ejercicio 1. Laboratorio 1                        #
#########################################################################
# Definir las funciones
crear_muestra <- function(n, N0, N1) {
  # INPUT: número de muestras; N0 la distribución 0 y
  #        N1 la distribución 1 
  # OUTPUT: un vector de pares [(X, C), ....]
  resultados <- vector("list", n)
  
  for (i in 1:n) {
    # Generar una variable aleatoria binomial
    c <- rbinom(1, size = 1, prob = 0.5)
    if (c == 0) {
      x <- rnorm(1, mean = N0[1], sd = N0[2])
    } else {
      x <- rnorm(1, mean = N1[1], sd = N1[2])
    }
    
    # Agregar el par (X, C) a la lista de resultados
    resultados[[i]] <- c(x, c)
  }
  return(do.call(rbind, resultados))
}


clasificador_bayes <- function(muestra, N0, N1){
  error <- 0
  length(muestra[, 1])
  for (n in 1:length(muestra[, 1])){
    d1 <- dnorm(muestra[n, 1], mean = N0[1], sd = N0[2])
    d2 <- dnorm(muestra[n, 1], mean = N1[1], sd = N1[2])
    
    # a es la clase predecida
    a <- 0
    if (d2 > d1){
      a <- 1
    }
    
    error <- error +  abs(muestra[n, 2] - a)
  }
  error / length(muestra[, 1])
  #error
}

# Calcular el error experimental del clasificador
N0 <- c(0, 1)
N1 <- c(1, 1)
N <- c(10, 20, 50, 100, 500, 1000, 5000)
N <- seq(from = 1000, to = 10000, by = 100)

muestras <- vector("list", length(N))
errores <- vector("list", length(N))
for (i in 1:length(N)){
  muestras[[i]] <- crear_muestra(N[i], N0, N1)
  errores[[i]] <- clasificador_bayes(muestras[[i]], N0, N1)
}
errores
plot(N, unlist(errores), type = "l", col = "purple", ylim = c(0, 0.5),
      xlab = "Tamaño de la muestra", ylab = "Error", 
      main = expression("Error experimental de un clasificador bayesiano " ~ mu[0] ~ "= 0, " ~ mu[1] ~ "= 1 y " ~ sigma[0] ~ "=" ~ sigma[1] ~ "= 1"))
abline(h= mean(unlist(errores)), col = "red", lty = 2)
text(x = 0, y = mean(unlist(errores)) -0.02, labels = round(mean(unlist(errores)), 3), col = "red", pos = 4)
legend("topright", legend = c("Errores", "Media del error"), col = c("purple", "red"), lty = c(1, 2), bty = "n")


# ¿Qué pasa si las desviaciones típicas no coinciden?
N0 <- c(0, 1)
N1 <- c(0, 2)
N <- seq(from = 1000, to = 10000, by = 100)

muestras <- vector("list", length(N))
errores <- vector("list", length(N))
for (i in 1:length(N)){
  muestras[[i]] <- crear_muestra(N[i], N0, N1)
  errores[[i]] <- clasificador_bayes(muestras[[i]], N0, N1)
}
errores
plot(N, errores, type = "l", col = "purple", ylim = c(0, 0.5), 
      xlab = "Tamaño de la muestra", ylab = "Error", 
      main = expression("Error experimental de un clasificador bayesiano " ~ mu[0] ~ "=" ~ mu[1] ~ "= 0, " ~ sigma[0] ~ "= 1 y " ~ sigma[1] ~ "= 2"))
abline(h= mean(unlist(errores)), col = "red", lty = 2)
text(x = 0, y = mean(unlist(errores)) -0.02, labels = round(mean(unlist(errores)), 3), col = "red", pos = 4)
legend("topright", legend = c("Errores", "Media del error"), col = c("purple", "red"), lty = c(1, 2), bty = "n")

########## Grafica error analítico clasificador en  R ###################
# Definir el rango de x
x <- seq(-4, 4, length.out = 100)
F1 <- dnorm(x, mean = 0, sd = 1)  # Distribución normal F1
F2 <- dnorm(x, mean = 1, sd = 1)  # Distribución normal F2
plot(x, F1, type = "l", col = "red", lwd = 2, ylim = c(0, 0.45),
     xlab = "x", ylab = "y", main = "Error de clasificación", axes = FALSE) 
lines(x, F2, col = "blue", lwd = 2)
points(x = 1/2, y =  dnorm(1/2, mean = 0, sd = 1), pch=19, col = "red", bg = "red")
x_intersection <- seq(1/2, 4, length.out = 100)  # Rango donde F1 y F2 se cruzan
polygon(c(x_intersection, rev(x_intersection)), 
        c(pmin(dnorm(x_intersection, mean = 0, sd = 1), 
               dnorm(x_intersection, mean = 1, sd = 1)), 
          rep(0, length(x_intersection))), 
        col = rgb(1, 0, 0, 0.5), border = NA)
axis(1, at = seq(-4, 4, by = 0.5), las = 1)
axis(2, at = seq(0, 0.45, by = 0.05), las = 1)  
legend("topright", legend = c("F1 (N(0, 1))", "F2 (N(1, 1))", "Error"),
       col = c("red", "blue", rgb(1, 0, 0, 0.5)), lwd = 2, 
       pt.bg = c(NA, NA, rgb(1, 0, 0, 0.5)), pch = c(NA, NA, 22))

########## Extender para X perteneciente a R^d ##########################
library(MASS)

# Función generalizada de densidad normal multivariada para d dimensiones
densidad_normal_multivariada <- function(x, mu, sigma) {
  d <- length(mu)             # Dimensión del vector mu
  Sigma <- diag(sigma, d, d)  # Crear una matriz de covarianza diagonal con los valores sigma
  inv_Sigma <- solve(Sigma)   # Inversa de la matriz de covarianza
  det_Sigma <- det(Sigma)     # Determinante de la matriz de covarianza
  coef <- 1 / sqrt((2 * pi)^d * det_Sigma)  # Coeficiente de la fórmula

  # Calcular el exponente
  x_mu <- x - mu  # Diferencia entre el punto x y la media mu
  exponente <- -0.5 * t(x_mu) %*% inv_Sigma %*% x_mu  # Exponente de la fórmula

  # Densidad
  densidad <- coef * exp(exponente)
  
  return(densidad)
}

# Función generalizada para crear muestras multivariadas
crear_muestra_multivariada <- function(n, N0, N1, d) {
  resultados <- vector("list", n)
  
  for (i in 1:n) {
    # Generar una variable aleatoria binomial
    c <- rbinom(1, size = 1, prob = 0.5)
    
    if (c == 0) {
      # Generar una muestra multivariada con media N0 y varianza N0[2]
      x <- mvrnorm(n = 1, mu = rep(N0[1], d), Sigma = diag(N0[2], d, d))
    } else {
      # Generar una muestra multivariada con media N1 y varianza N1[2]
      x <- mvrnorm(n = 1, mu = rep(N1[1], d), Sigma = diag(N1[2], d, d))
    }
    
    # Agregar el par (X, C) a la lista de resultados
    resultados[[i]] <- c(x, c)
  }
  return(do.call(rbind, resultados))
}

# Clasificador Bayes generalizado para d dimensiones
clasificador_bayes_multivariada <- function(muestra, N0, N1, d) {
  error <- 0
  
  for (n in 1:nrow(muestra)) {
    # Extraer la muestra de d dimensiones
    x <- muestra[n, 1:d]
    
    # Calcular las densidades bajo N0 y N1
    d1 <- densidad_normal_multivariada(x, rep(N0[1], d), rep(N0[2], d))
    d2 <- densidad_normal_multivariada(x, rep(N1[1], d), rep(N1[2], d))
    
    # Clasificación: a es la clase predicha
    a <- ifelse(d2 > d1, 1, 0)
    
    # Sumar el error de clasificación
    error <- error + abs(muestra[n, d + 1] - a)
  }
  
  return(error / nrow(muestra))  # Devolver la tasa de error
}

# Calcular el error experimental del clasificador para distribuciones de 3 dimensiones
N0 <- c(0, 1)  # Media y varianza para la clase 0
N1 <- c(1, 1)  # Media y varianza para la clase 1
d <- 2         # Dimensión de las distribuciones

# Generar tamaños de muestra
N <- seq(from = 1000, to = 10000, by = 100)

# Crear las muestras y calcular los errores
muestras <- vector("list", length(N))
errores <- vector("list", length(N))

for (i in 1:length(N)) {
  muestras[[i]] <- crear_muestra_multivariada(N[i], N0, N1, d)
  errores[[i]] <- clasificador_bayes_multivariada(muestras[[i]], N0, N1, d)
}

# Mostrar los errores
errores

# Graficar la tasa de error
plot(N, unlist(errores), type = "l", col = "purple", ylim = c(0, 0.5),
      xlab = "Tamaño de la muestra", ylab = "Error", 
      main = "Clasificador bayesiano bivariable")
abline(h= mean(unlist(errores)), col = "red", lty = 2)
text(x = N[1], y = mean(unlist(errores)) -0.02, labels = round(mean(unlist(errores)), 3), col = "red", pos = 4)
legend("topright", legend = c("Errores", "Media del error"), col = c("purple", "red"), lty = c(1, 2), bty = "n")

########## Grafica error analítico clasificador en  R² ##################
if (d == 2){
  # Definir los parámetros
  x1_seq <- seq(-3, 4, length.out = 50)  # Valores de x
  x2_seq <- seq(-3, 4, length.out = 50)  # Valores de y
  grid <- expand.grid(x1 = x1_seq, x2 = x2_seq)
  
  # Calcular las funciones de densidad
  F1 <- apply(grid, 1, function(x){
            densidad_normal_multivariada(x, rep(N0[1], d), rep(N0[2], d))
        })
  F2 <- apply(grid, 1, function(x){
            densidad_normal_multivariada(x, rep(N1[1], d), rep(N1[2], d))
        })
  z1 <- matrix(F1, nrow = length(x1_seq), ncol = length(x2_seq))
  z2 <- matrix(F2, nrow = length(x1_seq), ncol = length(x2_seq))

  # Calcular la recta de corte (x1 + x2 = 1)
  x_cut <- seq(-3, 4, length.out = 100)
  y_cut <- 1 - x_cut  # y = 1 - x
  z_cut <- rep(0, length(x_cut)) # Asumir que Z = 0
  df_cut <- data.frame(x = x_cut, y = y_cut)
  F1_cut <- apply(df_cut, 1, function(x){
            densidad_normal_multivariada(x, rep(N0[1], d), rep(N0[2], d))
        })


  # Crear el gráfico 3D con plotly
  library(plotly)
  fig <- plot_ly(x = x1_seq, y = x2_seq, z = z1, type = 'surface', name = "F1(x)", colorbar=list(title='F1(x)'),  opacity = 0.5)
  fig <- fig %>% add_surface(z = z2, name = "F2(x)", colorbar=list(title='F2(x)'), opacity = 0.5)
  fig <- fig %>% add_trace(x = x_cut, y = y_cut, z = z_cut, type = 'scatter3d', mode = 'lines', line = list(color = 'red', width = 5), name = "Error")
  fig <- fig %>% add_trace(x = x_cut, y = y_cut, z = F1_cut, type = 'scatter3d', mode = 'lines', showlegend = FALSE, line = list(color = 'red', width = 5), name = "Error")

  # Ajustar el diseño del gráfico
  fig <- fig %>% layout(
    title = "Error de un clasificador Bayesiano en R²",
    scene = list(
      xaxis = list(title = "X1"),
      yaxis = list(title = "X2"),
      zaxis = list(title = "Densidad")
    ), 
    showlegend = TRUE
  )

  # Mostrar el gráfico
  fig
}


########## Calcular error analítico clasificador en  R^d ################
bayes_error_mahalanobis <- function(mu0, mu1, sigma) {
  # mu0 y mu1 son los vectores de medias (d dimensiones)
  # sigma es la matriz de covarianza común
  
  # Calcular la distancia de Mahalanobis entre las dos medias
  delta <- sqrt(t(mu1 - mu0) %*% solve(sigma) %*% (mu1 - mu0))
  
  # Calcular el error usando la función de distribución acumulada normal estándar
  error <- pnorm(-delta / 2)
  
  return(error)
}

# Mostrar el error
N <- seq(from = 1, to = 100, by = 1)
errores <- vector("list", length(N))

for (i in 1:length(N)) {
  mu0 <- rep(0, N[i]) # Media de la primera distribución
  mu1 <- rep(1, N[i]) # Media de la segunda distribución
  sigma <- diag(N[i])
  errores[i] <- as.numeric(bayes_error_mahalanobis(mu0, mu1, sigma))
}

plot(N, unlist(errores), type = "l", col = "purple",
      xlab = "Numero variables", ylab = "Error", 
      main = "Error analítico de un clasificador bayesiano multivariable")
legend("topright", legend = c("Errores"), col = c("purple"), lty = c(1), bty = "n")
