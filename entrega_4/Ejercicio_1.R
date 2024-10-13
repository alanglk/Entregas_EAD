#########################################################################
#                     Ejercicio 1. Laboratorio 1                        #
#########################################################################

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
plot(1:length(N), errores, type = "b", ylim = c(0, 0.5))


########## Extender para X perteneciente a R² ###########################
# Definir la función de densidad normal bivariada
densidad_normal_bivariada <- function(x1, x2, mu_x1, mu_x2, sigma) {
  (1 / (2 * pi * sigma^2)) * exp(-0.5 * ((x1 - mu_x1)^2 + (x2 - mu_x2)^2) / sigma^2)
}

# Definir los parámetros
x1_seq <- seq(-3, 4, length.out = 50)  # Valores de x
x2_seq <- seq(-3, 4, length.out = 50)  # Valores de y
grid <- expand.grid(x1 = x1_seq, x2 = x2_seq)

# Calcular las funciones de densidad
F1 <- densidad_normal_bivariada(grid$x1, grid$x2, mu_x1 = 0, mu_x2 = 0, sigma = 1)
F2 <- densidad_normal_bivariada(grid$x1, grid$x2, mu_x1 = 1, mu_x2 = 1, sigma = 1)
z1 <- matrix(F1, nrow = length(x1_seq), ncol = length(x2_seq))
z2 <- matrix(F2, nrow = length(x1_seq), ncol = length(x2_seq))

# Calcular la recta de corte (x1 + x2 = 1)
x_cut <- seq(-3, 4, length.out = 100)
y_cut <- 1 - x_cut  # y = 1 - x
z_cut <- rep(0, length(x_cut)) # Asumir que Z = 0
F1_cut <- densidad_normal_bivariada(x_cut, y_cut, mu_x1 = 0, mu_x2 = 0, sigma = 1)


# Crear el gráfico 3D con plotly
library(plotly)
fig <- plot_ly(x = x1_seq, y = x2_seq, z = z1, type = 'surface', name = "F1(x)", colorbar=list(title='F1(x)'),  opacity = 0.5)
fig <- fig %>% add_surface(z = z2, name = "F2(x)", colorbar=list(title='F2(x)'), opacity = 0.5)
fig <- fig %>% add_trace(x = x_cut, y = y_cut, z = z_cut, type = 'scatter3d', mode = 'lines', line = list(color = 'red', width = 5), name = "Recta de Corte")
fig <- fig %>% add_trace(x = x_cut, y = y_cut, z = F1_cut, type = 'scatter3d', mode = 'lines', showlegend = FALSE, line = list(color = 'red', width = 5), name = "Recta de Corte")

# Ajustar el diseño del gráfico
fig <- fig %>% layout(
  title = "Error de un clasificador Bayesiano en R²",
  scene = list(
    xaxis = list(title = "X1"),
    yaxis = list(title = "X2"),
    zaxis = list(title = "Error")
  ), 
  showlegend = TRUE
)

# Mostrar el gráfico
fig

