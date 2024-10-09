#########################################################################
#                     Ejercicio 2. Laboratorio 1                        #
#########################################################################

# Generamos el modelo y(x) = w^t * x + e.
# Necesitamos inicializar w
w0 <-runif(10, min = 0, max = 1)
Y <- function(x, w, e = 0){
  t(w) %*% x + e
}

# Generamos un conjunto de Train y Test
# Para ello, las primeras 5 componentes de manera uniforme entre [0, 1] y
# las siguientes 5 componentes de manera uniforme entre [100, 1000]

generar_muestra_ruido <- function(n, w) {
  # Inicializamos una matriz para almacenar las muestras
  muestras <- matrix(NA, nrow = n, ncol = 11)  # 10 dimensiones de x + 1 de y
  
  for (i in 1:n) {
    # Calculamos la muestra
    x1 <- runif(5, min = 0, max = 1)
    x2 <- runif(5, min = 100, max = 1000)
    x <- c(x1, x2)
    # Calculamos el resultado "real"
    y <- Y(x, w, rnorm(1, mean = 0, sd = 1)) # Muestra con ruido
    # Almacenamos la muestra en la fila correspondiente
    muestras[i, ] <- c(x, y)  # x en las primeras 10 columnas, y en la última
  }
  
  return(muestras)
}

train <- generar_muestra_ruido(100, w0)
train_x <- train[, 1:10]
train_y <- train[, 11]

test <- generar_muestra_ruido(1000, w0)
test_x <- test[, 1:10]
test_y <- test[, 11]

########## Regresion por minimos cuadrados ####################
# w* = (XT%*%X)⁻¹XTy
w1 <- solve(t(train_x) %*% train_x) %*% t(train_x) %*% train_y

# Calcular el error cuadratico
y_pred <- apply(test_x, 1, function(x) Y(x, w1))
error <- mean(test_y - y_pred)^2
error

# TODO: Calcular varios errores cuando se varia la muestra (cambiando w inicial)

######### Regresión por mínimos cuadrados penalizados #########
# wλ = (XT X + λI)⁻¹XT y
# landa <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) , ncol = 1)

calcular_error <- function(landa) {
  w2 <- solve(t(train_x) %*% train_x + landa * diag(nrow = ncol(train_x))) %*% t(train_x) %*% train_y
  y_pred <- apply(test_x, 1, function(x) Y(x, w2))
  error <- mean((test_y - y_pred)^2)  # Error cuadrático
  return(error)
}

landas <- seq(0, 100, by = 0.1)
errores <- sapply(landas, calcular_error)

plot(landas, errores, type = "l", col = "blue", xlab = "Valores de lambda", ylab = "Error",  main = "Gráfica de Errores vs. Lambda")
# Buscar la landa óptima
i <- which.min(errores)
points(landas[i], errores[i], pch = 19, col = "red", bg = "red")  # Punto rojo
legend("topright", legend = c("Errores", "Error mínimo"), col = c("blue", "red"), pch = c(NA, 19), lty = c(1, NA))

######### Extensión ###########################################
X <- rbind(train_x, test_x)
x_svd <- svd(X)
# Agregamos inestabilidad multiplicando los valores propios menos significativos por 10 ^-2
x_svd$d[(length(x_svd$d) -2):length(x_svd$d)] <- x_svd$d[(length(x_svd$d) -2):length(x_svd$d)] * 10 ^(-2)
X_inestable <- x_svd$u %*% diag(x_svd$d) %*% t(x_svd$v)

y_sintetico1 <- apply(X_inestable, 1, function(x) Y(x, w0, rnorm(1, mean = 0, sd = 1)) )
y_sintetico2 <- apply(X_inestable, 1, function(x) Y(x, w0, rnorm(1, mean = 0, sd = 1)) )

# Los vectores de pesos son bastante distintos 
w_inestable1 <- solve(t(X_inestable) %*% X_inestable) %*% t(X_inestable) %*% y_sintetico1
w_inestable2 <- solve(t(X_inestable) %*% X_inestable) %*% t(X_inestable) %*% y_sintetico2
