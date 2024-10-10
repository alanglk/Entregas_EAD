#########################################################################
#                     Ejercicio 4. Laboratorio 3                        #
#########################################################################
# Cargamos los datos
train_x = as.matrix(read.table("entrega_4/data/X_laboratorio_AAA.csv", header = FALSE, sep = ","))[, 1:2]
train_y = as.matrix(read.table("entrega_4/data/y_laboratorio_AAA.csv", header = FALSE))

########## Clasificador por descenso del gradiente ######################
# Calcular w para los datos de entrenamiento
# w = argmin(1/n sum(log(1+e^ (-ywTx))) + landa * norma(w)²)
# Utilizando descenso del gradiente

entrenar_clasificador <- function(train_x, train_y, alpha){
  # Iniciailizar w0
  w <- c(-0.5, 0.5)
  numiterations <- 200
  landa <- 1
  
  # Métricas
  log_loss <- rep(0, numiterations)
  cro_one_loss <- rep(0, numiterations)
  accuracy <- 0
  
  # Función para calcular los pesos intermedios
  calcular_peso <- function(X, Y, wt, landa){
    wt <- as.matrix(wt)
    aux <- rep(0, length(wt))
    for (i in 1:nrow(X)){
      xi <- as.matrix(X[i, ])
      yi <- Y[i, ]
      
      aux <- aux + ( (-yi * xi) / as.numeric((1 + exp(yi * t(wt) %*% xi))) + 2 * landa * wt )
    }
    return( aux / nrow(X) )
  }
  
  # Batch iterator
  for (iter in 1:numiterations){
    w_new <- calcular_peso(train_x, train_y, w, landa)
    w_new <- w - alpha * w_new
    
    # Calcular el log loss
    log_loss[iter] <- mean(log(1 + exp(- train_y * train_x %*% w)))
    y_pred <- sign(train_x %*% w) # El clasificador es: h(x) = sign(wt * x)
    cro_one_loss[iter] <- mean(train_y * y_pred)
    accuracy <- sum(train_y == y_pred) / nrow(train_y)
    # cat("iter = ", iter, "W prev: ", w, "\n")
    
    # Comprobar si los pesos han cambiado con la iteración
    # Si no hemos terminado
    w <- w_new
  }
  
  return(list(w = w, log_loss = log_loss, accuracy = accuracy))
}

########## Apartado 1 #########################################
max_iterations <- 200
alpha <- 0.1 # Learning rate
cls <- entrenar_clasificador(train_x, train_y, alpha)
plot(1:length(cls$log_loss), cls$log_loss, type = "l", col = "blue")

# Graficar el clasificador lineal
x <- seq(-10, 10, by = 0.1)
y <- cls$w[1, 1] * x + cls$w[2, 1]

plot(x, y, type = "l", col = "blue", lwd = 2, xlab = "x", ylab = "y", main = "Recta con pesos w")
points()
grid()  # Agregar una cuadrícula



