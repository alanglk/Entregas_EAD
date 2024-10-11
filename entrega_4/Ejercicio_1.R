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

errores <- vector("list", length(N))
for (i in 1:length(N)){
  muestra<- crear_muestra(N[i], N0, N1)
  errores[i] <- clasificador_bayes(muestra, N0, N1)
}
errores
plot(1:length(N), errores, type = "b", ylim = c(0, 0.5))



