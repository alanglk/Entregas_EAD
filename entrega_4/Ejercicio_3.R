#########################################################################
#                     Ejercicio 3. Laboratorio 2                        #
#########################################################################

# Definición del modelo
modelo <- function(x){
  # x pertenece a R2
  3 * x[1]^2 -2*x[1]*x[2] + 2 * x[2]^2 + rnorm(1, mean = 0, sd = 1)
}

# Función para generar una muestra
generar_muestra <- function(n) {
  # Genera x1 y x2 como variables aleatorias (0 o 1)
  x1 <- runif(n, min = 0, max = 1)
  x2 <- runif(n, min = 0, max = 1)
  
  # Calcula y usando la función modelo
  y <- mapply(modelo, list(cbind(x1, x2)))
  
  # Crea la matriz de resultados
  resultado <- cbind(x1, x2, y)
  colnames(resultado) <- c("x1", "x2", "y")
  
  return(resultado)
}

# "Then, find two transformations Φ of the data into a space R p where a line
# can perfectly interpolate all the points (i.e., achieve zero error)".

# Función para comprobar los datos transformados 
es_lineal <- function(x_train, y_train){
  threshold <- 1e-5
  # Regresor lineal -> Devuelve w: los pesos asociados a X
  if (nrow(x_train) > ncol(x_train)){
    print("n > d")
    w <- solve(t(x_train) %*% x_train) %*% t(x_train) %*% y_train
  }else{
    print("n < d")
    w <- t(x_train) %*% solve(x_train %*% t(x_train)) %*% y_train
  }
  y_pred <- x_train %*% w
  error <- mean((y_train - y_pred)^2)
  cat("Error: ", error, "\n")
  cat("w: ", w, "\n")
  error < threshold
}

# Creamos una muestra cualquiera de 10 elementos
train <- generar_muestra(10)
x_train <- train[, 1:2]
y_train <- train[, 3]

es_lineal(x_train, y_train)
# Transformamos los datos de dos formas distintas
# Grado 1: X1, X2
# Grado 2: X1, X2, X1², X2², X1X2
# Grado 3: X1, X2, X1², X2², X1X2, X1³, X2³, X1²X2, X1X2²
# Grado 4:
x_aux <- poly(x_train, degree = 4)
es_lineal(x_aux, y_train)

# Otra transformación distinta
fi <- function(X){
  X1 <- X[, 1]
  X2 <- X[, 2]
  cbind(X1, X2, X1^2, X2^2, X1^3, X2^3, X1^4, X2^4, X1 * X2, sqrt(X1))
}
x_aux <- fi(x_train)
es_lineal(x_aux, y_train)
# El número mínimo de dimesión d para la transformación y que exista una recta que
# pase por todos los puntos de la muestra es n o número de elementos de la muestra.
# Esto tiene todo el sentido, porque de esta forma se consigue un sistema compatible determinado
# Siempre y cuando todos los features de X sean linealmente independientes (no son combinaciones)
# lineales unos de los otros.

# Es por ello que aplicar transformaciones de los datos es útil siempre y cuando
# el dataset sea de pocos elementos. En el momento que n empieza a ser grande, esto
# significa buscar una transformación a dimensión n y, por lo tanto, multiplicaciones
# de matrices muy grandes. Aquí es donde reside la potencia del Kernel Trick y
# el porqué no se transforman directamente los datos.

# Hayar una forma de calcular infinitas transformaciones para p dimensiones
# Random Fourier Features:
#   https://gregorygundersen.com/blog/2019/12/23/random-fourier-features/
#

# Función para generar Random Fourier Features
random_fourier_features <- function(X, num_features, scale = 1) {
  num_dimensions <- ncol(X)
  
  # Generar pesos aleatorios (W) y sesgos (b)
  W <- matrix(rnorm(num_dimensions * num_features, mean = 0, sd = 1 / scale), 
                nrow = num_dimensions)
  b <- runif(num_features, 0, 2 * pi)
  # Calcular la transformación
  Z <- cos(X %*% W + b)
  
  return(Z)
}

# Apartado 2
n <-20
train <- generar_muestra(n)
x_train <- train[, 1:2]
y_train <- train[, 3]

p <- 20
x_aux <-random_fourier_features(x_train, p)
es_lineal(x_aux, y_train)


########## Apartado 3 #########################################
calcular_errores <- function(train, test, p_values){
  x_train <- train[, 1:2]
  y_train <- train[, 3]
  x_test <- test[, 1:2]
  y_test <- test[, 3]
  
  errores <- numeric(length(p_values)) # Inicializamos los errores
  
  for (i in 1:length(p_values)){
    # Transformamos los datos con Random Fourier Features
    W <- matrix(rnorm(ncol(x_train) * p_values[i], mean = 0, sd = 1), nrow = ncol(x_train))
    b <- runif(p_values[i], 0, 2 * pi)
    x_aux_train <- cos(x_train %*% W + b)
    x_aux_test <-  cos(x_test  %*% W + b)
    
    # Entrenar el modelo con el train
    if (nrow(x_aux_train) > ncol(x_aux_train)){
      w <- solve(t(x_aux_train) %*% x_aux_train) %*% t(x_aux_train) %*% y_train
    }else{
      w <- t(x_aux_train) %*% solve(x_aux_train %*% t(x_aux_train)) %*% y_train
    }
    
    if ( p_values[i] == 10){
      cat("w: ", w)
    }
    if ( p_values[i] == 15){
      cat("w: ", w)
    }
    if ( p_values[i] == 20){
      cat("w: ", w)
    }
    if ( p_values[i] == 25){
      cat("w: ", w)
    }
    if ( p_values[i] == 30){
      cat("w: ", w)
    }
    
    # Evaluar el modelo con el test
    y_pred <- x_aux_test %*% w
    errores[i] <- mean((y_test - y_pred)^2)
    cat("Para p = ", p_values[i], " el error es: ", errores[i], "\n")
  }
  return(errores)
}

#p <- c(5, 10, 20, 30, 50)
p <- 2:50
#p <- seq(from = 3, to = 50, by =3)
train <- generar_muestra(20) # Generar el Train set
test <- generar_muestra(1000) # Generar el Test set
errores <- calcular_errores(train, test, p)
plot(p, errores, type = "l", col = "blue", xlab = "p", ylab = "Error", main = "Error con test en P dimensiones")

# Cuando la dimensión es pequeña, el modelo no se puede sobre-ajustar tanto a los datos
# Es por ello, que cuando la dimensión aumenta se produce un modelo que pasa por los todos puntos
# de entrenamiento, y conforme la dimensión es mayor este sobre ajuste se hace más evidente.
# para p = 20, w es muy grande. Esto significa un overfitting máximo.
# Para el resto de los casos no hace tanto overfiting. Ver las w con p = 20 y con p != 20
