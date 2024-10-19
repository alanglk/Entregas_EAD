#########################################################################
#                     Ejercicio 4. Laboratorio 3                        #
#########################################################################
# Cargar librerias y fuentes
library("ggplot2")
library("showtext")
library("plotly")
showtext_auto()
font_add_google("Roboto", "roboto")

# Cargamos los datos
train_x = as.matrix(read.table("entrega_4/data/X_laboratorio_AAA.csv", header = FALSE, sep = ","))[, 1:2]
train_y = as.matrix(read.table("entrega_4/data/y_laboratorio_AAA.csv", header = FALSE))

########## Funciones recurrentes ########################################
show_loss <- function(loss, maintext){
  N <- length(loss)
  df_log_loss <- data.frame(Iteration = 1:N, loss = loss)
  ggplot(df_log_loss, aes(x = Iteration, y = loss)) +
      geom_hline(yintercept = loss[N], linetype = "dashed", color = "#ff00006e", size = 1) +
      geom_line(color = "#3970ed", size = 1) +
      annotate("text", x = -20, y = loss[N], label = round(loss[N], 4), color = "red")+
      coord_cartesian(xlim = c(0, N),  clip = 'off') +
      labs(title = maintext, x = "Iteración", y = "Log Loss Error") +
      theme_minimal(base_size = 15, base_family = "roboto") +
      theme( axis.line = element_line(color = "#8f8f8f", size = 0.8), axis.ticks = element_line(color = "#8f8f8f"), axis.text = element_text(color = "#8f8f8f"), plot.title = element_text(face = "bold"))
}

show_2D_data_clasificator <- function(maintext, w = NA){
  df_train <- data.frame(X1 = train_x[, 1], X2 = train_x[, 2], Y = train_y[, 1])
  df_train$Y <- as.factor(df_train$Y)
  p <- ggplot(df_train, aes(x = X1, y = X2, color = Y)) + 
    geom_point(size = 3) +
    scale_color_manual(values = c("-1" = "#08215a", "1" = "#3970ed")) +  # Definir los colores
    labs(title = maintext, x = "X1", y = "X2") +
    theme_minimal(base_size = 15, base_family = "roboto") +
    theme( axis.line = element_line(color = "#8f8f8f", size = 0.8), axis.ticks = element_line(color = "#8f8f8f"), axis.text = element_text(color = "#8f8f8f"), plot.title = element_text(face = "bold"))
  
  if (!missing(w)) {
    x <- seq(min(df_train["X1"]), max(df_train["X1"]), by = ( max(df_train["X1"]) - min(df_train["X1"]) ) / (nrow(df_train["X1"]) -1 ) )
    y <- cls$w[1, 1] * x + cls$w[2, 1]
    df_W <- data.frame(Wx = x, Wy = y)
    p <- p + geom_line(data = df_W, aes(x = x, y = y), color = "red", size = 1)  # Añadir la línea
  }

  p
}

show_3D_data_clasificator <- function(maintext, w = NA, b = 0){
  data <- data.frame(X = train_x_trans[, 1], Y = train_x_trans[, 2], Z = train_x_trans[, 3], Clase = train_y[, 1])
  fig <- plot_ly(showscale = FALSE)
  fig <- fig %>% add_markers(x = data$X[data$Clase == 1],  y = data$Y[data$Clase == 1], z = data$Z[data$Clase == 1],  marker = list(size = 7, color = "#563784", opacity = 1), name = "1")
  fig <- fig %>% add_markers(x = data$X[data$Clase == -1],  y = data$Y[data$Clase == -1], z = data$Z[data$Clase == -1],  marker = list(size = 7, color = "#87d961", opacity = 1), name = "-1")
  if (!missing(w)) {
    x_vals <- seq(min(data$X), max(data$X), length.out = 30)
    y_vals <- seq(min(data$Y), max(data$Y), length.out = 30)
    grid <- expand.grid(X = x_vals, Y = y_vals)

    if (length(w) == 3) {
      grid$Z <- (-w[1] * grid$X - w[2] * grid$Y - b) / w[3]
      Z_matrix <- matrix(grid$Z, nrow = length(x_vals), byrow = TRUE)
      fig <- fig %>% add_surface(x = x_vals, y = y_vals, z = Z_matrix, colorscale = list(c(0, 1), c("red", "red")), opacity = 0.5, name = "Clasificador")

    } else {
      stop("El vector de pesos 'w' debe tener exactamente 3 elementos.")
    }
  }

  # Ajustar el diseño del gráfico
  fig <- fig %>% layout(
    title = list(text = maintext, font = list(size = 24), y = 0.95),
    scene = list( xaxis = list(title = "X1"), yaxis = list(title = "X2"), zaxis = list(title = "X3") ),
    legend = list(
           title = list(text = 'Tipos'),
           font = list(size = 16)
    ),
    showlegend = TRUE
    )

  fig # Mostrar el gráfico
}

########## Clasificador por descenso del gradiente ######################
# Calcular w para los datos de entrenamiento
# w = argmin(1/n sum(log(1+e^ (-ywTx))) + landa * norma(w)²)
# Utilizando descenso del gradiente
entrenar_clasificador <- function(train_x, train_y, alpha, w = NA){
  # Iniciailizar w0
  # w <- c(-0.5, 0.5)
  if (missing(w)) {
    w <- runif(length(train_x[1, ]), min = -0.5, max = 0.5)
  }
  numiterations <- 200
  landa <- 1
  
  # vectores w por cada iteracion
  ws <- matrix(NA, nrow = 0, ncol = length(train_x[1, ]))
  ws <- rbind(ws, w)
  
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
    
    # Comprobar si los pesos han cambiado con la iteración
    # Si no hemos terminado
    w <- w_new
    ws <- rbind(ws, t(w))

    # Calcular el log loss
    log_loss[iter] <- mean(log(1 + exp(- train_y * train_x %*% w)))
    y_pred <- sign(train_x %*% w) # El clasificador es: h(x) = sign(wt * x)
    cro_one_loss[iter] <- mean(train_y * y_pred)
    accuracy <- sum(train_y == y_pred) / nrow(train_y)
    # cat("iter = ", iter, "W prev: ", w, "\n")
    
  }
  
  rownames(ws) <- 0:numiterations
  return(list(ws = ws, w = w, log_loss = log_loss, accuracy = accuracy))
}

########## Apartado 1 #########################################
show_2D_data_clasificator("Datos de entrenamiento") # Visualizar datos

# Entrenar el clasificador lineal
max_iterations <- 200
alpha <- 0.1 # Learning rate
cls <- entrenar_clasificador(train_x, train_y, alpha)

show_loss(cls$log_loss, "Evolución del Log Loss a lo largo de las iteraciones")
show_2D_data_clasificator("Datos de entrenamiento", w = cls$w) # Graficar el clasificador lineal

########## Apartado 2 #########################################
data_transformation <- function(x1, x2){
  return(x1 ^2 + x2 ^2 - 1/2)
}

# a) Transformar los datos
train_x_trans <- cbind(train_x, apply(train_x, 1, function(x){data_transformation(x[1], x[2]) }))
train_x_trans[, 3] <- scale(train_x_trans[, 3], center = TRUE)

# b) Plotear los datos en 3D
show_3D_data_clasificator("Datos transformados")

# c) Repetir el descenso del gradiente sobre los datos transformados
cls_trans <- entrenar_clasificador(train_x_trans, train_y, alpha)
show_loss(cls_trans$log_loss, "Log Loss datos transformados")

# d) Visualizar datos y plano del clasificador
show_3D_data_clasificator("Datos transformados", cls_trans$w)

########## Apartado 3 #########################################
# Funcion para calcular w a partir de c
W <- function(c, train_x){
  N <- length(train_x[, 1])
  d <- length(train_x[1, ])
  w <- rep(0, d)
  for (i in 1:N){
    w <- w + train_x[i, ] * c[i]
  }
  return(t(w))
}

# Clasificador iterando sobre las cs
entrenar_clasificador_ci <- function(train_x, train_y, alpha, c = NA){
  numIteraciones <- 200
  landa <- 1
  N <- length(train_x[, 1])
  d <- length(train_x[1, ])
  
  # Inicializar c
  if (missing(c)) {
    c <- runif(N, min = 0, max = 1)
  }

  # Calculamos w0
  w0 <- W(c, train_x)
  
  # vectores w por cada iteracion
  ws <- matrix(NA, nrow = 0, ncol = length(train_x[1, ]))
  ws <- rbind(ws, w0)
  
  # Metricas
  log_loss <- rep(0, numIteraciones)
  accuracy <- 0

  # Loop principal. Cada iteracion es un batch
  for (t in 1:numIteraciones){
    ct <- c
    
    # Definir ft(x)
    ft <- function(x){
      sum <- 0.0
      for (i in 1:N){
        sum <- sum + t(train_x[i, ]) %*% x * ct[i]
      }
      return( sum )
    }
    
    # Iteramos todo el batch (datos)
    for (i in 1:N){
      xi <- train_x[i, ]
      ci <- c[i]
      yi <- train_y[i]
      c[i] <- ci - alpha *  ( (1 / N) * (-yi / ( 1+ exp(yi * ft(xi)) ) ) + 2 * landa * ci)

    }
    
    # Calcular métricas y ws
    w <- W(c, train_x)
    ws <- rbind(ws, w)
    log_loss[t] <- mean(log(1 + exp(- train_y * train_x %*%  t(w) )))
    y_pred <- sign(train_x %*% t(w))
    accuracy <- sum(train_y == y_pred) / nrow(train_y)

  }

  # Calculamos w final
  w <- W(c, train_x)
  rownames(ws) <- 0:numIteraciones
  return( list(ws = ws, w = w, c = c, log_loss = log_loss, accuracy = accuracy) )
}

# Entrenar el clasificador iterando sobre las c's
c0 <- matrix(runif(length(train_x_trans[, 1]), min = 0, max = 1), ncol = 1)
w0 <- as.vector(W(c0, train_x_trans))

cls_c <- entrenar_clasificador_ci(train_x_trans, train_y, alpha, c0)
cls_w <- entrenar_clasificador(train_x_trans, train_y, alpha, w0)

# Graficar el log loss
show_loss(cls_c$log_loss, "Evolución del Log Loss iterando sobre cs")
show_loss(cls_w$log_loss, "Evolución del Log Loss iterando sobre ws")

show_3D_data_clasificator("Datos transformados iterando sobre cs", cls_c$w)
# TODO: Mostrar que si w0 es igual, da lo mismo iterar por ws o por cs. Los vectores de w deberían coincidir en cada iteración.

# b) Resolver el problema utilizando la hinge-loss
entrenar_clasificador_hingeloss <- function(train_x, train_y, alpha, w = NA, b = NA){
  # Iniciailizar w0
  # w <- c(-0.5, 0.5)
  if (missing(w)) {
    w <- runif(length(train_x[1, ]), min = -0.5, max = 0.5)
  }
  if (missing(b)) {
    b <- 0
  }
  numiterations <- 200
  landa <- 1
  
  # vectores w por cada iteracion
  ws <- matrix(NA, nrow = 0, ncol = length(train_x[1, ]))
  ws <- rbind(ws, w)

  # vectores b por cada iteracion
  bs <- matrix(NA, nrow = 0, ncol = 1)
  bs <- rbind(bs, b)
  
  # Métricas
  hinge_loss <- rep(0, numiterations)
  cro_one_loss <- rep(0, numiterations)
  accuracy <- 0
  
  # Subgradiente Hinge loss w
  Sw <- function(xi, yi, wt, bt){
      aux <- yi *( t(wt) %*% xi + bt)
      if (aux > 1) return( 0 )
      return(-yi * xi )
  }

  # Subgradiente Hinge loss b 
  Sb <- function(xi, yi, wt, bt){
      aux <- yi * (t(wt) %*% xi + bt)
      if (aux > 1) return( 0 )
      return(-yi)
  }

  # Función para calcular los pesos intermedios
  calcular_peso_bias <- function(X, Y, wt, bt, landa){
    wt <- as.matrix(wt)
    peso <- rep(0, length(wt))
    bias <- 0
    for (i in 1:nrow(X)){
      xi <- as.matrix(X[i, ])
      yi <- Y[i, ]
      
      peso <- peso + ( Sw(xi, yi, wt, bt) + 2 * landa * wt )
      bias <- bias + Sb(xi, yi, wt, bt)
    }
    return( list(peso = peso / nrow(X), bias = bias / nrow(X)) )
  }
  
  # Batch iterator
  for (iter in 1:numiterations){
    res <- calcular_peso_bias(train_x, train_y, w, b, landa)
    r <- alpha / sqrt(iter) # Factor de regularizacion
    w_new <- w - r * res$peso
    b_new <- b - r * res$bias
    
    # Actualizamos w y b
    w <- w_new
    b <- b_new
    ws <- rbind(ws, t(w))
    bs <- rbind(bs, b)

    # Calcular el hinge loss
    hinge_loss[iter] <- mean( max(c(0, 1 - train_y * (train_x %*% w + b))) ) 
    y_pred <- sign(train_x %*% w + b) # El clasificador es: h(x) = sign(wt * x + b)
    cro_one_loss[iter] <- mean(train_y * y_pred)
    accuracy <- sum(train_y == y_pred) / nrow(train_y)
    
  }
  
  rownames(ws) <- 0:numiterations
  rownames(bs) <- 0:numiterations
  return(list(ws = ws, w = w, bs = bs, b = b, hinge_loss = hinge_loss, accuracy = accuracy))
}


# Entrenar el clasificador hinge loss con biases
cls_h <- entrenar_clasificador_hingeloss(train_x_trans, train_y, alpha, w0, 0.4)
show_loss(cls_h$hinge_loss, "Evolución Hinge Loss")
show_3D_data_clasificator("Hinge loss", cls_h$w, cls_h$b)
