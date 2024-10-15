#########################################################################
#                     Ejercicio 4. Laboratorio 3                        #
#########################################################################
# Cargar librerias y fuentes
library("ggplot2")
library("showtext")
showtext_auto()
font_add_google("Roboto", "roboto")

# Cargamos los datos
train_x = as.matrix(read.table("entrega_4/data/X_laboratorio_AAA.csv", header = FALSE, sep = ","))[, 1:2]
train_y = as.matrix(read.table("entrega_4/data/y_laboratorio_AAA.csv", header = FALSE))

########## Clasificador por descenso del gradiente ######################
# Calcular w para los datos de entrenamiento
# w = argmin(1/n sum(log(1+e^ (-ywTx))) + landa * norma(w)²)
# Utilizando descenso del gradiente

entrenar_clasificador <- function(train_x, train_y, alpha){
  # Iniciailizar w0
  # w <- c(-0.5, 0.5)
  w <- runif(length(train_x[1, ]), min = -0.5, max = 0.5)
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
    
    # Calcular el log loss
    log_loss[iter] <- mean(log(1 + exp(- train_y * train_x %*% w)))
    y_pred <- sign(train_x %*% w) # El clasificador es: h(x) = sign(wt * x)
    cro_one_loss[iter] <- mean(train_y * y_pred)
    accuracy <- sum(train_y == y_pred) / nrow(train_y)
    # cat("iter = ", iter, "W prev: ", w, "\n")
    
    # Comprobar si los pesos han cambiado con la iteración
    # Si no hemos terminado
    ws <- rbind(ws, t(w_new))
    w <- w_new
  }
  
  rownames(ws) <- 0:numiterations
  return(list(ws = ws, w = w, log_loss = log_loss, accuracy = accuracy))
}

########## Apartado 1 #########################################
# Visualizar datos
df_train <- data.frame(X1 = train_x[, 1], X2 = train_x[, 2], Y = train_y[, 1])
df_train$Y <- as.factor(df_train$Y)
ggplot(df_train, aes(x = X1, y = X2, color = Y)) + 
    geom_point(size = 3) +
    scale_color_manual(values = c("-1" = "#08215a", "1" = "#3970ed")) +  # Definir los colores
    labs(title = "Datos de entrenamiento", x = "X1", y = "X2") +
    theme_minimal(base_size = 15, base_family = "roboto") +
    theme( axis.line = element_line(color = "#8f8f8f", size = 0.8), axis.ticks = element_line(color = "#8f8f8f"), axis.text = element_text(color = "#8f8f8f"), plot.title = element_text(face = "bold"))

# Entrenar el clasificador lineal
max_iterations <- 200
alpha <- 0.1 # Learning rate
cls <- entrenar_clasificador(train_x, train_y, alpha)

# Graficar el error logarítmico
df_log_loss <- data.frame(Iteration = 1:length(cls$log_loss), LogLoss = cls$log_loss)
ggplot(df_log_loss, aes(x = Iteration, y = LogLoss)) +
    geom_hline(yintercept = cls$log_loss[length(cls$log_loss)], linetype = "dashed", color = "#ff00006e", size = 1) +
    geom_line(color = "#3970ed", size = 1) +
    annotate("text", x = -20, y = cls$log_loss[length(cls$log_loss)], label = round(cls$log_loss[length(cls$log_loss)], 4), color = "red")+
    coord_cartesian(xlim = c(0, max_iterations),  clip = 'off') +
    labs(title = "Evolución del Log Loss a lo largo de las iteraciones", x = "Iteración", y = "Log Loss Error") +
    theme_minimal(base_size = 15, base_family = "roboto") +
    theme( axis.line = element_line(color = "#8f8f8f", size = 0.8), axis.ticks = element_line(color = "#8f8f8f"), axis.text = element_text(color = "#8f8f8f"), plot.title = element_text(face = "bold"))

# Graficar el clasificador lineal
x <- seq(min(df_train["X1"]), max(df_train["X1"]), by = ( max(df_train["X1"]) - min(df_train["X1"]) ) / (nrow(df_train["X1"]) -1 ) )
y <- cls$w[1, 1] * x + cls$w[2, 1]
df_W <- data.frame(Wx = x, Wy = y)

ggplot(df_train) + 
    geom_point(aes(x = X1, y = X2, color = as.factor(Y)), size = 3) +  # Asegúrate de que Y sea un factor
    scale_color_manual(values = c("-1" = "#08215a", "1" = "#3970ed")) +  # Definir los colores
    geom_line(data = df_W, aes(x = x, y = y), color = "red", size = 1) +  # Añadir la línea
    labs(title = "Datos de entrenamiento", x = "X1", y = "X2") +
    theme_minimal(base_size = 15, base_family = "roboto") +
    theme( axis.line = element_line(color = "#8f8f8f", size = 0.8), axis.ticks = element_line(color = "#8f8f8f"), axis.text = element_text(color = "#8f8f8f"), plot.title = element_text(face = "bold") )



########## Apartado 2 #########################################
data_transformation <- function(x1, x2){
  return(x1 ^2 + x2 ^2 - 1/2)
}

# a) Transformar los datos
train_x_trans <- cbind(train_x, apply(train_x, 1, function(x){data_transformation(x[1], x[2]) }))
train_x_trans[, 3] <- scale(train_x_trans[, 3], center = TRUE)
# b) Plotear los datos en 3D
library(plotly)
data <- data.frame(X = train_x_trans[, 1], Y = train_x_trans[, 2], Z = train_x_trans[, 3], Clase = train_y[, 1])
fig <- plot_ly(data, x = ~X,  y = ~Y, z = ~Z,  color = ~Clase,
               type = 'scatter3d', 
               mode = 'markers') 

fig <- fig %>% layout(scene = list(xaxis = list(title = 'X1'),
                                   yaxis = list(title = 'X2'),
                                   zaxis = list(title = 'X3')))
fig

# c) Repetir el descenso del gradiente sobre los datos transformados
max_iterations <- 200
alpha <- 0.1 # Learning rate
cls_trans <- entrenar_clasificador(train_x_trans, train_y, alpha)

# Graficar el error logarítmico
df_log_loss_trans <- data.frame(Iteration = 1:length(cls_trans$log_loss), LogLoss = cls_trans$log_loss)
ggplot(df_log_loss_trans, aes(x = Iteration, y = LogLoss)) +
  geom_hline(yintercept = cls_trans$log_loss[length(cls_trans$log_loss)], linetype = "dashed", color = "#ff00006e", size = 1) +
  geom_line(color = "#3970ed", size = 1) +
  annotate("text", x = -20, y = cls_trans$log_loss[length(cls_trans$log_loss)], label = round(cls_trans$log_loss[length(cls_trans$log_loss)], 4), color = "red")+
  coord_cartesian(xlim = c(0, max_iterations),  clip = 'off') +
  labs(title = "Log Loss datos transformados", x = "Iteración", y = "Log Loss Error") +
  theme_minimal(base_size = 15, base_family = "roboto") +
  theme( axis.line = element_line(color = "#8f8f8f", size = 0.8), axis.ticks = element_line(color = "#8f8f8f"), axis.text = element_text(color = "#8f8f8f"), plot.title = element_text(face = "bold"))

# d) Visualizar datos y plano del clasificador


########## Apartado 3 #########################################
entrenar_clasificador_ci <- function(train_x, train_y, alpha){
  numIteraciones <- 200
  landa <- 1
  N <- length(train_x[, 1])
  d <- length(train_x[1, ])
  
  # Inicializar c
  c <- runif(N, min = 0, max = 1)

  # Funcion para calcular w a partir de c
  W <- function(c){
    w <- rep(0, d)
    for (i in 1:N){
      w <- w + train_x[i, ] * c[i]
    }
    return(t(w))
  }

  # Calculamos w0
  w0 <- W(c)
  
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
    w <- W(c)
    ws <- rbind(ws, w)
    log_loss[t] <- mean(log(1 + exp(- train_y * train_x %*%  t(w) )))
    y_pred <- sign(train_x %*% t(w))
    accuracy <- sum(train_y == y_pred) / nrow(train_y)

  }

  # Calculamos w final
  w <- W(c)
  rownames(ws) <- 0:numIteraciones
  return( list(ws = ws, w = w, log_loss = log_loss, accuracy = accuracy) )
}

# Entrenar el clasificador iterando sobre las c's
alpha <- 0.1 # Learning rate
cls_c <- entrenar_clasificador_ci(train_x_trans, train_y, alpha)

# Graficar el log loss
df_log_loss_c <- data.frame(Iteration = 1:length(cls_c$log_loss), LogLoss = cls_c$log_loss)
ggplot(df_log_loss_c, aes(x = Iteration, y = LogLoss)) +
    geom_hline(yintercept = cls_c$log_loss[length(cls_c$log_loss)], linetype = "dashed", color = "#ff00006e", size = 1) +
    geom_line(color = "#3970ed", size = 1) +
    annotate("text", x = -20, y = cls_c$log_loss[length(cls_c$log_loss)], label = round(cls_c$log_loss[length(cls_c$log_loss)], 4), color = "red")+
    coord_cartesian(xlim = c(0, max_iterations),  clip = 'off') +
    labs(title = "Evolución del Log Loss a lo largo de las iteraciones", x = "Iteración", y = "Log Loss Error") +
    theme_minimal(base_size = 15, base_family = "roboto") +
    theme( axis.line = element_line(color = "#8f8f8f", size = 0.8), axis.ticks = element_line(color = "#8f8f8f"), axis.text = element_text(color = "#8f8f8f"), plot.title = element_text(face = "bold"))

# TODO: Mostrar que si w0 es igual, da lo mismo iterar por ws o por cs. Los vectores de w deberían coincidir en cada iteración.

# b) Resolver el problema utilizando la hinge-loss
