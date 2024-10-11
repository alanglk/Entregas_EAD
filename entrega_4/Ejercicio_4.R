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


