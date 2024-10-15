setwd('C:/users/eneko/Downloads')
train_x <- as.matrix(read.csv('X_laboratorio_AAA.csv', header=F))
train_y <- as.matrix(read.csv('y_laboratorio_AAA.csv', header=F))

library(e1071)
library(rgl)    

# (a) Ajustar SVM lineal sin regularización

# Ajustar el modelo SVM con un kernel lineal
train_x <- scale(train_x, scale=T)
# El cost es el landa, cuando es pequeño el margen es grande y coge como vectores de soporte a todos los puntos (1e-5), si es el de default coge lo normal (1) y si es grande (1e5) el margen es más pequeño y coge 4 vectores de soporte.
svm_model <- svm(train_x, as.factor(train_y), kernel = "linear", cost = 1e5)
summary(svm_model)

# (b) Crear una función para graficar en 3D
train_y[train_y == -1] <- 0 

# Obtener los índices de los vectores de soporte
support_vectors_idx <- svm_model$index

# Configurar los colores de las clases
train_y <- as.factor(train_y)
colores <- c("red", "blue")  # Colores para las clases (puedes cambiar estos colores si lo deseas)
point_colors <- colores[as.numeric(train_y)]
point_colors

# Abrir una ventana 3D
open3d()
plot3d(train_x, col = point_colors, size = 5, type = "s", box = FALSE)
points3d(train_x[support_vectors_idx,], col = "black", size = 200, alpha = 0.8)
legend3d("topright", legend = levels(as.factor(train_y)), pch = 16, col = colores, cex = 1.2)

# Obtener los coeficientes del hiperplano
w <- t(svm_model$coefs) %*% svm_model$SV  # Vector normal del hiperplano
b <- -svm_model$rho  # Sesgo del hiperplano

# Crear una cuadrícula para el plano
x_seq <- seq(min(train_x[, 1]), max(train_x[, 1]), length = 50)
y_seq <- seq(min(train_x[, 2]), max(train_x[, 2]), length = 50)
z_matrix <- outer(x_seq, y_seq, function(x, y) (-w[1] * x - w[2] * y - b) / w[3])

# Dibujar el plano en 3D
surface3d(x_seq, y_seq, z_matrix, color = "green", alpha = 0.5)

# (c) Numero de vectores de soporte
length(svm_model$index) # 4

# (d) 
new_train_x <- svm_model$SV
new_train_y <- train_y[svm_model$index]
new_svm_model <- svm(new_train_x, new_train_y, kernel = "linear", cost = 1e5)
print(new_svm_model)

new_sv_idx <- new_svm_model$index
new_train_y <- as.factor(new_train_y)
new_colores <- c("red", "blue")  # Colores para las clases (puedes cambiar estos colores si lo deseas)
new_point_colors <- new_colores[as.numeric(new_train_y)]
new_point_colors

open3d()
plot3d(new_train_x, col = new_point_colors, size = 5, type = "s", box = FALSE)
points3d(new_train_x[new_sv_idx,], col = "black", size = 200, alpha = 0.8)
legend3d("topright", legend = levels(as.factor(new_train_y)), pch = 16, col = new_colores, cex = 1.2)
surface3d(x_seq, y_seq, z_matrix, color = "green", alpha = 0.5)

# (e) No acaba de pintar bien el plano, por lo demas bien.
new_train_x2 <- train_x[-19,]
new_train_y2 <- as.factor(train_y[-19])
new_svm_model2 <- svm(new_train_x2, new_train_y2, kernel = "linear", cost = 1e5)
print(new_svm_model2)

new_sv_idx2 <- new_svm_model2$index
new_train_y2 <- as.factor(new_train_y2)
new_colores2 <- c("red", "blue")  # Colores para las clases (puedes cambiar estos colores si lo deseas)
new_point_colors2 <- new_colores2[as.numeric(new_train_y2)]
new_point_colors2

open3d()
plot3d(new_train_x2, col = new_point_colors2, size = 5, type = "s", box = FALSE)
points3d(new_train_x2[new_sv_idx2,], col = "black", size = 200, alpha = 0.8)
legend3d("topright", legend = levels(as.factor(new_train_y2)), pch = 16, col = new_colores2, cex = 1.2)

# Obtener los coeficientes del hiperplano
w2 <- t(new_svm_model2$coefs) %*% new_svm_model2$SV  # Vector normal del hiperplano
b2 <- -new_svm_model2$rho  # Sesgo del hiperplano

# Crear una cuadrícula para el plano
x_seq2 <- seq(min(new_train_x2[, 1]), max(new_train_x2[, 1]), length = 50)
y_seq2 <- seq(min(new_train_x2[, 2]), max(new_train_x2[, 2]), length = 50)
z_matrix2 <- outer(x_seq2, y_seq2, function(x, y) (-w2[1] * x - w2[2] * y - b2) / w[3])

# Dibujar el plano en 3D
surface3d(x_seq2, y_seq2, z_matrix2, color = "green", alpha = 0.5)

# Apartado 2 --------------------------------------------------------------

library(mlbench)
library(e1071)
library(ggplot2)
library(gridExtra)

train_x2 <- as.matrix(read.csv('X_laboratorio_AAA.csv', header=F))[,1:2]
train_y2 <- as.matrix(read.csv('y_laboratorio_AAA.csv', header=F))

train_y2 <- as.factor(train_y2)

# Función para crear un grid y hacer predicciones para visualización de fronteras
create_grid <- function(data, n = 100){
  x_range <- seq(min(data[,1]) - 1, max(data[,1]) + 1, length.out = n)
  y_range <- seq(min(data[,2]) - 1, max(data[,2]) + 1, length.out = n)
  grid <- expand.grid(x_range, y_range)
  colnames(grid) <- colnames(data)
  return(grid)
}

# Valores de lambda (en R es equivalente al parámetro cost)
lambdas <- c(0.1, 1, 5)

# Función para entrenar SVM y graficar resultados
plot_svm <- function(lambda_value, train_x, train_y, grid){
  # Entrenar el modelo SVM con soft margin usando kernel lineal
  svm_model <- svm(train_x, train_y, kernel = "linear", cost = lambda_value, scale = FALSE)
  
  w <- t(svm_model$coefs) %*% svm_model$SV  # Pesos w
  b <- svm_model$rho  # Sesgo b
  
  decision_function <- function(x, w, b) {
    (-w[1] * x - b) / w[2]  # Despejar para obtener y en función de x
  }
  
  # Crear un rango de valores de X para graficar
  x_vals <- seq(min(train_x[, 1]) - 1, max(train_x[, 1]) + 1, length.out = 100)
  
  # Frontera de decisión (w * x + b = 0)
  decision_boundary <- decision_function(x_vals, w, b)
  
  # Márgenes (w * x + b = ±1)
  margin_1 <- decision_function(x_vals, w, b - 1)  # Margen superior
  margin_2 <- decision_function(x_vals, w, b + 1)  # Margen inferior
  
  # Predicción sobre el grid
  grid_pred <- predict(svm_model, grid)
  
  # Crear un dataframe para visualización
  grid_df <- cbind(grid, pred = as.factor(grid_pred))
  
  # Graficar los puntos y la frontera de decisión
  plot <- ggplot(as.data.frame(train_x), aes(x = V1, y = V2, color = train_y)) +
    geom_point(size = 3) +
    geom_line(aes(x = x_vals, y = decision_boundary), color = "black", linetype = "solid") +  # Frontera de decisión
    geom_line(aes(x = x_vals, y = margin_1), color = "blue", linetype = "dashed") +           # Margen positivo
    geom_line(aes(x = x_vals, y = margin_2), color = "blue", linetype = "dashed") +           # Margen negativo
    theme_minimal() +
    ggtitle(paste("SVM with lambda =", lambda_value)) +
    theme(legend.position = "none")
  
  return(plot)
}

# Crear el grid
grid <- create_grid(train_x2)

# Graficar para diferentes valores de lambda
plots <- list()
for (lambda in lambdas) {
  plots[[as.character(lambda)]] <- plot_svm(lambda, train_x2, train_y2, grid)
}

# Mostrar todos los gráficos juntos
do.call(grid.arrange, c(plots, ncol = 1))

