#########################################################################
#                               PRIMERA GRÁFICA                         #
#########################################################################
# ¿Cuál es el lugar con mayor concentración de fitoplacton recientemente en Euskadi?
bio <- read.table("entrega_1/data/measure_bi_2024.csv", header = TRUE, sep = ";",stringsAsFactors = FALSE, encoding = "utf-8")
lugares <- read.table("entrega_1/data/samplePoints_2024.csv", header = TRUE, sep = ";", encoding = "utf-8")

# Damos formato a los datos y cambiamos nombres de columnas
colnames(bio)
names(bio)[names(bio) == 'Sample.Point.Code'] <- 'Code'
names(bio)[names(bio) == 'Date'] <- 'date'
bio$date <- as.Date(bio$date, format = "%d/%m/%Y") # string -> Date
colnames(lugares)

# Hacemos un left join para obtener todas las entradas de bio que se puedan relacionar con
# lugares y aquellas de bio que no seguiran apareciendo con N/A 
# install.packages("tidyverse")
library(tidyverse)
dat <- left_join(bio, lugares, by = join_by(Code))

# Obtenemos las entradas con la fecha mas actual manteniendo la diferencia entre
# los niveles de profundidad
library(dplyr)
dat_grouped <- dat %>% group_by(Code)
dat_grouped %>% summarise(n = n()) # Cuantas entradas hay por cada lugar
dat_last <- dat_grouped %>% filter(date == max(date)) %>% ungroup()

# Level: S -> superficie | F -> Fondo
df <- data.frame(code = dat_last$Code, date = dat_last$date, nivel = dat_last$Level, val = dat_last$Value, name = dat_last$Sample.Point)

# Creamos un diagrama de barras simple de los datos de la superficie
df_filtered <- df[df$nivel == "S", ]
df_filtered <- df_filtered[order(df_filtered$val, decreasing = FALSE), ] # ordenamos
par(mar = c(5, 10, 2, 2))  # Aumentar el margen inferior
barplot(df_filtered$val, names.arg = df_filtered$name, main = "Concentración clorofila A en el fondo", xlab = "g/l", cex.names = 0.5, las = 1, space = 0.5, horiz = TRUE)

# Utilizamos ggplot2 para hacer la grafica final
library(ggplot2)
df$nivel <- ifelse(df$nivel == "S", "Superficie", ifelse(df$nivel == "F", "Fondo", df$nivel))
df_filtered <- df %>% filter(nivel %in% c("Superficie", "Fondo"))

# Ordenar de mayor a menor en funcion de la media
df_ordered <- df_filtered %>% group_by(name) %>% summarize(mean_val = mean(val, na.rm = TRUE)) %>% arrange(desc(mean_val))
df_filtered$name <- factor(df_filtered$name, levels = df_ordered$name)

ggplot(df_filtered, aes(x = name, y = val, fill = nivel)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) +
  labs(title = "Concentración de Clorofila A por litoral", 
       x = "Nombre del Punto de Muestreo", 
       y = "Concentración (g/l)") +
  theme_minimal(base_size = 18) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotar etiquetas del eje x
  theme(panel.grid.major.x = element_blank())  # Quitar líneas de cuadrícula en el eje x






