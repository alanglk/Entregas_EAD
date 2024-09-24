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


#########################################################################
#                               SEGUNDA GRÁFICA                         #
#########################################################################
# ¿Cuál de las estaciones tiene más seguimiento biológico?
bio <- read.table("entrega_1/data/measure_bi_2024.csv", header = TRUE, sep = ";",stringsAsFactors = FALSE, encoding = "utf-8")
lugares <- read.table("entrega_1/data/samplePoints_2024.csv", header = TRUE, sep = ";", encoding = "utf-8")

names(bio)[names(bio) == 'Sample.Point.Code'] <- 'Code'
names(bio)[names(bio) == 'Date'] <- 'date'
bio$date <- as.Date(bio$date, format = "%d/%m/%Y")
bio$Level <- ifelse(bio$Level == "S", "Superficie", ifelse(bio$Level == "F", "Fondo", bio$Level))

# Agrupamos bio y lugares y creamos el dataframe 
library(tidyverse)
dat <- left_join(bio, lugares, by = join_by(Code)) 
df <- data.frame(id = dat$Code, nombre = dat$Sample.Point, x = dat$XETRS89, y = dat$YETRS89)

# Agrupamos por ID, contamos el número de apariciones de cada uno, lo añadimos en una nueva columna y nos quedamos con las entradas de id distinto
df <- df %>%  group_by(id) %>% mutate(numMuestras = n() - 1)
df <- df %>% distinct(id, .keep_all = TRUE)
ungroup()

# Crear un mapa geográfico con los puntos de interés
# install.packages(c("sf", "ggspatial", "rnaturalearth", "rnaturalearthdata", "ggrepel"))
library(ggplot2)
library(sf)
library(ggspatial)
library(ggrepel)
library(rnaturalearth)
library(rnaturalearthhires) # remotes::install_github("ropensci/rnaturalearthhires")
library(rnaturalearthdata)

# convertimos el dataframe a sf para trabajar con coordenadas
df_sf <- st_as_sf(df, coords = c("x", "y"), crs = 25830)  # ETRS89
df_sf <- st_transform(df_sf, crs = 4326)  # Convertir a WGS84 para el mapa

# Descargamos los mapas geográficos de España y Francia y el mapa geopolítico de España
spain <- ne_countries(country = "Spain", scale = "medium", returnclass = "sf")
france <- ne_countries(country = "France", scale = "medium", returnclass = "sf")
provinces <- ne_states(country = "Spain", returnclass = "sf")

# Creamos la zona de recorte de los mapas
euskadi <- st_bbox(c(xmin = -3.4, xmax = -1.0, ymin = 43.0, ymax = 44.0), crs = st_crs(4326))

ggplot() + 
     geom_sf(data = spain, fill = "lightgray", color = "white") +
     geom_sf(data = france, fill = "lightgray", color = "white") +
     geom_sf(data = provinces, fill = NA, color = "black", linetype = "dashed") +

     geom_sf(data = df_sf, aes(size = numMuestras), shape = 21, fill = "blue", color = "black", alpha = 0.6) +
     geom_text_repel(data = df_sf, aes(label = nombre, geometry = geometry), stat = "sf_coordinates", size = 4, force = 15, min.segment.length = 0.3, nudge_x = c(-0.3, 0.3), nudge_y = c(0.15, -0.15), box.padding = 0.6, point.padding = 0.5,) +
     labs(x = "Longitud", y = "Latitud", size = "Número de Muestras", title = "Mapa del Litoral de Euskadi con Círculos según el Número de Muestras", subtitle = "Coordenadas ETRS89 proyectadas en WGS84") + 
     annotation_scale(location = "bl", width_hint = 0.5) +
     annotation_north_arrow(location = "tl", which_north = "true", pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"), style = north_arrow_fancy_orienteering) +
  
     coord_sf(xlim = c(euskadi["xmin"], euskadi["xmax"]), ylim = c(euskadi["ymin"], euskadi["ymax"]), expand = FALSE) +
     theme_minimal() +
     theme(legend.position = "right")

#########################################################################
#                               TERCCERA GRÁFICA                        #
#########################################################################
d1 <- read.table("measure_fq_2024.csv", sep=";", header=TRUE, stringsAsFactors = FALSE, fileEncoding="utf-8")
d2 <- read.table("measure_bi_2024.csv", sep=";", header=TRUE, stringsAsFactors = FALSE, fileEncoding="utf-8")

#Crear el subconjunto de datos a utilizar.
df <- subset(d1, Sample.Point.Code %in% c("L-UR20", "L-N20", "L-B20", "L-BI10", "L-U10")) 
df <- df %>% select(-Date, -Hour, -Level, -Depth)

d22 <- d2 %>%
  filter(Parameter == "Clorofila A") %>%  # Filtrar solo el parámetro "Clorofila A"
  group_by(Sample.Point.Code, Type, Subgroup, Parameter, Species, Operator, Unit, Additional.information, Situation) %>%
  summarise(Value = mean(Value, na.rm = TRUE))
df2 <- rbind(df, subset(d22, Sample.Point.Code %in% c("L-UR20", "L-N20", "L-B20", "L-BI10", "L-U10")))
df3 <- subset(df2, Parameter %in% c("Clorofila A", "Benceno", "Aclonifeno", "Hexaclorobutadieno"))

#Cambiar los códigos del eje x a su nombre.
litorales <- data.frame(
  Sample.Point.Code = c("L-B20", "L-BI10", "L-N20", "L-U10", "L-UR20"),  # Los códigos que tienes
  Litoral.Name = c("Litoral de Bakio", "Litoral de Hondarribia", "Litoral de Sopelana", "Litoral de Zumaia", "Litoral de Mompás")  # Los nombres correspondientes
)

df3 <- df3 %>%
  left_join(litorales, by = "Sample.Point.Code") %>%
  mutate(Sample.Point.Code = Litoral.Name)

# En el eje Y los parametros, en el X nombre de los literales, de color el parametro 
ggplot(df3, aes(x = Sample.Point.Code, y = Value, group = Parameter)) +
  geom_point(size = 4, shape = 21, aes(fill = Parameter), stroke = 1) + # Forma con borde negro y relleno transparente
  geom_line(aes(color= Parameter), size = 1) +
  labs(x = "", y = "Valor (µg/l)") +
  ggtitle("Relación entre clorofila A y varios parámetros físico/químicos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
