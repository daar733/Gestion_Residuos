# Instalar y cargar los paquetes necesarios
install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")

library(readxl)
library(dplyr)
library(ggplot2)

# Leer el archivo Excel
file_path <- "C:/DataSetMaestria/Vinculacion/Parte1.xlsx"
datos <- read_excel(file_path)

# Mostrar las primeras filas de los datos
head(datos)

# Distribución de género
datos <- datos %>%
  mutate(Género = ifelse(!is.na(Hombre), "Hombre", "Mujer")) %>%
  select(Género, Edad)

# Mostrar las primeras filas de los datos transformados
print(datos)

# Distribución de género
tabla_genero <- table(datos$Género)
print(tabla_genero)

# Crear el diagrama de pastel para la distribución de género
library(ggplot2)

ggplot(tabla_genero, aes(x = "", y = Frecuencia, fill = Género)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Distribución de Género") +
  theme_void()

# Gráfico de barras para la distribución de género
barplot(tabla_genero, 
        main = "Distribución de Género", 
        col = c("blue", "pink"), 
        xlab = "Género", 
        ylab = "Frecuencia")

# Histograma para la distribución de edad
hist(datos$Edad, 
     breaks = 10, 
     main = "Distribución de Edad", 
     col = "lightblue", 
     xlab = "Edad", 
     ylab = "Frecuencia")

# Calcular los cuantiles de la edad
cuantiles_edad <- quantile(datos$Edad, probs = seq(0, 1, 0.25), na.rm = TRUE)

# Mostrar los cuantiles
print(cuantiles_edad)

# Crear el gráfico de caja y bigotes para los cuantiles de la edad
ggplot(datos, aes(y = Edad)) +
  geom_boxplot() +
  labs(title = "Cuantiles de la Edad",
       y = "Edad") +
  theme_minimal()

# Crear rangos de edad personalizados
datos <- datos %>%
  mutate(Rango_Edad = case_when(
    Edad >= 0 & Edad <= 12 ~ "Niños",
    Edad >= 13 & Edad <= 19 ~ "Adolescentes",
    Edad >= 20 & Edad <= 35 ~ "Adultos jóvenes",
    Edad >= 36 & Edad <= 55 ~ "Adultos",
    Edad >= 56 & Edad <= 64 ~ "Adultos mayores",
    Edad >= 65 ~ "Tercera edad"
  ))

# Mostrar las primeras filas de los datos con los nuevos rangos de edad
print(datos)

# Gráfico de barras apiladas para la distribución de edad por género
ggplot(datos, aes(x = Género, fill = Rango_Edad)) +
  geom_bar(position = "stack") +
  labs(title = "Distribución de Edad por Género", x = "Género", y = "Frecuencia", fill = "Rango de Edad") +
  theme_minimal()

# Gráfico de barras agrupadas para la distribución de edad por género
ggplot(datos, aes(x = Rango_Edad, fill = Género)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribución de Edad por Género", x = "Rango de Edad", y = "Frecuencia", fill = "Género") +
  theme_minimal() +
  scale_x_discrete(drop = FALSE)

------------------ 2 ------------
  
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("readxl")

library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)

  
  # Leer el archivo Excel
  file_path <- "C:/DataSetMaestria/Vinculacion/Parte1.xlsx"
datos <- read_excel(file_path)

head(datos)

# Crear el subconjunto con los campos de interés
subconjunto_conocimiento <- datos %>%
  select(`Muy informado` = `1 Muy informado`, `Informado` = `1 Informado`, `Poco informado` = `1 Poco Informado`, `Desconocimiento` = `1 Desconocimiento`)

# Mostrar las primeras filas del subconjunto
head(subconjunto_conocimiento)

# Contar la cantidad de respuestas para cada nivel de conocimiento
conteo_conocimiento <- subconjunto_conocimiento %>%
  summarise_all(sum, na.rm = TRUE)

head(conteo_conocimiento)

# Transformar el conteo a formato largo
datos_largos <- conteo_conocimiento %>%
  pivot_longer(cols = everything(), names_to = "Nivel_Conocimiento", values_to = "Frecuencia")

# Mostrar las primeras filas de los datos transformados
head(datos_largos)

# Crear el gráfico de barras para la distribución de los niveles de conocimiento
ggplot(datos_largos, aes(x = Nivel_Conocimiento, y = Frecuencia, fill = Nivel_Conocimiento)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribución del Nivel de Conocimiento sobre el Sistema de Gestión de Residuos Sólidos",
       x = "Nivel de Conocimiento", y = "Frecuencia") +
  theme_minimal()


-------------- 2.1 ---------------------
  
  
  # Leer el archivo Excel
  file_path <- "C:/DataSetMaestria/Vinculacion/Parte1.xlsx"
datos <- read_excel(file_path)

head(datos)

  subconjunto_conocimiento <- datos %>%
  select(`Excelente` = `2 Excelente`, `Bueno` = `2 Bueno`, `Regular` = `2 Regular`, `Malo` = `2 Malo`)

# Mostrar las primeras filas del subconjunto
head(subconjunto_conocimiento)

# Contar la cantidad de respuestas para cada nivel de conocimiento
conteo_conocimiento <- subconjunto_conocimiento %>%
  summarise_all(sum, na.rm = TRUE)

head(conteo_conocimiento)

# Transformar el conteo a formato largo
datos_largos <- conteo_conocimiento %>%
  pivot_longer(cols = everything(), names_to = "Calificación", values_to = "Frecuencia")

# Mostrar las primeras filas de los datos transformados
head(datos_largos)

# Crear el gráfico de barras para la distribución de los niveles de conocimiento
ggplot(datos_largos, aes(x = Calificación, y = Frecuencia, fill = Calificación)) +
  geom_bar(stat = "identity") +
  labs(title = "Calificación del servicio de recolección",
       x = "Calificación", y = "Frecuencia") +
  theme_minimal()




-------------- 4 ---------------------
  
  
  # Leer el archivo Excel
  file_path <- "C:/DataSetMaestria/Vinculacion/Parte1.xlsx"
datos <- read_excel(file_path)

head(datos)

subconjunto_conocimiento <- datos %>%
  select(`Excelente` = `8 Excelente`, `Bueno` = `8 Bueno`, `Regular` = `8 Regular`, `Malo` = `8 Malo`, `No se cuenta con el servicio` = `8 No se cuenta con el servicio`)

# Mostrar las primeras filas del subconjunto
head(subconjunto_conocimiento)

# Contar la cantidad de respuestas para cada nivel de conocimiento
conteo_conocimiento <- subconjunto_conocimiento %>%
  summarise_all(sum, na.rm = TRUE)

head(conteo_conocimiento)

# Transformar el conteo a formato largo
datos_largos <- conteo_conocimiento %>%
  pivot_longer(cols = everything(), names_to = "Calificación", values_to = "Frecuencia")

# Mostrar las primeras filas de los datos transformados
head(datos_largos)

# Crear el gráfico de barras para la distribución de los niveles de conocimiento
ggplot(datos_largos, aes(x = Calificación, y = Frecuencia, fill = Calificación)) +
  geom_bar(stat = "identity") +
  labs(title = "Calificación del servicio de barrido de calles y espacios públicos en la ciudad",
       x = "Calificación", y = "Frecuencia") +
  theme_minimal()



-------------- 4.1 ---------------------
  
  
  # Leer el archivo Excel
  file_path <- "C:/DataSetMaestria/Vinculacion/Parte1.xlsx"
datos <- read_excel(file_path)

head(datos)


subconjunto_conocimiento <- datos %>%
  select( `Si` = `9 Si`, `No` = `9 No`)

round(cor(x = subconjunto_conocimiento, method = "pearson"), 3)

# Mostrar las primeras filas del subconjunto
head(subconjunto_conocimiento)

# Contar la cantidad de respuestas para cada nivel de conocimiento
conteo_conocimiento <- subconjunto_conocimiento %>%
  summarise_all(sum, na.rm = TRUE)

head(conteo_conocimiento)

# Transformar el conteo a formato largo
datos_largos <- conteo_conocimiento %>%
  pivot_longer(cols = everything(), names_to = "Calificación", values_to = "Frecuencia")

# Mostrar las primeras filas de los datos transformados
head(datos_largos)

# Crear el gráfico de barras para la distribución de los niveles de conocimiento
ggplot(datos_largos, aes(x = Calificación, y = Frecuencia, fill = Calificación)) +
  geom_bar(stat = "identity") +
  labs(title = "Implementación de contenedores de basura.",
       x = "", y = "") +
  theme_minimal()
------------------------------------------------
# Crear el subconjunto con los campos de interés
subconjunto_9 <- datos %>%
  select(Si = `9 Si`, No = `9 No`)

# Mostrar las primeras filas del subconjunto
head(subconjunto_9)

# Transformar los datos de formato ancho a formato largo
datos_largos_9 <- subconjunto_9 %>%
  pivot_longer(cols = everything(), names_to = "Respuesta", values_to = "Frecuencia")

# Mostrar las primeras filas de los datos transformados
head(datos_largos_9)

# Crear el gráfico de torta para la distribución de las respuestas "9 Si" y "9 No"
ggplot(datos_largos_9, aes(x = "", y = Frecuencia, fill = Respuesta)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Distribución de las Respuestas '9 Si' y '9 No'") +
  theme_void()

# Crear el gráfico de barras apiladas para la distribución de las respuestas "9 Si" y "9 No"
ggplot(datos_largos_9, aes(x = "", y = Frecuencia, fill = Respuesta)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribución de las Respuestas 'Si' y 'No'",
       x = "Respuesta", y = "Frecuencia") +
  theme_minimal() +
  coord_flip()

----------------------------- 1 Pastel ------------------------
  
  file_path <- "C:/DataSetMaestria/Vinculacion/Parte1.xlsx"
datos <- read_excel(file_path)
  
  # Crear el subconjunto con las columnas de interés
  subconjunto_genero <- datos %>%
  select(Hombre, Mujer)

# Contar la cantidad de respuestas para cada género
conteo_genero <- subconjunto_genero %>%
  summarise(Hombre = sum(Hombre, na.rm = TRUE), Mujer = sum(Mujer, na.rm = TRUE))

# Transformar el conteo a formato largo
datos_largos_genero <- conteo_genero %>%
  pivot_longer(cols = everything(), names_to = "Género", values_to = "Frecuencia")

# Crear el diagrama de pastel para la distribución de género
ggplot(datos_largos_genero, aes(x = "", y = Frecuencia, fill = Género)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Distribución de Género") +
  theme_void()





















 
