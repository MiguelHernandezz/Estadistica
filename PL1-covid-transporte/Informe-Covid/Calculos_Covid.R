# Instala los paquetes que falten y carga los datos (Proyecto.R)
paquetes <- c("ggplot2")
dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE, showWarnings = FALSE)
.libPaths(c(Sys.getenv("R_LIBS_USER"), .libPaths()))
faltan <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan)) install.packages(faltan, repos = "https://cloud.r-project.org")
library(ggplot2)
source("Proyecto.R")

numero_hospitalizados = covid2$num_hosp
numero_fallecidos = covid2$num_def
# Función para calcular estadísticas
calcular_estadisticas <- function(x) {
  # Media
  media <- mean(x, na.rm = TRUE)
  
  # Mediana
  mediana <- median(x, na.rm = TRUE)
  
  # Mínimo
  minimo <- min(x, na.rm = TRUE)
  
  # Máximo
  maximo <- max(x, na.rm = TRUE)
  
  # Rango
  rango <- maximo - minimo
  
  # Varianza
  varianza <- var(x, na.rm = TRUE)
  
  # Desviación estándar
  desviacion_estandar <- sd(x, na.rm = TRUE)
  
  # Coeficiente de variación
  coeficiente_variacion <- desviacion_estandar / media
  
  # Primer cuartil
  primer_cuartil <- quantile(x, 0.25, na.rm = TRUE)
  
  # Tercer cuartil
  tercer_cuartil <- quantile(x, 0.75, na.rm = TRUE)
  
  # Rango intercuartílico
  rango_intercuartilico <- tercer_cuartil - primer_cuartil
  
  # Coeficiente de asimetría
  coeficiente_asimetría <- (3 * (media - mediana)) / desviacion_estandar
  
  # Curtosis
  curtosis <- (sum((x - media)^4) / (length(x) * desviacion_estandar^4)) - 3
  
  # Resultados
  resultados <- list(
    Media = media,
    Mediana = mediana,
    Minimo = minimo,
    Maximo = maximo,
    Rango = rango,
    Varianza = varianza,
    Desviacion_Estandar = desviacion_estandar,
    Coeficiente_Variacion = coeficiente_variacion,
    Primer_Cuartil = primer_cuartil,
    Tercer_Cuartil = tercer_cuartil,
    Rango_Intercuartilico = rango_intercuartilico,
    Coeficiente_Asimetría = coeficiente_asimetría,
    Curtosis = curtosis,
    Valores_Inferiores = x[x < media],
    Valores_Superiores = x[x > media]
  )

  return(resultados)
}

# Ejemplo de uso
x <- numero_hospitalizados
resultados_hospitalizados <- calcular_estadisticas(x)
resultados <- resultados_hospitalizados

# Imprime los resultados
for (i in setdiff(names(resultados), c("Valores_Inferiores", "Valores_Superiores"))) {
  cat(i, ": ", resultados[[i]], "\n")
}

# Crear el data frame
df <- data.frame(
  Valor = c(resultados$Valores_Inferiores, resultados$Valores_Superiores),
  Tipo = c(rep("Valor Inferior a la Media", length(resultados$Valores_Inferiores)),
           rep("Valor Superior a la Media", length(resultados$Valores_Superiores)))
)

# Agregar la media al data frame
df$Media <- rep(resultados$Media, times = nrow(df))

# Agregar la desviación estándar al data frame
df$Desviacion_Estandar <- rep(resultados$Desviacion_Estandar, times = nrow(df))

# Histograma
ggplot(df, aes(x = Valor)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "skyblue") + 
  labs(title = "Distribución de los datos", x = "Valor", y = "Frecuencia")

# Boxplot
ggplot(df, aes(x = "", y = Valor)) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 1) + 
  labs(title = "Boxplot de los datos", x = "", y = "Valor")
# Gráfico de dispersión
ggplot(df, aes(x = Valor, y = Media)) + 
  geom_point() + 
  labs(title = "Gráfico de dispersión de los datos", x = "Valor", y = "Media")

# Gráfico de barras
ggplot(df, aes(x = Tipo, fill = Tipo)) + 
  geom_bar(position = "dodge") + 
  labs(title = "Frecuencia de valores inferiores y superiores a la media", x = "", y = "Frecuencia")

# Gráfico de densidad
ggplot(df, aes(x = Valor)) + 
  geom_density(alpha = 0.5, fill = "skyblue") + 
  labs(title = "Gráfico de densidad de los datos", x = "Valor", y = "Densidad")
hist(num_casos)

sum(numero_casos_Covid2)
sum(numero_hospitalizados)
sum(numero_fallecidos)











