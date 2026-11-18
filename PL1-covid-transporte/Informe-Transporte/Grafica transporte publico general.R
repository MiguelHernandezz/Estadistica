# Paquetes (se instalan si faltan)
paquetes <- c("knitr", "moments")
dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE, showWarnings = FALSE)
.libPaths(c(Sys.getenv("R_LIBS_USER"), .libPaths()))
faltan <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan)) install.packages(faltan, repos = "https://cloud.r-project.org")
library(knitr)
library(moments)

(tabla_transporte_publico_general = read.csv2("20239.csv"))

# Verificar los primeros registros de la columna Nivel 2
head(tabla_transporte_publico_general$Tipo.de.transporte..Nivel.2)

# La columna Total mezcla viajeros (en miles, con punto de miles: "346.302")
# con tasas de variación ("3,7"). Nos quedamos con los viajeros transportados
# del total general (sin desglose por tipo de transporte)
viajeros <- subset(tabla_transporte_publico_general,
                   Viajeros.y.tasas == "Viajeros transportados" &
                   Tipo.de.transporte..Nivel.1 == "Total de viajeros" &
                   Tipo.de.transporte..Nivel.2 == "")

# Quitar el punto de miles y convertir a numérico
viajeros$Total <- as.numeric(gsub(".", "", viajeros$Total, fixed = TRUE))

# Verificar si hubo conversiones exitosas
summary(viajeros$Total)

colnames(tabla_transporte_publico_general)

total <- na.omit(viajeros$Total)


media <- mean(total)
mediana <- median(total)
minimo <- min(total)
maximo <- max(total)
rango <- maximo - minimo
varianza <- var(total)
desviacion_estandar <- sd(total)
coef_var <- desviacion_estandar / media
primer_cuartil <- quantile(total, 0.25)
tercer_cuartil <- quantile(total, 0.75)
rango_intercuartilico <- tercer_cuartil - primer_cuartil


# Función para calcular estadísticas
calcular_estadisticas <- function(Total) {
  # Media
  media <- mean(Total, na.rm = TRUE)

  # Mediana
  mediana <- median(Total, na.rm = TRUE)

  # Mínimo
  minimo <- min(Total, na.rm = TRUE)

  # Máximo
  maximo <- max(Total, na.rm = TRUE)

  # Rango
  rango <- maximo - minimo

  # Varianza
  varianza <- var(Total, na.rm = TRUE)

  # Desviación estándar
  desviacion_estandar <- sd(Total, na.rm = TRUE)

  # Coeficiente de variación
  coeficiente_variacion <- desviacion_estandar / media

  # Primer cuartil
  primer_cuartil <- quantile(Total, 0.25, na.rm = TRUE)

  # Tercer cuartil
  tercer_cuartil <- quantile(Total, 0.75, na.rm = TRUE)

  # Rango intercuartílico
  rango_intercuartilico <- tercer_cuartil - primer_cuartil

  # Coeficiente de asimetría
  coeficiente_asimetría <- (3 * (media - mediana)) / desviacion_estandar

  # Curtosis
  curtosis <- (sum((Total - media)^4) / (length(Total) * desviacion_estandar^4)) - 3

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
    Curtosis = curtosis
  )

  return(resultados)
}

resultados <- calcular_estadisticas(total)

# Imprime los resultados
for (i in names(resultados)) {
  cat(i, ": ", resultados[[i]], "\n")
}


# Cálculo de estadísticas
asimetria <- skewness(total, na.rm = TRUE)
curtosis <- kurtosis(total, na.rm = TRUE)

# Crear la tabla de estadísticas
tabla_estadisticas <- data.frame(
  Estadística = c("Media", "Mediana", "Mínimo", "Máximo", "Rango",
                  "Varianza", "Desviación Estándar", "Coeficiente de Variación",
                  "Primer Cuartil", "Tercer Cuartil", "Rango Intercuartílico",
                  "Coeficiente de Asimetría", "Curtosis"),
  Valor = c(media, mediana, minimo, maximo, rango,
            varianza, desviacion_estandar, coef_var,
            primer_cuartil, tercer_cuartil, rango_intercuartilico,
            asimetria, curtosis)
)

# Mostrar la tabla con knitr
knitr::kable(tabla_estadisticas, caption = "Estadísticas descriptivas: viajeros de transporte público (miles)")
print(tabla_estadisticas)

n <- length(total)

# Número de intervalos (regla de Sturges)
k <- ceiling(1 + 3.322 * log10(n))
viajeros_intervalos <- cut(total, breaks = k)
table(viajeros_intervalos)

# Gráficas
hist(total, breaks = k, col = "lightblue", main = "Viajeros de transporte público (miles/mes)",
     xlab = "Viajeros (miles)", ylab = "Frecuencia")
boxplot(total, main = "Diagrama de caja de viajeros", ylab = "Viajeros (miles)", col = "lightblue")
