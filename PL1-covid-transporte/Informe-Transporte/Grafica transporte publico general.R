(tabla_transporte_publico_general = read.csv2("20239.csv"))

total <- na.omit(tabla_transporte_publico_general$Total)

# Verificar los primeros registros de la columna Total
head(tabla_transporte_publico_general$Nivel.2.)

# Quitar posibles caracteres no numéricos (como puntos o comas de miles) y convertir a numérico
tabla_transporte_publico_general$Total <- as.numeric(gsub(";", "", tabla_transporte_publico_general$Total))

# Verificar si hubo conversiones exitosas
summary(tabla_transporte_publico_general$Total)

colnames(tabla_transporte_publico_general)


tabla_transporte_publico_general$`Total`




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
  
  # Mediana# MedTotaliana
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
  curtosis <- (sum((Total - media)^4) / (length(x) * desviacion_estandar^4)) - 3
  
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
  )
  
  return(resultados)
}

# Ejemplo de uso
x <- numero_hospitalizados
Total <- calcular_estadisticas(Total)

# Imprime los resultados
for (i in names(resultados)) {
  cat(i, ": ", resultados[[i]], "\n")
  
view(calcular_estadisticas(Total))


install.packages("ggplot2")
install.packages("knitr")  
library(knitr)




# Cálculo de estadísticas
media <- mean(Total, na.rm = TRUE)
mediana <- median(Total, na.rm = TRUE)
minimo <- min(Total, na.rm = TRUE)
maximo <- max(Total, na.rm = TRUE)
rango <- maximo - minimo
varianza <- var(Total, na.rm = TRUE)
desviacion_estandar <- sd(Total, na.rm = TRUE)
coef_var <- desviacion_estandar / media
primer_cuartil <- quantile(Total, 0.25, na.rm = TRUE)
tercer_cuartil <- quantile(Total, 0.75, na.rm = TRUE)
rango_intercuartilico <- tercer_cuartil - primer_cuartil
asimetria <- skewness(Total, na.rm = TRUE)
curtosis <- kurtosis(Total, na.rm = TRUE)

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

# Mostrar la tabla con knitr y kableExtra
knitr::kable(tabla_estadisticas, caption = "Estadísticas descriptivas para la columna Total") %>%
  kableExtra::kable_styling(bootstrap_options = "striped", full_width = FALSE)
print(tabla_estadisticas)

n <- length(total)





ceiling(1+3.344*log10(1299030))
viajeros_intervalos <- cut(total, breaks = 22)
