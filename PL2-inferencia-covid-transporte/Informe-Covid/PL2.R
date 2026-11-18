Covid_Grande = read.csv2("casos_hosp_uci_def_sexo_edad_provres.csv", header = TRUE, sep = ",")
Covid_pequeño = read.csv2("Covid_muestra_27Datos.csv", header = TRUE, sep = ",")

casosContagios= Covid_Grande$num_casos
casosContagios_pequeño= Covid_pequeño$num_casos

#Test de normalidad
install.packages("nortest")
library(nortest)
ad.test(casosContagios)
ad.test(casosContagios_pequeño)

shapiro_test <- shapiro.test(casosContagios_pequeño)
shapiro_test

install.packages("tseries")
library(tseries)
jb_test <- jarque.bera.test(casosContagios)
jb_test
jb_test <- jarque.bera.test(casosContagios_pequeño)
jb_test

#Histogramas y graficos QQ
hist(casosContagios, breaks = 30, main = "Histograma de casos de contagios", col = "lightblue")
hist(casosContagios_pequeño, breaks = 30, main = "Histograma de casos de contagios", col = "lightblue")

qqnorm(casosContagios_pequeño)
qqline(casosContagios_pequeño, col = "red")

#Tabla de test de normalidad
# Instalar librerias
if(!require(knitr)) install.packages("knitr")
if(!require(kableExtra)) install.packages("kableExtra")

# Crear los datos para la tabla
tabla <- data.frame(
  `Test de Normalidad` = c("Alderson-Darling", "Jarque-Berca", "Shapiro-Wilk"),
  `Muestra grande contagios` = c("p-value < 2.2e-16", "p-value < 2.2e-16", ""),
  `Muestra pequeña contagios` = c("p-value = 1.386e-10", "p-value < 2.2e-16", "p-value = 6.043e-08")
)

# Generar la tabla
knitr::kable(tabla, align = "c", caption = "Tabla 1. Test de normalidad realizados") %>%
  kableExtra::kable_styling(full_width = FALSE, position = "center", latex_options = "hold_position") %>%
  kableExtra::row_spec(0, bold = TRUE) %>% # Encabezado en negrita
  kableExtra::column_spec(1, bold = TRUE, border_left = TRUE) %>% # Línea vertical izquierda
  kableExtra::column_spec(2:3, bold = TRUE, border_left = TRUE, border_right = TRUE) %>% # Líneas verticales entre columnas
  kableExtra::row_spec(nrow(tabla), extra_css = "border-bottom: 2px solid;") %>% # Línea horizontal inferior
  kableExtra::add_header_above(c(" " = 1, "Muestra grande" = 1, "Muestra pequeña" = 1), bold = TRUE)

#Analisis de la homogeneidad de la varianza

install.packages("car")
library(car)
# Crear un data frame
datosLevene <- data.frame(
  valores = c(casosContagios, casosContagios_pequeño),
  grupo = factor(c(rep("Grande", length(casosContagios)), rep("Pequeño", length(casosContagios_pequeño))))
)

# Realizar el test de Levene
resultado <- leveneTest(valores ~ grupo, data = datosLevene)

# Mostrar los resultados
print(resultado)

#Calculo intervalos de confianza 95% Media
install.packages("BSDA")
library(BSDA)
z.test(casosContagios, sigma.x=sd(casosContagios), conf.level=0.95)
t.test(casosContagios_pequeño, conf.level=0.95)
#Grafico intervalos media
# Datos
media_grande <- 8.918638
ci_inf_grande <- 8.831292
ci_sup_grande <- 9.005983

media_pequeña <- 8.703704
ci_inf_pequeña <- 2.05450
ci_sup_pequeña <- 15.35291


#Total variables proporciones
total= sum(Covid_Grande$num_casos)
print(total)
suma_hombres <- sum(Covid_Grande$num_casos[Covid_Grande$sexo == "H"])
print(suma_hombres)
suma_mujeres <- sum(Covid_Grande$num_casos[Covid_Grande$sexo == "M"])
print(suma_mujeres)
suma_mujeres <- sum(Covid_Grande$num_casos[Covid_Grande$sexo == "M"])
suma_Soria <- sum(Covid_Grande$num_casos[Covid_Grande$provincia_iso == "SO"], na.rm = TRUE)
print(suma_Soria)
suma_Madrid <- sum(Covid_Grande$num_casos[Covid_Grande$provincia_iso == "MA"], na.rm = TRUE)
print(suma_Madrid)
suma_10_19 <- sum(Covid_Grande$num_casos[Covid_Grande$grupo_edad == "10-19"], na.rm = TRUE)
print(suma_10_19)
suma_30_39 <- sum(Covid_Grande$num_casos[Covid_Grande$grupo_edad == "30-39"], na.rm = TRUE)
print(suma_30_39)

#Total variables proporciones Muestra pequena
total_pequeño <- sum(Covid_pequeño$num_casos)
print(total_pequeño)

suma_hombres_pequeño <- sum(Covid_pequeño$num_casos[Covid_pequeño$sexo == "H"])
print(suma_hombres_pequeño)

suma_mujeres_pequeño <- sum(Covid_pequeño$num_casos[Covid_pequeño$sexo == "M"])
print(suma_mujeres_pequeño)

suma_Soria_pequeño <- sum(Covid_pequeño$num_casos[Covid_pequeño$provincia_iso == "SO"], na.rm = TRUE)
print(suma_Soria_pequeño)

suma_Madrid_pequeño <- sum(Covid_pequeño$num_casos[Covid_pequeño$provincia_iso == "MA"], na.rm = TRUE)
print(suma_Madrid_pequeño)

suma_10_19_pequeño <- sum(Covid_pequeño$num_casos[Covid_pequeño$grupo_edad == "10-19"], na.rm = TRUE)
print(suma_10_19_pequeño)

suma_30_39_pequeño <- sum(Covid_pequeño$num_casos[Covid_pequeño$grupo_edad == "30-39"], na.rm = TRUE)
print(suma_30_39_pequeño)


install.packages("dplyr")
library(dplyr)
fechas= Covid_Grande$fecha <- as.Date(Covid_Grande$fecha)
datos_filtradosInvierno <- Covid_Grande %>%
  filter(format(fecha, "%Y-%m") %in% c("2020-12", "2021-01", "2021-02", "2021-12", "2022-01", "2022-02"))
print(datos_filtrados)
datos_filtradosVerano <- Covid_Grande %>%
  filter(format(fecha, "%Y-%m") %in% c("2020-06", "2020-07", "2020-08", "2021-06", "2021-07", "2021-08"))
sumaInvierno <- sum(datos_filtradosInvierno$num_casos)
print(sumaInvierno)
sumaVerano <- sum(datos_filtradosVerano$num_casos)
print(sumaVerano)


fechas_pequeño = Covid_pequeño$fecha <- as.Date(Covid_pequeño$fecha)
datos_filtradosInvierno_pequeño <- Covid_pequeño %>%
  filter(format(fecha, "%Y-%m") %in% c("2020-12", "2021-01", "2021-02", "2021-12", "2022-01", "2022-02"))
print(datos_filtradosInvierno_pequeño)
datos_filtradosVerano_pequeño <- Covid_pequeño %>%
  filter(format(fecha, "%Y-%m") %in% c("2020-06", "2020-07", "2020-08", "2021-06", "2021-07", "2021-08"))
sumaInvierno_pequeño <- sum(datos_filtradosInvierno_pequeño$num_casos)
print(sumaInvierno_pequeño)
sumaVerano_pequeño <- sum(datos_filtradosVerano_pequeño$num_casos)
print(sumaVerano_pequeño)



#Proporciones
proporcion_hombres <- suma_hombres / total
print(proporcion_hombres)
proporcion_mujeres <- suma_mujeres / total
print(proporcion_mujeres)
proporcion_Soria <- suma_Soria / total
print(proporcion_Soria)
proporcion_Madrid <- suma_Madrid / total
print(proporcion_Madrid)
proporcion_adolescentes <- suma_10_19 / total
print(proporcion_adolescentes)
proporcion_adultos <- suma_30_39 / total
print(proporcion_adultos)
proporcionInvierno <- sumaInvierno / total
print(proporcionInvierno)
proporcionVerano <- sumaVerano / total
print(proporcionVerano)


#Intervalos de confianza
intervaloHombres= prop.test(suma_hombres, total, conf.level = 0.95, correct = FALSE)
print(intervaloHombres)
intervaloMujeres= prop.test(suma_mujeres, total, conf.level = 0.95, correct = FALSE)
print(intervaloMujeres)
intervaloSoria= prop.test(suma_Soria, total, conf.level = 0.95, correct = FALSE)
print(intervaloSoria)
intervaloMadrid= prop.test(suma_Madrid, total, conf.level = 0.95, correct = FALSE)
print(intervaloMadrid)
intervaloAdolescentes= prop.test(suma_10_19, total, conf.level = 0.95, correct = FALSE)
print(intervaloAdolescentes)
intervaloAdultos= prop.test(suma_30_39, total, conf.level = 0.95, correct = FALSE)
print(intervaloAdultos)
intervaloInvierno= prop.test(sumaInvierno, total, conf.level = 0.95, correct = FALSE)
print(intervaloInvierno)
intervaloVerano= prop.test(sumaVerano, total, conf.level = 0.95, correct = FALSE)
print(intervaloVerano)

# Proporciones pequeño
proporcion_hombres_pequeño <- suma_hombres_pequeño / total_pequeño
print(proporcion_hombres_pequeño)

proporcion_mujeres_pequeño <- suma_mujeres_pequeño / total_pequeño
print(proporcion_mujeres_pequeño)

proporcion_Soria_pequeño <- suma_Soria_pequeño / total_pequeño
print(proporcion_Soria_pequeño)

proporcion_Madrid_pequeño <- suma_Madrid_pequeño / total_pequeño
print(proporcion_Madrid_pequeño)

proporcion_adolescentes_pequeño <- suma_10_19_pequeño / total_pequeño
print(proporcion_adolescentes_pequeño)

proporcion_adultos_pequeño <- suma_30_39_pequeño / total_pequeño
print(proporcion_adultos_pequeño)

proporcionInvierno_pequeño <- sumaInvierno_pequeño / total_pequeño
print(proporcionInvierno_pequeño)

proporcionVerano_pequeño <- sumaVerano_pequeño / total_pequeño
print(proporcionVerano_pequeño)

# Intervalos de confianza pequeño
intervaloHombres_pequeño <- prop.test(suma_hombres_pequeño, total_pequeño, conf.level = 0.95, correct = FALSE)
print(intervaloHombres_pequeño)

intervaloMujeres_pequeño <- prop.test(suma_mujeres_pequeño, total_pequeño, conf.level = 0.95, correct = FALSE)
print(intervaloMujeres_pequeño)

intervaloSoria_pequeño <- prop.test(suma_Soria_pequeño, total_pequeño, conf.level = 0.95, correct = FALSE)
print(intervaloSoria_pequeño)

intervaloMadrid_pequeño <- prop.test(suma_Madrid_pequeño, total_pequeño, conf.level = 0.95, correct = FALSE)
print(intervaloMadrid_pequeño)

intervaloAdolescentes_pequeño <- prop.test(suma_10_19_pequeño, total_pequeño, conf.level = 0.95, correct = FALSE)
print(intervaloAdolescentes_pequeño)

intervaloAdultos_pequeño <- prop.test(suma_30_39_pequeño, total_pequeño, conf.level = 0.95, correct = FALSE)
print(intervaloAdultos_pequeño)

intervaloInvierno_pequeño <- prop.test(sumaInvierno_pequeño, total_pequeño, conf.level = 0.95, correct = FALSE)
print(intervaloInvierno_pequeño)

intervaloVerano_pequeño <- prop.test(sumaVerano_pequeño, total_pequeño, conf.level = 0.95, correct = FALSE)
print(intervaloVerano_pequeño)



#Tabla muestra grande
# Cargar la librería necesaria
library(knitr)

tabla <- data.frame(
  Medidas = c(
    "Tamaño muestra",
    "Media (muestra)",
    "Mediana (muestra)",
    "IC (95%) Mediana",
    "IC (95%) Media",
    "IC (95%) Prop. Hombres",
    "Prop. Hombres (muestra)",
    "IC (95%) Prop. Mujeres",
    "Prop. Mujeres (muestra)",
    "IC (95%) Prop. Soria",
    "Prop. Soria (muestra)",
    "IC (95%) Prop. Madrid",
    "Prop. Madrid (muestra)",
    "IC (95%) Prop. Adolescentes",
    "Prop. Adolescentes (muestra)",
    "IC (95%) Prop. Adultos",
    "Prop. Adultos (muestra)",
    "IC (95%) Invierno",
    "Prop. Invierno (muestra)",
    "IC (95%) Verano",
    "Prop. Verano (muestra)"
  ),
  Valores = c(
    total,                           # Tamaño muestra
    mean(Covid_Grande$num_casos),    # Media
    median(Covid_Grande$num_casos),  # Mediana
    paste0("[", round(ci_inf_grande, 2), ", ", round(ci_sup_grande, 2), "]"), # IC Mediana
    "",                              # Placeholder para IC Media
    paste0("[", round(intervaloHombres$conf.int[1] * 100, 2), "%, ", round(intervaloHombres$conf.int[2] * 100, 2), "%]"), # IC Hombres
    round(proporcion_hombres * 100, 2),              # Proporción Hombres
    paste0("[", round(intervaloMujeres$conf.int[1] * 100, 2), "%, ", round(intervaloMujeres$conf.int[2] * 100, 2), "%]"), # IC Mujeres
    round(proporcion_mujeres * 100, 2),              # Proporción Mujeres
    paste0("[", round(intervaloSoria$conf.int[1] * 100, 2), "%, ", round(intervaloSoria$conf.int[2] * 100, 2), "%]"),     # IC Soria
    round(proporcion_Soria * 100, 2),                # Proporción Soria
    paste0("[", round(intervaloMadrid$conf.int[1] * 100, 2), "%, ", round(intervaloMadrid$conf.int[2] * 100, 2), "%]"),   # IC Madrid
    round(proporcion_Madrid * 100, 2),               # Proporción Madrid
    paste0("[", round(intervaloAdolescentes$conf.int[1] * 100, 2), "%, ", round(intervaloAdolescentes$conf.int[2] * 100, 2), "%]"), # IC Adolescentes
    round(proporcion_adolescentes * 100, 2),         # Proporción Adolescentes
    paste0("[", round(intervaloAdultos$conf.int[1] * 100, 2), "%, ", round(intervaloAdultos$conf.int[2] * 100, 2), "%]"), # IC Adultos
    round(proporcion_adultos * 100, 2),              # Proporción Adultos
    paste0("[", round(intervaloInvierno$conf.int[1] * 100, 2), "%, ", round(intervaloInvierno$conf.int[2] * 100, 2), "%]"), # IC Invierno
    round(proporcionInvierno * 100, 2),              # Proporción Invierno
    paste0("[", round(intervaloVerano$conf.int[1] * 100, 2), "%, ", round(intervaloVerano$conf.int[2] * 100, 2), "%]"),   # IC Verano
    round(proporcionVerano * 100, 2)                 # Proporción Verano
  )
)

# Imprimir la tabla
kable(tabla, caption = "Medidas e Intervalos de Confianza")

# Crear la tabla con los datos de la muestra pequeña
tabla_pequeña <- data.frame(
  Medidas = c(
    "Tamaño muestra",
    "Media (muestra)",
    "Mediana (muestra)",
    "IC (95%) Media",
    "IC (95%) Prop. Hombres",
    "Prop. Hombres (muestra)",
    "IC (95%) Prop. Mujeres",
    "Prop. Mujeres (muestra)",
    "IC (95%) Prop. Soria",
    "Prop. Soria (muestra)",
    "IC (95%) Prop. Madrid",
    "Prop. Madrid (muestra)",
    "IC (95%) Prop. Adolescentes",
    "Prop. Adolescentes (muestra)",
    "IC (95%) Prop. Adultos",
    "Prop. Adultos (muestra)",
    "IC (95%) Invierno",
    "Prop. Invierno (muestra)",
    "IC (95%) Verano",
    "Prop. Verano (muestra)"
  ),
  Valores = c(
    total_pequeño,                           # Tamaño muestra
    mean(Covid_pequeño$num_casos),           # Media
    median(Covid_pequeño$num_casos),         # Mediana
    paste0("[", round(ci_inf_pequeña, 2), ", ", round(ci_sup_pequeña, 2), "]"), # IC Media
    paste0("[", round(intervaloHombres_pequeño$conf.int[1] * 100, 2), "%, ", round(intervaloHombres_pequeño$conf.int[2] * 100, 2), "%]"), # IC Hombres
    round(proporcion_hombres_pequeño * 100, 2),              # Proporción Hombres
    paste0("[", round(intervaloMujeres_pequeño$conf.int[1] * 100, 2), "%, ", round(intervaloMujeres_pequeño$conf.int[2] * 100, 2), "%]"), # IC Mujeres
    round(proporcion_mujeres_pequeño * 100, 2),              # Proporción Mujeres
    paste0("[", round(intervaloSoria_pequeño$conf.int[1] * 100, 2), "%, ", round(intervaloSoria_pequeño$conf.int[2] * 100, 2), "%]"),     # IC Soria
    round(proporcion_Soria_pequeño * 100, 2),                # Proporción Soria
    paste0("[", round(intervaloMadrid_pequeño$conf.int[1] * 100, 2), "%, ", round(intervaloMadrid_pequeño$conf.int[2] * 100, 2), "%]"),   # IC Madrid
    round(proporcion_Madrid_pequeño * 100, 2),               # Proporción Madrid
    paste0("[", round(intervaloAdolescentes_pequeño$conf.int[1] * 100, 2), "%, ", round(intervaloAdolescentes_pequeño$conf.int[2] * 100, 2), "%]"), # IC Adolescentes
    round(proporcion_adolescentes_pequeño * 100, 2),         # Proporción Adolescentes
    paste0("[", round(intervaloAdultos_pequeño$conf.int[1] * 100, 2), "%, ", round(intervaloAdultos_pequeño$conf.int[2] * 100, 2), "%]"), # IC Adultos
    round(proporcion_adultos_pequeño * 100, 2),              # Proporción Adultos
    paste0("[", round(intervaloInvierno_pequeño$conf.int[1] * 100, 2), "%, ", round(intervaloInvierno_pequeño$conf.int[2] * 100, 2), "%]"), # IC Invierno
    round(proporcionInvierno_pequeño * 100, 2),              # Proporción Invierno
    paste0("[", round(intervaloVerano_pequeño$conf.int[1] * 100, 2), "%, ", round(intervaloVerano_pequeño$conf.int[2] * 100, 2), "%]"),   # IC Verano
    round(proporcionVerano_pequeño * 100, 2)                 # Proporción Verano
  )
)

# Imprimir la tabla
kable(tabla_pequeña, caption = "Medidas e Intervalos de Confianza (Muestra Pequeña)")


#Graficos 
# Calcular errores 
error_grande <- ci_sup_grande - media_grande
error_pequeña <- ci_sup_pequeña - media_pequeña

# Crear el gráfico
medias <- c(media_grande, media_pequeña)
errores <- c(error_grande, error_pequeña)
x <- c(1, 2)

# Dibujar el gráfico
plot(x, medias, ylim = c(0, 16), pch = 19, col = "red", xaxt = "n",
     xlab = "Tipo de muestra", ylab = "Casos contagios",
     main = "Media de casos de contagios con intervalos de confianza del 95%")
arrows(x, medias - errores, x, medias + errores, angle = 90, code = 3, length = 0.1, col = "black")
axis(1, at = x, labels = c("Muestra grande", "Muestra pequeña"))
grid(nx = NULL, ny = NULL, lty = "dotted", col = "gray")

# Función para generar gráficos individuales
crear_grafico <- function(proporcion_grande, error_grande, proporcion_pequeña, error_pequeña, titulo) {
  medias <- c(proporcion_grande, proporcion_pequeña)
  errores <- c(error_grande, error_pequeña)
  x <- c(1, 2)
  
  # Crear gráfico
  plot(x, medias, ylim = c(0, max(medias + errores) + 5), pch = 19, col = "red", xaxt = "n",
       xlab = "Tipo de muestra", ylab = "Proporción (%)", main = titulo)
  arrows(x, medias - errores, x, medias + errores, angle = 90, code = 3, length = 0.1, col = "black")
  axis(1, at = x, labels = c("Muestra grande", "Muestra pequeña"))
  grid(nx = NULL, ny = NULL, lty = "dotted", col = "gray")
}

# Generar gráficos para cada proporción
crear_grafico(proporcion_hombres * 100, (intervaloHombres$conf.int[2] - proporcion_hombres) * 100,
              proporcion_hombres_pequeño * 100, (intervaloHombres_pequeño$conf.int[2] - proporcion_hombres_pequeño) * 100,
              "Proporción de Hombres")

crear_grafico(proporcion_mujeres * 100, (intervaloMujeres$conf.int[2] - proporcion_mujeres) * 100,
              proporcion_mujeres_pequeño * 100, (intervaloMujeres_pequeño$conf.int[2] - proporcion_mujeres_pequeño) * 100,
              "Proporción de Mujeres")

crear_grafico(proporcion_Soria * 100, (intervaloSoria$conf.int[2] - proporcion_Soria) * 100,
              proporcion_Soria_pequeño * 100, (intervaloSoria_pequeño$conf.int[2] - proporcion_Soria_pequeño) * 100,
              "Proporción en Soria")

crear_grafico(proporcion_Madrid * 100, (intervaloMadrid$conf.int[2] - proporcion_Madrid) * 100,
              proporcion_Madrid_pequeño * 100, (intervaloMadrid_pequeño$conf.int[2] - proporcion_Madrid_pequeño) * 100,
              "Proporción en Madrid")

crear_grafico(proporcion_adolescentes * 100, (intervaloAdolescentes$conf.int[2] - proporcion_adolescentes) * 100,
              proporcion_adolescentes_pequeño * 100, (intervaloAdolescentes_pequeño$conf.int[2] - proporcion_adolescentes_pequeño) * 100,
              "Proporción de Adolescentes")

crear_grafico(proporcion_adultos * 100, (intervaloAdultos$conf.int[2] - proporcion_adultos) * 100,
              proporcion_adultos_pequeño * 100, (intervaloAdultos_pequeño$conf.int[2] - proporcion_adultos_pequeño) * 100,
              "Proporción de Adultos")

crear_grafico(proporcionInvierno * 100, (intervaloInvierno$conf.int[2] - proporcionInvierno) * 100,
              proporcionInvierno_pequeño * 100, (intervaloInvierno_pequeño$conf.int[2] - proporcionInvierno_pequeño) * 100,
              "Proporción en Invierno")

crear_grafico(proporcionVerano * 100, (intervaloVerano$conf.int[2] - proporcionVerano) * 100,
              proporcionVerano_pequeño * 100, (intervaloVerano_pequeño$conf.int[2] - proporcionVerano_pequeño) * 100,
              "Proporción en Verano")



#Graficos QQ
# Instalar paquetes
if (!requireNamespace("ggplot2")) install.packages("ggplot2")
if (!requireNamespace("qqplotr")) install.packages("qqplotr")

# Cargar las bibliotecas
library(ggplot2)
library(qqplotr)


# Crear un dataframe con los datos
data <- data.frame(casos = casosContagios_pequeño)

ggplot(data, aes(sample = casos)) +
  stat_qq_band(alpha = 0.4, fill = "blue") +  # Bandas de confianza en azul
  stat_qq_line(color = "blue") +             # Línea de referencia
  stat_qq_point(size = 1) +                  # Puntos Q-Q
  labs(
    title = "Distribución Q-Q de los casos de contagios",
    x = "Cuantiles normales",
    y = "Número de casos de contagios"
  ) +
  theme_minimal()














