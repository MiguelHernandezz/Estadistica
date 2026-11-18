# Paquetes (se instalan si faltan)
dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE, showWarnings = FALSE)
.libPaths(c(Sys.getenv("R_LIBS_USER"), .libPaths()))
if (!"gt" %in% rownames(installed.packages())) install.packages("gt", repos = "https://cloud.r-project.org")

# Las sumas y totales se calculan en PL2.R
source("PL2.R")

# Realizar la prueba de proporciones de dos muestras
resultadoHM <- prop.test(x = c(suma_hombres, suma_mujeres), n = c(total, total), alternative = "greater", correct = FALSE)


# Ver los resultados
print(resultadoHM)


# Realizar la prueba de proporciones de dos muestras
resultadoSM <- prop.test(x = c(suma_Madrid, suma_Soria), n = c(total, total), alternative = "greater", correct = FALSE)

# Ver los resultados
print(resultadoSM)


# Realizar la prueba de proporciones de dos muestras
resultadoAA <- prop.test(x = c(suma_10_19, suma_30_39), n = c(total, total), alternative = "greater", correct = FALSE)

# Ver los resultados
print(resultadoAA)


# Realizar la prueba de proporciones de dos muestras
resultadoIV <- prop.test(x = c(sumaInvierno, sumaVerano), n = c(total, total), alternative = "greater", correct = FALSE)

# Ver los resultados
print(resultadoIV)

# Realizar la prueba de proporciones de dos muestras
resultadoHM <- prop.test(x = c(suma_hombres_pequeño, suma_mujeres_pequeño), n = c(total_pequeño, total_pequeño), alternative = "greater", correct = FALSE)


# Ver los resultados
print(resultadoHM)


# Realizar la prueba de proporciones de dos muestras
resultadoSM <- prop.test(x = c(suma_Madrid_pequeño, suma_Soria_pequeño), n = c(total_pequeño, total_pequeño), alternative = "greater", correct = FALSE)

# Ver los resultados
print(resultadoSM)


# Realizar la prueba de proporciones de dos muestras
resultadoAA <- prop.test(x = c(suma_10_19_pequeño, suma_30_39_pequeño), n = c(total_pequeño, total_pequeño), alternative = "greater", correct = FALSE)

# Ver los resultados
print(resultadoAA)


# Realizar la prueba de proporciones de dos muestras
resultadoIV <- prop.test(x = c(sumaInvierno_pequeño, sumaVerano_pequeño), n = c(total_pequeño, total_pequeño), alternative = "greater", correct = FALSE)

# Ver los resultados
print(resultadoIV)


# Cargar la librería
library(gt)

# Crear el data frame
tabla <- data.frame(
  `Hipótesis nula (H0)` = c(
    "Proporciones de Hombres/Mujeres iguales",
    "Proporciones de Soria/Madrid iguales",
    "Proporciones de Adultos/Adolescentes iguales",
    "Proporciones de Invierno/Verano iguales"
  ),
  `Hipótesis alternativa (HA)` = c(
    "Proporción de hombres mayor que la de las mujeres",
    "Proporciones de Madrid mayor que la de Soria",
    "Proporción de adultos mayor que la de adolescentes",
    "Proporción de Invierno mayor que la de Verano"
  ),
  `p-valor` = c(1, 2.2e-16, 1, 2.2e-16)
)

# Crear el data frame
tabla_muestra_pequena <- data.frame(
  `Hipótesis nula (H0)` = c(
    "Proporciones de Hombres/Mujeres iguales",
    "Proporciones de Soria/Madrid iguales",
    "Proporciones de Adultos/Adolescentes iguales",
    "Proporciones de Invierno/Verano iguales"
  ),
  `Hipótesis alternativa (HA)` = c(
    "Proporción de hombres mayor que la de las mujeres",
    "Proporciones de Madrid mayor que la de Soria",
    "Proporción de adultos mayor que la de adolescentes",
    "Proporción de Invierno mayor que la de Verano"
  ),
  `p-valor` = c(0.0001604, 0.8413, 0.9994, 5.381e-07)
)

# Tablas en texto (consola)
print(tabla)
print(tabla_muestra_pequena)

# Crear la tabla con gt (se ve en el Viewer de RStudio; con Rscript se guarda en HTML)
tabla_gt <- gt(tabla_muestra_pequena) %>%
  tab_header(
    title = "Tabla 4. Contraste de hipótesis usando muestras pequeñas"
  ) %>%
  fmt_number(
    columns = `p.valor`, # Columna a formatear
    decimals = 4         # Número de decimales a mostrar
  )
if (interactive()) {
  print(tabla_gt)
} else {
  tryCatch(gtsave(tabla_gt, "tabla_contraste_hipotesis.html"),
           error = function(e) message("No se pudo guardar la tabla HTML: ", conditionMessage(e)))
}

