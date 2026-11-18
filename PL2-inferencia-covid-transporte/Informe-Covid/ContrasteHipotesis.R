
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


# Instalar librería 
install.packages("gt")

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

# Crear la tabla con gt
gt(tabla_muestra_pequena) %>%
  tab_header(
    title = "Tabla 4. Contraste de hipótesis usando muestras pequeñas"
  ) %>%
  fmt_number(
    columns = `p.valor`, # Columna a formatear
    decimals = 4         # Número de decimales a mostrar
  )

