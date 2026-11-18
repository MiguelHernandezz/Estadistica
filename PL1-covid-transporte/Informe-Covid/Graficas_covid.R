# Cargar las librerías necesarias (se instalan si faltan)
paquetes <- c("knitr", "ggplot2")
dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE, showWarnings = FALSE)
.libPaths(c(Sys.getenv("R_LIBS_USER"), .libPaths()))
faltan <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan)) install.packages(faltan, repos = "https://cloud.r-project.org")
library(knitr)
library(ggplot2)

# Datos (Proyecto.R carga los CSV)
source("Proyecto.R")
numero_hospitalizados = covid2$num_hosp
numero_fallecidos = covid2$num_def
# Intervalos de Sturges: ceiling(1 + 3.322 * log10(n)) = 22
diagnostico_intervalos <- cut(numero_casos_Covid2, breaks = 22)
# Crear los datos en un data frame
tabla_resultados <- data.frame(
  Medida = c("Media", "Mediana", "Mínimo", "Máximo", "Rango", "Varianza", "Desviación estándar",
             "Coeficiente de variación", "Primer cuartil", "Tercer cuartil", "Rango intercuartílico",
             "Coeficiente de asimetría", "Curtosis"),
  resultados_diagnosticos = c(8.92, 0, 0, 3750, 3750, 2580, 50.8, 5.7, 0, 4, 4, 0.527, 998),
  resultados_hospitalizados = c(0.4, 0, 0, 271, 271, 6.38, 2.52, 6.31, 0, 0, 0, 0.476, 2080),
  resultados_fallecidos = c(0.0802, 0, 0, 100, 100, 0.65, 0.806, 10.1, 0, 0, 0, 0.298, 2963)
)

# Generar la tabla con knitr::kable
kable(tabla_resultados, caption = "Medidas Estadísticas Descriptivas", col.names = c("Medida", 
                                                                                     "Diagnosticados", "Hospitalizados", "Fallecidos"), align = "lccc")
frecuencia_absoluta_diagnosticados <- table(diagnostico_intervalos)
frecuencia_absoluta_diagnosticados
frecuencia_relativa_diagnosticados <- prop.table(frecuencia_absoluta_diagnosticados)
frecuencia_relativa_diagnosticados
tabla_frecuencias <- data.frame(
  Diagnosticados = names(frecuencia_absoluta_diagnosticados),
  Frecuencia_Absoluta = as.vector(frecuencia_absoluta_diagnosticados),
  Frecuencia_Relativa = as.vector(frecuencia_relativa_diagnosticados)
)
#Se ha repetido cuando a sido necesario en vez de crear tres variables reducidas
casosReducidos <- cut(numero_casos_Covid2, breaks = 22)
tabla_frecuencias
#Repetimos la misma tabla de frecuencias cambiando la variable principal de datos para obtener las frecuencas de el numero de diagnosticados, hospitalizados y fallecidos

kable(tabla_frecuencias, 
      caption = "Tabla de Frecuencias Absolutas y Relativas",
      col.names = c("Fallecidos", "Frecuencia Absoluta", "Frecuencia Relativa"),
      align = 'c')
ceiling(1+3.322*log10(1299030))


(total_hospitalizdos= sum(numero_hospitalizados))
(total_diagnosticados= sum(numero_casos_Covid2))
(total_fallecidos= sum(numero_fallecidos))

porcentaje_hospitalizados= (total_hospitalizdos*100)/total_diagnosticados
porcentaje_hospitalizados
porcentaje_fallecidos= (total_fallecidos*100)/total_diagnosticados
porcentaje_fallecidos

porcentajesDiagrama1= c(4.48, 0.89, 94.63)
etiquetas= c("Hospitalizados 4.48%", "Fallecidos 0.89%", "Resto de diagnosticados 94.63%")
colores <- c("cyan", "blue", "red")
pie(porcentajesDiagrama1, labels = etiquetas, col = colores, main = "Diagrama con el porcentaje de diagnosticados")

#Diagrama diagnosticados
poblacionES = 47398695
(porcentaje_diagnosticados_total = (total_diagnosticados*100)/poblacionES)
porcentajesDiagrama2= c(24.44, 75.56)
etiquetas= c("Personas Sanas 75.56%", "Personas con COVID 24.44%")
colores <- c("yellow", "green")
pie(porcentajesDiagrama2, labels = etiquetas, col = colores, main = "Diagrama con el impacto del COVID")

#Histograma
hist(numero_casos_Covid2, 
     breaks = 1000,                 # Aumenta el número de intervalos para mejor resolución
     col = "lightblue", 
     border = "black", 
     main = "Histograma del numero de diagnosticados",
     xlab = "Valores", 
     ylab = "Frecuencia",
     xlim = c(0, max(numero_fallecidos) * 1.1), # Asegúrate de cubrir el rango completo de datos
     ylim = c(0, max(hist(numero_fallecidos, plot = FALSE)$counts) * 1.1)) # Ajusta la escala Y para mejor visualización
hist(numero_casos_Covid2, breaks = 10)

fecha = covid2$fecha
NumeroDiagnosticos <- numero_casos_Covid2
plot(NumeroDiagnosticos, main = "Casos Covid")
#Diagramas de caja
boxplot(numero_casos_Covid2, main="Diagrama de caja de los diagnosticados", ylab="Valores", col="lightblue")
boxplot(numero_hospitalizados, main="Diagrama de caja de los hospitalizados", ylab="Valores", col="lightblue")
boxplot(numero_fallecidos, main="Diagrama de caja de los fallecidos", ylab="Valores", col="lightblue")
