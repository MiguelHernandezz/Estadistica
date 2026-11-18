#Archivo inicial donde se han cargado las cosas necesarias al principio del proyecto
#Hay algunas lineas de prueba que no se han usado mas adelante por lo tanto es como un archivo tambien para probar cosas o hacer cosas en sucio

transporte = read.csv("transporte.csv")
Covid <- read.csv("casos_diagnostico_ccaa.csv", sep = ",", stringsAsFactors = FALSE)
num_casos = Covid$num_casos
fecha = Covid$fecha
na.omit(fecha)
na.omit(num_casos)
barplot(num_casos)
sum(num_casos)
covid2 <- read.csv("casos_hosp_uci_def_sexo_edad_provres.csv", sep = ",", stringsAsFactors = FALSE)
numero_casos_Covid = covid2$num_casos
sum(numero_casos_Covid2)
hist(mean(numero_casos_Covid2))
