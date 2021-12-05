main <- function() {
  # Importacion de datos
  datos <- obt_datos()
  # Tabulación de los datos
  View(datos)
  # Limpieza de datos
  x <- datos[, 4:16]
  Y <- datos[, 0:3]
  View(x)
  View(Y)
  # Intervalos de confianza
  
  # Analisis de componentes principales
  
  # Analisis de factores
  
  # Analisis Cluster
  
}

obt_datos <- function() {
  # Incializamos y declaramos la ruta del proyecto
  ruta <- "C:/Users/breko/Documents/Practicas/Programacion_Ciencia_Datos/proyecto3"
  setwd(dir = ruta)
  # Importamos los datos
  datos <- read.csv(file = "cereal.csv", header = T)
  return(datos)
}

main()