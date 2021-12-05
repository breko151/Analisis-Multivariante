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
  #inter_conf(x)
  # Analisis de componentes principales
  acp(datos = x)
  # Analisis de factores
  #af(x)
  # Analisis Cluster
  #ac(x)
}

obt_datos <- function() {
  # Incializamos y declaramos la ruta del proyecto
  ruta <- "C:/Users/breko/Documents/Practicas/Programacion_Ciencia_Datos/proyecto3"
  setwd(dir = ruta)
  # Importamos los datos
  datos <- read.csv(file = "cereal.csv", header = T)
  return(datos)
}

inter_conf <- function(datos) {
  
}

acp <- function(datos) {
  # Obtenemos las medias de cada columna
  media <- apply(X = datos, MARGIN = 2, FUN = mean)
  # Obtenemos las varianzas de cada columna
  varianza <- apply(X = datos, MARGIN = 2, FUN = var)
  # Obtenemos las desviaciones estandar de cada columna
  desvest <- apply(X = datos, MARGIN = 2, FUN = sd)
  # Mostramos los resultados
  desc_datos <- data.frame(media, varianza, desvest)
  print(desc_datos)
  # Generamos nuestros componentes principales de manera automatica con sd = 1.
  pca <- prcomp(datos, scale = TRUE) 
  print(pca)
  print("Media PCA:")
  print(pca$center)
  print("Desviacion tipica PCA:")
  print(pca$scale)
  print("Loadings PCA:")
  print(pca$rotation)
  print("X PCA")
  print(head(pca$x))
  # Graficamos
  biplot(x = pca, scale = 0.0, cex = c(0.5, 1), col = c("blue4", "brown3"), 
         expand = 1, xlim = c(-6.0, 2.0), ylim = c(-6.0, 2.0))
  library(ggplot2)
  # Varianzas
  print("Varianzas")
  print(pca$sdev^2)
  # Porcentaje de Varianzas
  prop_varianza <- pca$sdev^2/sum(pca$sdev^2)
  print("Porcentajes Varianzas")
  print(prop_varianza)
  # Grafica de Porcentaje de Varianzas
  plot(ggplot(data = data.frame(prop_varianza, pc = 1:8),
              aes(x = pc, y = prop_varianza)) +
         geom_col(width = 0.3) +
         scale_y_continuous(limits = c(0,1)) +
         theme_bw() +
         labs(x = "Componente principal",
              y = "Prop. de varianza explicada"))
  # Porcentaje de Varianzas Acumuladas
  prop_varianza_acum <- cumsum(prop_varianza)
  print("Porcentajes Varianzas Acumuladas")
  print(prop_varianza_acum)
  # Grafica de Porcentaje de Varianzas Acumuladas
  plot(ggplot(data = data.frame(prop_varianza_acum, pc = 1:8),
              aes(x = pc, y = prop_varianza_acum, group = 1)) +
         geom_point() +
         geom_line() +
         theme_bw() +
         labs(x = "Componente principal",
              y = "Prop. varianza explicada acumulada"))
}

main()