
# Función para calcular el coeficiente de silueta (fs)
silhouette_score <- function(k, data, features) {
  cat("Kmeans-Caracteristicas: ", feature, "\n")
  # Calcular el factor de silueta para las características dadas
  km <- kmeans(data[, features], centers = k, nstart = 25)
  ss <- silhouette(km$cluster, dist(data[, features]))
  # Devolver el promedio del factor de silueta
  cat("Factor de silueta: ", mean(ss[, 3]), "\n")
  return(mean(ss[,3]))
}


# Función para calcular el coeficiente de silueta utilizando Fuzzy K-Means
fuzzy_silhouette_score <- function(k, dataKM, featureSel) {
  cat("Fuzzy K-means-Caracteristicas: ", featureSel, "\n")
  # Aplicar Fuzzy K-means a los datos dataKM, k-grupos
  fuzzy_km <- fanny(dataKM[, featureSel], k,memb.exp = 1.5)
  ss <- silhouette(fuzzy_km, dist(dataKM[, featureSel]))
  cat("Factor de silueta: ", mean(ss[, 3]), "\n")
  return(mean(ss[, 3]))  # Regresar coeficiente de silueta
}












# Función para calcular el coeficiente de silueta
#silhouette_score <- function(k, dataKM, featureSel) {
#  cat("Kmeans-Caracteristicas: ", featureSel, "\n")
#  # Aplicar K-means a los datos dataKM, k-grupos
#  km <- kmeans(dataKM[, featureSel], centers = k, nstart = 25)
#  ss <- silhouette(km$cluster, dist(dataKM[, featureSel]))
# cat("Factor de silueta: ", mean(ss[, 3]), "\n")
#return(mean(ss[, 3]))  # Regresar coeficiente de silueta
#}
