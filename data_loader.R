# Función para cargar y preparar datos
load_and_prepare_data <- function(file_path) {
  # Cargar el conjunto de datos desde un archivo CSV
  data <- read.csv(file_path, header = TRUE)
  
  # Eliminar las columnas no deseadas
  data <- data[, !(names(data) %in% c("NObeyesdad", "FAVC", "SMOKE", "SSC", "Gender", "family_history_with_overweight"))]
  
  # Convertir datos categóricos a numéricos
  data$CAEC <- as.numeric(factor(data$CAEC, levels = c("no", "Sometimes", "Frequently", "Always"), labels = c(0, 1, 2, 3)))
  data$CALC <- as.numeric(factor(data$CALC, levels = c("no", "Sometimes", "Frequently", "Always"), labels = c(0, 1, 2, 3)))
  data$MTRANS <- as.numeric(factor(data$MTRANS, levels = c("Public_Transportation","Walking","Automobile","Motorbike","Bike"), labels = c(0, 1, 2, 3, 4)))
  
  # Filtrar columnas numéricas para calcular las medias y desviaciones estándar
  numeric_cols <- sapply(data, is.numeric)
  df_numeric <- data[, numeric_cols]
  
  # Calcular las medias y desviaciones estándar de las columnas numéricas
  mean_features <- sapply(df_numeric, mean, na.rm = TRUE)
  sd_features <- sapply(df_numeric, sd, na.rm = TRUE)
  
  # Normalizar los datos utilizando las medias y desviaciones estándar calculadas
  dataNorm <- as.data.frame(scale(df_numeric, center = mean_features, scale = sd_features))
  
  # Filtrar valores extremos por media
  isindexExtremo <- apply(dataNorm, 2, function(x) isVExtremo(x, 0.0, 1.0))
  dataNorm <- dataNorm[!apply(isindexExtremo, 1, any), ]
  
  return(dataNorm)
}
