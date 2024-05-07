library(corrplot)
library(cluster)
library(ggplot2)

# Establecer semilla para reproducibilidad
set.seed(123)

# Ruta del archivo CSV
setwd("your_path//")
file_path <- "your_path//ObesityDataSet_raw_and_data_sinthetic.csv"

# Cargar módulos
source("data_loader.R")
source("silhouette_calculator.R")

# Cargar y preparar los datos
dataClientesN <- load_and_prepare_data(file_path)

# Datos de ejemplo
data <- dataClientesN




#################################### K MEANS ####################################

# Listas para almacenar los resultados finales
best_features_list <- list()
all_selected_features_list <- list()

# Paso 1: Calcular fs de todas las características y guardar la mejor
best_feature <- ""
best_fs <- -Inf
for (feature in colnames(data)) {
  fs <- silhouette_score(2, data, feature)
  if (fs > best_fs) {
    best_fs <- fs
    best_feature <- feature
  }
}

# Realizar selección de características para cada valor de k de 2 a 10
for (k in 2:10) {
  print("-------------------------------------------------------------------------------------------------------------------------------------------------------------------------")
  cat("Cluster seleccionado:", k, "\n")
  
  selected_features <- c(best_feature)
  selected_fs <- c(best_fs)
  
  # Paso 2-6: Iterar para comparar todas las características con las seleccionadas
  for (i in 2:ncol(data)) {
    # Obtener las características no seleccionadas
    remaining_features <- colnames(data)[!(colnames(data) %in% selected_features)]
    best_feature <- ""
    best_fs <- -Inf
    for (feature in remaining_features) {
      # Calcular fs de la característica contra las seleccionadas
      fs <- silhouette_score(k, data, c(selected_features, feature))
      if (fs > best_fs) {
        best_fs <- fs
        best_feature <- feature
      }
    }
    # Agregar la mejor característica y su fs a los arreglos seleccionados
    selected_features <- c(selected_features, best_feature)
    selected_fs <- c(selected_fs, best_fs)
    cat("----------------------------- Selected_features  -> ",selected_features, "\n")
    cat("----------------------------- FS  -> ",selected_fs, "\n")
  }
  
  # Almacenar la mejor característica y su puntaje FS
  best_features_list[[k - 1]] <- list(Feature = selected_features[1], FS = selected_fs[1])
  
  # Almacenar todas las características seleccionadas y sus puntajes FS correspondientes
  all_selected_features_list[[k - 1]] <- list(Features = selected_features, FS = selected_fs)
  
  # Crear el dataframe para la visualización
  dataF <- data.frame(numFeature = c(1:length(selected_fs)), 
                      Feature = selected_features, 
                      FS = selected_fs)
  dataF$numFeature <- factor(dataF$numFeature)
  
  # Graficar utilizando ggplot2 y guardar el gráfico en un archivo
  p <- ggplot(dataF, aes(numFeature, FS, group = 1)) + geom_point() + geom_line() +
    ggtitle(paste("Selección envolvente de características para k =", k)) +
    xlab("Número de características seleccionadas") +
    ylab("Factor de Silueta (FS)")
  print(p)
  
  # Guardar el gráfico en un archivo
  ggsave(paste("graficos_k_means", k, ".png"), plot = p)
}

# Mostrar las mejores características y sus puntajes FS para cada valor de k
for (i in 1:length(best_features_list)) {
  cat("Para k =", i + 1, ", la mejor característica es", best_features_list[[i]]$Feature, "con un FS de", best_features_list[[i]]$FS, "\n")
}


# Crear un dataframe para almacenar los datos de la gráfica
graph_data <- data.frame(K = integer(), Feature = character(), FS = numeric())

# Llenar el dataframe con las características seleccionadas hasta el índice del máximo valor de FS para cada valor de k
for (i in 1:length(all_selected_features_list)) {
  max_fs_index <- which.max(all_selected_features_list[[i]]$FS)
  max_features <- all_selected_features_list[[i]]$Features[1:max_fs_index]
  max_fs <- all_selected_features_list[[i]]$FS[max_fs_index]
  
  k <- rep(i + 1, length(max_features))
  
  graph_data <- rbind(graph_data, data.frame(K = k, Feature = max_features, FS = max_fs))
}

# Graficar utilizando ggplot2
p <- ggplot(graph_data, aes(x = factor(K), y = FS, fill = Feature)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Características seleccionadas hasta el máximo FS para cada valor de k") +
  xlab("Valor de k") +
  ylab("Puntaje FS") +
  theme_minimal() +
  theme(legend.position = "top")
print(p)

# Mejor agrupamiento, indicar centros de los grupos y nombres de las aplicaciones que contiene cada grupo

# Realizar el agrupamiento con K-means para el mejor valor de K
best_k <- 2  

# Realizar el agrupamiento K-means
kmeans_result <- kmeans(data, centers = best_k)

# Obtener los centroides de los grupos
centroids <- kmeans_result$centers

# Asignar las observaciones a los grupos correspondientes
group_assignments <- kmeans_result$cluster

# Crear un dataframe para almacenar los nombres de las aplicaciones y las asignaciones de grupos
app_group_data <- data.frame(App = rownames(data), Group = group_assignments)

# Imprimir los nombres de las aplicaciones en cada grupo
for (i in 1:best_k) {
  cat("Grupo", i, ":", "\n")
  cat("Centroide:", centroids[i, ], "\n")
  cat("Aplicaciones:", "\n")
  apps_in_group <- app_group_data$app[app_group_data$Group == i]
  cat(apps_in_group, "\n\n")
}

############################## FUZZY K MEANS ###################################


# Listas para almacenar los resultados finales
best_features_list_fuzzy <- list()
all_selected_features_list_fuzzy <- list()

# Paso 1: Calcular fs de todas las características y guardar la mejor
best_feature_fuzzy <- ""
best_fs_fuzzy <- -Inf
for (feature in colnames(data)) {
  fs_fuzzy <- fuzzy_silhouette_score(2, data, feature)
  if (fs_fuzzy > best_fs_fuzzy) {
    best_fs_fuzzy <- fs_fuzzy
    best_feature_fuzzy <- feature
  }
}

# Realizar selección de características para cada valor de k de 2 a 10
for (k in 2:10) {
  print("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
  cat("Cluster seleccionado:", k, "\n")
  
  selected_features_fuzzy <- c(best_feature_fuzzy)
  selected_fs_fuzzy <- c(best_fs_fuzzy)
  
  # Paso 2-6: Iterar para comparar todas las características con las seleccionadas
  for (i in 2:ncol(data)) {
    # Obtener las características no seleccionadas
    remaining_features_fuzzy <- colnames(data)[!(colnames(data) %in% selected_features_fuzzy)]
    best_feature_fuzzy <- ""
    best_fs_fuzzy <- -Inf
    for (feature in remaining_features_fuzzy) {
      # Calcular fs de la característica contra las seleccionadas
      fs_fuzzy <- fuzzy_silhouette_score(k, data, c(selected_features_fuzzy, feature))
      if (fs_fuzzy > best_fs_fuzzy) {
        best_fs_fuzzy <- fs_fuzzy
        best_feature_fuzzy <- feature
      }
    }
    # Agregar la mejor característica y su fs a los arreglos seleccionados
    selected_features_fuzzy <- c(selected_features_fuzzy, best_feature_fuzzy)
    selected_fs_fuzzy <- c(selected_fs_fuzzy, best_fs_fuzzy)
    cat("++++++++++++++++++++++++++++++ Selected_features  -> ", selected_features_fuzzy, "\n")
    cat("++++++++++++++++++++++++++++++ FS  -> ", selected_fs_fuzzy, "\n")
  }
  
  # Almacenar la mejor característica y su puntaje FS
  best_features_list_fuzzy[[k - 1]] <- list(Feature = selected_features_fuzzy[1], FS = selected_fs_fuzzy[1])
  
  # Almacenar todas las características seleccionadas y sus puntajes FS correspondientes
  all_selected_features_list_fuzzy[[k - 1]] <- list(Features = selected_features_fuzzy, FS = selected_fs_fuzzy)
  
  # Crear el dataframe para la visualización
  dataF_fuzzy <- data.frame(numFeature = c(1:length(selected_fs_fuzzy)), 
                            Feature = selected_features_fuzzy, 
                            FS = selected_fs_fuzzy)
  dataF_fuzzy$numFeature <- factor(dataF_fuzzy$numFeature)
  
  # Graficar utilizando ggplot2 y guardar el gráfico en un archivo
  p_fuzzy <- ggplot(dataF_fuzzy, aes(numFeature, FS, group = 1)) + geom_point() + geom_line() +
    ggtitle(paste("Selección envolvente de características para k =", k)) +
    xlab("Número de características seleccionadas") +
    ylab("Factor de Silueta (FS)")
  print(p_fuzzy)
  
  # Guardar el gráfico en un archivo
  ggsave(paste("graficos_fuzzy_k_means", k, ".png"), plot = p_fuzzy)
}

# Crear un dataframe para almacenar los datos de la gráfica
graph_data_fuzzy <- data.frame(K = integer(), Feature = character(), FS = numeric())

# Llenar el dataframe con las características seleccionadas hasta el índice del máximo valor de FS para cada valor de k
for (i in 1:length(all_selected_features_list_fuzzy)) {
  max_fs_index_fuzzy <- which.max(all_selected_features_list_fuzzy[[i]]$FS)
  max_features_fuzzy <- all_selected_features_list_fuzzy[[i]]$Features[1:max_fs_index_fuzzy]
  max_fs_fuzzy <- all_selected_features_list_fuzzy[[i]]$FS[max_fs_index_fuzzy]
  
  k_fuzzy <- rep(i + 1, length(max_features_fuzzy))
  
  graph_data_fuzzy <- rbind(graph_data_fuzzy, data.frame(K = k_fuzzy, Feature = max_features_fuzzy, FS = max_fs_fuzzy))
}

# Graficar utilizando ggplot2
p_fuzzy <- ggplot(graph_data_fuzzy, aes(x = factor(K), y = FS, fill = Feature)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Características seleccionadas hasta el máximo FS para cada valor de k") +
  xlab("Valor de k") +
  ylab("Puntaje FS") +
  theme_minimal() +
  theme(legend.position = "top")
print(p_fuzzy)

# Realizar el agrupamiento con K-means para el mejor valor de K
best_k <- 2  

# Realizar el agrupamiento K-means
kmeans_result <- kmeans(data, centers = best_k)

# Obtener los centroides de los grupos
centroids <- kmeans_result$centers

# Asignar las observaciones a los grupos correspondientes
group_assignments <- kmeans_result$cluster

# Crear un dataframe para almacenar los nombres de las aplicaciones y las asignaciones de grupos
app_group_data <- data.frame(App = rownames(data), Group = group_assignments)

# Imprimir los nombres de las aplicaciones en cada grupo
for (i in 1:best_k) {
  cat("Grupo", i, ":", "\n")
  cat("Centroide:", centroids[i, ], "\n")
  cat("Aplicaciones:", "\n")
  apps_in_group <- app_group_data$app[app_group_data$Group == i]
  cat(apps_in_group, "\n\n")
}

