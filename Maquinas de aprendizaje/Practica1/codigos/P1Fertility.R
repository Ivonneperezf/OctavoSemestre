# Cargar bibliotecas necesarias
library(dplyr)
library(GGally)
library(caret)
library(class)
library(gmodels)
fertility_measures <- read.csv("fertility_measures.csv")
fertility_measures_vista <- read.csv("fertility_measures.csv")
str(fertility_measures_vista)
View(fertility_measures)

var.names.data <-tolower(colnames(fertility_measures))
colnames(fertility_measures) <- var.names.data

var.names.data <-tolower(colnames(fertility_measures_vista))
colnames(fertility_measures_vista) <- var.names.data

head(fertility_measures)

summary(fertility_measures)
str(fertility_measures)

View(fertility_measures)


#Veremos la distribucion de las variables
par(mfrow=c(3,2))
hist(fertility_measures_vista$fertility, col="blue", breaks=20, main="fertility", xlab="fertility")
hist(fertility_measures_vista$agriculture, col="blue", breaks=20, main="agriculture", xlab="agriculture")
hist(fertility_measures_vista$examination, col="blue", breaks=20, main="examination", xlab="examination")
hist(fertility_measures_vista$education, col="blue", breaks=20, main="education", xlab="education")
hist(fertility_measures_vista$catholic, col="blue", breaks=20, main="catholic", xlab="catholic")
hist(fertility_measures_vista$infant..mortality, col="blue", breaks=20, main="infant mortality", xlab="infant mortality")

#Grafico de correlacion
ggpairs(fertility_measures[2:7])

#Algunas variables correlacionadas positivamente son
# Education- examinator
# Algunas variables correlacionadas negativamente son
# examination - fertility
# fertility - education
# examination - agriculture
# agriculture - education










#Seleccion de las variables en este caso la clasificacion con fertilidad
data_for_clustering <- fertility_measures[,"fertility"]
data_scaled <- scale(data_for_clustering)
View(data_scaled)

#Realizamos la prueba del codo para ver que k es optima para encontar el numero 
#de grupos adecuado
set.seed(123)
wcss <- numeric(5)
for (k in 1:5) {
  kmeans_result <- kmeans(data_scaled, centers = k, nstart = 10)
  wcss[k] <- kmeans_result$tot.withinss
}

par(mfrow=c(1,1))
# Graficar la prueba del codo
plot(1:5, wcss, type = "b", pch = 19, col = "blue",
     xlab = "Número de clústeres (k)", ylab = "Suma de Distancias Cuadradas",,
     main = "Prueba del codo para k-means")



# Realizar el clustering con k-means con el valor optimo de k = 3

kmeans_result <- kmeans(data_scaled, centers = 3, nstart = 25)

# Ver los resultados del clustering
kmeans_result$cluster 
# Agregar los resultados del clustering al dataset original
fertility_measures$fertility_category <- as.factor(kmeans_result$cluster)
fertility_measures_vista$fertility_category <- as.factor(kmeans_result$cluster)
# Ver el dataset con las categorías de clustering
head(fertility_measures)
# Interpretación de los clusters

#Visualizamos el promedio de los clusters

cluster_means <- aggregate(fertility_measures[,"fertility"], 
          by = list(fertility_measures$fertility_category), FUN = mean)
names(cluster_means) <- c("cluster", "mean_fertility")
View(cluster_means)

cluster_means_2 <- aggregate(fertility_measures_vista[,"fertility"], 
                           by = list(fertility_measures_vista$fertility_category), FUN = mean)
names(cluster_means_2) <- c("cluster", "mean_fertility")
View(cluster_means_2)

#Ordenamos la columna de los promedios

cluster_means <- cluster_means[order(cluster_means$mean_fertility), ]
cluster_means$category <- c("bajo", "medio", "alto")

#Reemplazamos las categorias por los factores para la variable objetivo

fertility_measures$fertility_category <- cluster_means$category[
  match(fertility_measures$fertility_category, cluster_means$cluster)
]

View(fertility_measures)

#Vemos la distribucion de las categorias
library(ggplot2)

ggplot(fertility_measures, aes(x = x, y = fertility, color = fertility_category)) +
  geom_point(size = 3) +
  labs(title = "Distribución de Fertility por Categoría",x = "Región",
    y = "Fertility",color = "Categoría") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calcular la matriz de correlación para las variables numéricas
cor(fertility_measures[,2:7])
ggpairs(fertility_measures[2:7])


#Hacemos el escalamiento de las variables
View(fertility_measures)
numerical_data <- 

  #Primera evaluacion----------------------------------------------------
# Escalar las columnas numéricas y separar datos relevantes

scaled_data <- scale(fertility_measures[, 4:5])
labels <- as.factor(fertility_measures$fertility_category)
province_names <- fertility_measures$x  

str(labels)
View(province_names)

#Division del conjunto de prueba y entrenamiento 75% - 25% para las variables 
#examination y education
# Ver la cantidad de observaciones por categoría
table(fertility_measures$fertility_category)

# Calcular las proporciones de cada categoría
prop.table(table(fertility_measures$fertility_category))

index <- createDataPartition(labels, p = 0.75, list = FALSE)

train_data <- scaled_data[index, ]
train_labels <- labels[index]
train_provinces <- province_names[index]

test_data <- scaled_data[-index, ]
test_labels <- labels[-index]
test_provinces <- province_names[-index]


#Entrena el modelo con diferentes valores de k y guarda el porcentaje de error 
#en el vector error

error <- c()
for (i in 1:8) {
  knn.fit <- knn(train = train_data, test = test_data, cl = train_labels, k = i)
  error[i] = 1 - mean(knn.fit == test_labels)
}

ggplot(data = data.frame(error), aes(x = 1:8, y = error)) +
  geom_line(color = "Blue")

#El valor minimo de error en la mayoria de los casos es 3, por lo que usaremos k =3

knn.fit <- knn(train = train_data, test = test_data, cl = train_labels, k = 3)

class_comparison <- data.frame(PredictedFertilityCategory = knn.fit, ObservedFertilityCategory = test_labels)

#head(class_comparison)

#Evaluamos los resultados
CrossTable(x = class_comparison$ObservedFertilityCategory, y = class_comparison$PredictedFertilityCategory,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE)

conf_matrix <- table(class_comparison$ObservedFertilityCategory, class_comparison$PredictedFertilityCategory)

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

# Mostrar la precisión
cat("Precisión (Accuracy):", accuracy * 100, "%")


#Segunda evaluacion----------------------------------------------------
scaled_data_2 <- scale(fertility_measures[, c("examination", "education", "catholic", "infant..mortality")])
labels_2 <- as.factor(fertility_measures$fertility_category)
province_names_2 <- fertility_measures$x  
View(scaled_data_2)

index_2 <- createDataPartition(labels_2, p = 0.75, list = FALSE)

train_data_2 <- scaled_data_2[index_2, ]
train_labels_2 <- labels_2[index_2]
train_provinces_2 <- province_names_2[index_2]

test_data_2 <- scaled_data_2[-index_2, ]
test_labels_2 <- labels_2[-index_2]
test_provinces_2 <- province_names_2[-index_2]

error_2 <- c()
#Entrena el modelo con diferentes valores de k y guarda el porcentaje de error 
#en el vector error
for (i in 1:15) {
  knn.fit_2 <- knn(train = train_data_2, test = test_data_2, cl = train_labels_2, k = i)
  error_2[i] = 1 - mean(knn.fit_2 == test_labels_2)
}

ggplot(data = data.frame(error_2), aes(x = 1:15, y = error_2)) +
  geom_line(color = "Blue")

#Evaluar con k=5
knn.fit_2 <- knn(train = train_data_2, test = test_data_2, cl = train_labels_2, k = 5)
class_comparison_2 <- data.frame(PredictedFertilityCategory = knn.fit_2, ObservedFertilityCategory = test_labels_2)

#Evaluamos los resultados
CrossTable(x = class_comparison_2$ObservedFertilityCategory, y = class_comparison_2$PredictedFertilityCategory,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE)

conf_matrix_2 <- table(class_comparison_2$ObservedFertilityCategory, class_comparison_2$PredictedFertilityCategory)

accuracy_2 <- sum(diag(conf_matrix_2)) / sum(conf_matrix_2)

# Mostrar la precisión
cat("Precisión (Accuracy):", accuracy_2 * 100, "%")
