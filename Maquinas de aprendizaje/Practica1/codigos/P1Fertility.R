library(GGally)
fertility_measures <- read.csv("fertility_measures.csv")
View(fertility_measures)
var.names.data <-tolower(colnames(fertility_measures))
colnames(fertility_measures) <- var.names.data
head(fertility_measures)

summary(fertility_measures)
str(fertility_measures)






# Cargar bibliotecas necesarias
library(dplyr)

#Seleccion de las variables
data_for_clustering <- fertility_measures[, c("fertility", "agriculture", "examination", "education", "catholic", "infant..mortality")]
#Normalizar las variables 
data_scaled <- scale(data_for_clustering)

# Realizar el clustering con k-means
set.seed(123)
kmeans_result <- kmeans(data_scaled, centers = 4, nstart = 25)
# Ver los resultados del clustering
kmeans_result$cluster 
# Agregar los resultados del clustering al dataset original
fertility_measures$fertility_category <- as.factor(kmeans_result$cluster)
# Ver el dataset con las categorías de clustering
head(fertility_measures)
# Interpretación de los clusters
aggregate(fertility_measures[, c("fertility", "agriculture", "examination", "education", "catholic", "infant..mortality")], 
          by = list(fertility_measures$fertility_category), FUN = mean)


View(fertility_measures)

#Veremos la distribucion de las variables
par(mfrow=c(2,2))
hist(fertility_measures$fertility, col="blue", breaks=20)
hist(fertility_measures$agriculture, col="blue", breaks=20)
hist(fertility_measures$examination, col="blue", breaks=20)
hist(fertility_measures$education, col="blue", breaks=20)

hist(fertility_measures$catholic, col="blue", breaks=20)
hist(fertility_measures$infant..mortality, col="blue", breaks=20)

#Grafico de correlacion
ggpairs(fertility_measures[2:7])

#Algunas variables correlacionadas positivamente son
# Education- examinator
# Algunas variables correlacionadas negativamente son
# examination - fertility
# examination - agriculture
# agriculture - education
# fertility - education
