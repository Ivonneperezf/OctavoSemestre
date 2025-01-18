library("dplyr")
library("class")
library("ggplot2")
library("GGally")
library("caret")
set.seed(12345)

#Carga del dataset
pokemon_data <- read.csv("pokemon.csv")
View(pokemon_data)

#Creamos un subconjunto solo con atributos para un subconjunto
reduced_pokemon_data <- pokemon_data %>%
  select(name, attack, defense, sp_attack, sp_defense, speed, hp, type1, type2)
head(reduced_pokemon_data)

#Subcojunto solo con algunos tipos y un solo tipo
final_dataset <- reduced_pokemon_data %>%
  filter( type2 == "" & type1 %in% c( "bug", "dragon", "fighting", "electric", "normal"))
head(final_dataset)
View(final_dataset)

#Se quita la columna tipo 2 ya que no entra en los criterios
#y se clasifica en tipos el tipo 1
final_dataset <- final_dataset %>%
  select(-c(type2)) %>%
  mutate_at(vars(type1), ~(factor(type1)))
View(final_dataset)

#Seleccionamos el 75% del dataset de manera aleatoria
random_rows <- sample(1:nrow(final_dataset), nrow(final_dataset) * .75)

#Conjunto de entrenamiento 75%
training_data <- final_dataset[random_rows, ]
  # Renumber the rows
row.names(training_data) <- 1:nrow(training_data)
View(training_data)

#Conjunto de prueba 25%
testing_data <- final_dataset[-random_rows, ]
View(testing_data)

#Conjunto de validacion 30% del 75% de entrenamiento
random_rows_validation <- sample(1:nrow(training_data), nrow(training_data) * .30)
validation_data <- training_data[random_rows_validation, ]
View(validation_data)

#El resto, es decir 70% del 75% para el entrenamiento total
training_data <- training_data[-random_rows_validation, ]
View(training_data)

ggplot(data = training_data, aes(x = attack, y = speed, col = type1)) +
  geom_point(size = 3) +
  ggtitle("Attack versus Speed for Pokemon of a Single Type (training)")

#Combinamos el conjunto de entrenamiento y prueba para analizar el conjunto de los datos
combined_training_test <- rbind(training_data, testing_data)
View(training_data)
View(testing_data)
combined_training_test$type1 <- factor(c(as.character(training_data$type1), rep(NA,
                                                                              nrow(testing_data))))
ggplot(data = combined_training_test, aes(x = attack, y = speed, col = type1)) +
   geom_point(size = 3) +
   ggtitle("Attack versus Speed for Pokemon of a Single Type (training and testing)")

#Visualizacion por pares de cada variable disponible
pairs(final_dataset[, 2:8],col = training_data$type1,lower.panel=NULL)

#Aplicacion de KNN con velocidad/ataque
training_data_speed_attack <- training_data %>%
  select(c(speed,attack))
testing_data_speed_attack <- testing_data %>%
   select(c(speed,attack))
knn_attack_speed <- knn(train = training_data_speed_attack, test = testing_data_speed_attack, cl
                        = training_data$type1, k = 5)
confusionMatrix(knn_attack_speed,testing_data$type1 )


# Definir los parámetros de control para la validación cruzada
trControl <- trainControl(method = "cv", number = 10)
# Entrenar el modelo KNN con validación cruzada
fit <- train(type1 ~ speed + attack,
             method = "knn",
             tuneGrid = expand.grid(k = 1:5),
             trControl = trControl,
             metric = "Accuracy",
             data = validation_data)
# Mostrar los resultados del modelo
fit


#Validacion de mas variables
# Determine the optimal K
fit_all_attributes <- train(type1 ~ speed + attack + defense + hp + sp_attack + sp_defense,
                               method = "knn",
                               tuneGrid = expand.grid(k = 1:5),
                               trControl = trControl,
                               metric = "Accuracy",
                               data = validation_data)
fit_all_attributes

# Perform the KNN
training_data_all <- training_data %>%
   select(c(speed,attack, defense, hp, sp_attack, sp_defense))
testing_data_all <- testing_data %>%
   select(c(speed,attack, defense, hp, sp_attack, sp_defense))
knn_all <- knn(train = training_data_all, test = testing_data_all, cl = training_data$type1, k = 4)

confusionMatrix(knn_all,testing_data$type1 )









