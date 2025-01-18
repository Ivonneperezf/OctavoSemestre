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
View(combined_training_test)                                                                               nrow(testing_data))))





