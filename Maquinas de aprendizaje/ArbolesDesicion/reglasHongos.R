#Cargamos los datos como factores dado que sabemos que todas son nominales
mushrooms <- read.csv("mushrooms.csv", stringsAsFactors = TRUE)
str(mushrooms)

View(mushrooms)
#Podeos ver que la variable veil_color contiene una sola clase. Ya que esto
# provocaria inconsistencias, ademas que no resulta relevante para el analisis
# la eliminamos
mushrooms$veil_type <- NULL
table(mushrooms$type)
#En este caso usaremos el algoritmo 1R
#install.packages("OneR")
#Definimos type como objetivo y consideraremos todas las caracteristicas
# restantes del dataframe

library(OneR)
mushroom_1R <- OneR(type ~ ., data = mushrooms)
mushroom_1R
#En general la regla  generada fue:
# "Si el hongo huele apetitoso entonces es comestible"

mushroom_1R_pred <- predict(mushroom_1R, mushrooms)
table(actual = mushrooms$type, predicted = mushroom_1R_pred)



#install.packages("RWeka") 

library(RWeka)
library(caret)

#Hacemos el entrenamiento ahora con RIPPER
mushroom_JRip <- JRip(type ~ ., data = mushrooms)
mushroom_JRip

predictions <- predict(mushroom_JRip, newdata = mushrooms)
conf_matrix <- confusionMatrix(predictions, mushrooms$type)
print(conf_matrix)
