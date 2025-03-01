library(kernlab)
#Leemos el dataset con las caracteriticas de los glifos
letters <- read.csv("letterdata.csv", stringsAsFactors = TRUE)
str(letters)

#Contruccion de los conjuntos de entrenamiento y prueba
letters_train <- letters[1:16000, ]
letters_test <- letters[16001:20000, ]

#Entrenamiento del modelo
letter_classifier <- ksvm(letter ~ ., data = letters_train,kernel = "vanilladot")
letter_classifier

#Realizamos algunas predicciones
letter_predictions <- predict(letter_classifier, letters_test)
head(letter_predictions)

#Visualizar predicciones
table(letter_predictions, letters_test$letter)

#De manera mas resumida vemos la con valores booleanos
agreement <- letter_predictions == letters_test$letter
table(agreement)
prop.table(table(agreement))

# Mejora con kernel gaussiano
RNGversion("3.5.2")
set.seed(12345)
letter_classifier_rbf <- ksvm(letter ~ ., data = letters_train, kernel = "rbfdot")
letter_predictions_rbf <- predict(letter_classifier_rbf, letters_test)
# Comparamos la precisión con nuestro SVM lineal:
agreement_rbf <- letter_predictions_rbf == letters_test$letter
table(agreement_rbf)
prop.table(table(agreement_rbf))


#Busqueda del valor optimo de C
cost_values <- c(1, seq(from = 5, to = 40, by = 5))
RNGversion("3.5.2")
accuracy_values <- sapply(cost_values, function(x) {
  set.seed(12345) 
  m <- ksvm(letter ~ ., data = letters_train,
            kernel = "rbfdot", C = x)
  pred <- predict(m, letters_test)
  agree <- ifelse(pred == letters_test$letter, 1, 0)
  accuracy <- sum(agree) / nrow(letters_test) 
  return (accuracy) 
  })
plot(cost_values, accuracy_values, type = "b")



