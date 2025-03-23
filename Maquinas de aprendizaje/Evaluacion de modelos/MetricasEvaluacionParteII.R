library(caret)
library(C50)
library(irr)

# Ejemplo para divisiones
credit <- read.csv("credit.csv",stringsAsFactors = TRUE)
# Indexes aleatorios en orden
random_ids <- order(runif(1000))

# Conjuntos de entrenamiento,prueba y validacion
credit_train <- credit[random_ids[1:500], ]
credit_validate <- credit[random_ids[501:750], ]
credit_test <- credit[random_ids[751:1000], ]

# Muestreo aleatorio estratificado
in_train <- createDataPartition(credit$default, p = 0.75, list = FALSE)
credit_train <- credit[in_train, ]
credit_test <- credit[-in_train, ]

# Validacion cruzada
set.seed(123)
folds <- createFolds(credit$default, k = 10)
# Los folds son indexes 
str(folds)
# Creacion de prueba y entrenamiento con el primer fold
credit01_test <- credit[folds$Fold01, ]
credit01_train <- credit[-folds$Fold01, ]

# Evaluacion de k folds usando C50
set.seed(123)
folds <- createFolds(credit$default, k = 10)

# Funcion para devolver kappa mandando vectores de index para el entrenamiento
# esto aplicado a todos los vectores de cada fold
cv_results <- lapply(folds, function(x) {
  credit_train <- credit[-x, ]
  credit_test <- credit[x, ]
  credit_model <- C5.0(default ~ ., data = credit_train)
  credit_pred <- predict(credit_model, credit_test)
  credit_actual <- credit_test$default
  kappa <- kappa2(data.frame(credit_actual, credit_pred))$value
  return(kappa)
  })

str(cv_results)
mean(unlist(cv_results))
sd(unlist(cv_results))











