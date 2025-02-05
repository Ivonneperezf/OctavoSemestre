library(C50)
library(gmodels)
#Dado que ya son factores, los importamos como factores
credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
str(credit)

#Table de las variables que podrian influir en el prestamo
table(credit$checking_balance)
table(credit$savings_balance)
#Para variables numericas
summary(credit$months_loan_duration)
summary(credit$amount)

#Para ver pagos o impagos 
table(credit$default)

#Hacemos la division del conjunto de entrenamiento y preuba 90-10 de manera
#aleatoria
set.seed(9829)
train_sample <- sample(1000, 900)
str(train_sample)

credit_train <- credit[train_sample, ]
credit_test <- credit[-train_sample, ]
#Proporciones del conjunto de netrenamiento y prueba respecto a la cantidad de
#prestamos

prop.table(table(credit_train$default))
str(credit_train)
prop.table(table(credit_test$default))
str(credit_test)

#Ahora implementaremos el entrenamiento del arbol, donde tomaremos los valores por
#default de los arboles C5.0

credit_model <- C5.0(default ~ ., data = credit_train)
credit_model
summary(credit_model)

#Evaluacion de predictores segun el modelo
credit_pred <- predict(credit_model, credit_test)
CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, 
           dnn = c('actual default', 'predicted default'))


#Implementamos el boosting al modelo del arbol
credit_boost10 <- C5.0(default ~ ., data = credit_train, trials = 10)
credit_boost10
summary(credit_boost10)

credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))


#Matriz de penalizacion
matrix_dimensions <- list(c("no", "yes"), c("no", "yes"))
names(matrix_dimensions) <- c("predicted", "actual")
matrix_dimensions
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2,dimnames = matrix_dimensions)
error_cost

#Evaluamos ahora con matriz de confusion
credit_cost <- C5.0(default ~ ., data = credit_train,
                    costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)
CrossTable(credit_test$default, credit_cost_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))


