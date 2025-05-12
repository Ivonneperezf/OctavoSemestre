library(caret)
# Verificar los parametros de un modelo
modelLookup("C5.0")
credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
set.seed(300)
# Buscamos el mejor modelo
m <- train(default ~ ., data = credit, method = "C5.0")
m
# Usamos el mejor modleo para hacer predicciones
p <- predict(m, credit)
table(p, credit$default)

head(predict(m, credit))

# Con probabilidades
head(predict(m, credit, type = "prob"))


# IMplementacion de train con parametros personalizados
ctrl <- trainControl(method = "cv", number = 10,selectionFunction = "oneSE")

# Implementacion con rejilla en un parametro
grid <- expand.grid(model = "tree",trials = c(1, 5, 10, 15, 20, 25, 30, 35),winnow = FALSE)
grid

# Implementacion que usa la metrica Kappa para defirnir el mejor modelo
set.seed(300)
m <- train(default ~ ., data = credit, method = "C5.0",metric = "Kappa",trControl = ctrl,tuneGrid = grid)


# Uso de algoritmos de conuntos
#  bagged
library(ipred)
credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
set.seed(123)
mybag <- bagging(default ~ ., data = credit, nbagg = 25)

credit_pred <- predict(mybag, credit)
table(credit_pred, credit$default)

# IMplementacion con CV-10folds
library(caret)
credit <- read.csv("credit.csv")
set.seed(300)
ctrl <- trainControl(method = "cv", number = 10)
train(default ~ ., data = credit, method = "treebag",trControl = ctrl)

# Boosting
library(adabag)
credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
set.seed(300)
m_adaboost <- boosting(default ~ ., data = credit)
p_adaboost <- predict(m_adaboost, credit)
head(p_adaboost$class)
p_adaboost$confusion
# Evaluacion con CV - 10 folds
set.seed(300)
adaboost_cv <- boosting.cv(default ~ ., data = credit)
adaboost_cv$confusion
# kappa de boosting
library(vcd)
Kappa(adaboost_cv$confusion)

# Random forest
library(randomForest)
set.seed(300)
rf <- randomForest(default ~ ., data = credit)
rf
# Calcular kappa
library(vcd)
Kappa(rf$confusion[1:2,1:2])

# Evaluacion con ranger
library(ranger)
set.seed(300)
m_ranger <- ranger(default ~ ., data = credit)
m_ranger
Kappa(m_ranger$confusion.matrix)
