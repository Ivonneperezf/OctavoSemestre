library(gbm)
library(vcd)
library(caret)
library(xgboost)
library(Matrix)
# Evaluacion de boosting con descenso del gradiente GBM
credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
credit$default <- ifelse(credit$default == "yes", 1, 0)
set.seed(123)
train_sample <- sample(1000, 900)
credit_train <- credit[train_sample, ]
credit_test <- credit[-train_sample, ]
set.seed(300)
m_gbm <- gbm(default ~ ., data = credit_train)
m_gbm

# Hacemos predicciones con el modelo
p_gbm <- predict(m_gbm, credit_test, type = "response")

# Dado que las predicciones son probabilidades, debemos convertirlas a binario
# Si la probabilidad de impago del préstamo es superior al 50 %, predeciremos impago; de lo 
# contrario, predeciremos no impago
p_gbm
p_gbm_c <- ifelse(p_gbm > 0.50, 1, 0)
table(credit_test$default, p_gbm_c)
# Estadistica Kappa para revisar el desempeño
Kappa(table(credit_test$default, p_gbm_c))

# Vamos a buscar el mejor modelo buscando con rejilla
grid_gbm <- expand.grid(n.trees = c(100, 150, 200),interaction.depth = c(1, 2, 3),shrinkage = c(0.01, 0.1, 0.3),
                        n.minobsinnode = 10)

# Usando trainControl ajustamos algunos parametros de control
ctrl <- trainControl(method = "cv", number = 10,selectionFunction = "best")

# Evaluacion usando la gbm y kappa como metrica de evaluacion
credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
set.seed(300)
m_gbm_c <- train(default ~ ., data = credit, method = "gbm",trControl = ctrl, tuneGrid = grid_gbm,
                 metric = "Kappa",verbose = FALSE)
m_gbm_c

# XGBoost
credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
credit_matrix <- sparse.model.matrix(~ . -default, data = credit)
dim(credit_matrix)
print(credit_matrix[1:5, 1:15])

# Dado que no estamos construyendo un modelo de regresión  1 
# es inútil para este análisis y puede eliminarse de la matriz
credit_matrix <- credit_matrix[, -1]

# división 90-10
set.seed(12345)
train_ids <- sample(1000, 900)
credit_train <- credit_matrix[train_ids, ]
credit_test <- credit_matrix[-train_ids, ]

dim(credit_train)
dim(credit_test)

# Pasamos las variables a binarias
credit_train_labels <- ifelse(credit[train_ids, c("default")] == "yes", 1, 0)
credit_test_labels <- ifelse(credit[-train_ids, c("default")] == "yes", 1, 0)

# Restableceremos los valores predeterminados
params.xgb <- list(objective = "binary:logistic",max_depth = 6,eta = 0.3,gamma = 0,
                   colsample_bytree = 1,min_child_weight = 1,subsample = 1)
# Una vez definidos los parametros, implementamos el modelo con 100 iteraciones
# esto dada la evidencia empirica
set.seed(555)
xgb_credit <- xgboost(params = params.xgb,data = credit_train,label = credit_train_labels,
                      nrounds = 100,verbose = 1,print_every_n = 10)
# Hacemos las predicciones para evaluar el rendimiento del modelo
prob_default <- predict(xgb_credit, credit_test)
# Dado que se da por probabilidades, de igual manera codificamos dependiendo de los decimales
pred_default <- ifelse(prob_default > 0.50, 1, 0)
# Presicion del 76%
table(pred_default, credit_test_labels)
# El estadistico kappa nos dio un resultado un poco menor
Kappa(table(pred_default, credit_test_labels))

# Una vez ya conocido un desmpeño menor con los parametros predeterminados, hacemos la busqueda 
# usando una rejilla
grid_xgb <- expand.grid(eta = c(0.3, 0.4),max_depth = c(1, 2, 3),colsample_bytree = c(0.6, 0.8),
                        subsample = c(0.50, 0.75, 1.00),nrounds = c(50, 100, 150),gamma = c(0, 1),
                        min_child_weight = 1)
# Evaluacion con rejilla y CV 10 folds
ctrl <- trainControl(method = "cv", number = 10,selectionFunction = "best")
credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
set.seed(300)
m_xgb <- train(default ~ ., data = credit, method = "xgbTree",trControl = ctrl, tuneGrid = grid_xgb,
               metric = "Kappa", verbosity = 0)
m_xgb$bestTune
# Valor kappa para el mejor modelo
max(m_xgb$results["Kappa"])
