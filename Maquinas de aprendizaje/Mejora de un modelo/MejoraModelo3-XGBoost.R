library(dplyr)
library(caret)
library(Rtsne)
library(xgboost)
library(DiagrammeR)
library(pROC) # Curva ROC
library(vcd) # Kappa
library(mltools) # Coeficiente de correlacion de Mattews

# 8 variables numericas
carac_numeric <- c("person_age","person_income","person_emp_exp","loan_amnt","loan_int_rate",
                   "loan_percent_income","cb_person_cred_hist_length","credit_score")
# 6 categoricas
carac_categoric <- c("person_gender","person_education","person_home_ownership","loan_intent",
                     "previous_loan_defaults_on_file","loan_status")

# Carga de datos 
df_loan_xgb <- read.csv("loan_data_clean.csv", stringsAsFactors = TRUE)
df_loan_xgb$loan_status <- as.factor(df_loan_xgb$loan_status)
str(df_loan_xgb)

# Observemos las clases de cada variable categorica
for (col in carac_categoric) {
  cat("\n--- Clases de la variable: ", col, "---\n")
  print(levels(df_loan_xgb[[col]]))
}

head(df_loan_xgb)
#==========================================================================
#                         Gráfica tSNE
#==========================================================================

# Convertimos a numerico las categoricas
df_loan_xgb_t_tsne <- df_loan_xgb %>% mutate(across(carac_categoric, ~ as.numeric(.)) - 1)
df_loan_xgb_t_tsne$loan_status <- as.factor(df_loan_xgb_t_tsne$loan_status)

loan_status <- df_loan_xgb_t_tsne$loan_status
df_loan_xgb_t_tsne$loan_status <- NULL

# Gráfica tSNE
tsne = Rtsne(as.matrix(df_loan_xgb_t_tsne), check_duplicates=FALSE, pca=TRUE, perplexity=30, theta=0.5, dims=2)
embedding = as.data.frame(tsne$Y)
embedding$Class = loan_status
g = ggplot(embedding, aes(x=V1, y=V2, color=Class)) +
  geom_point(size=1.25) +
  guides(colour=guide_legend(override.aes=list(size=6))) +
  xlab("") + 
  ylab("") + 
  ggtitle("t-SNE 2D Embedding of loan_status") + 
  theme_light(base_size=20) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())
print(g)

#==========================================================================
#                   DIVISION DE PRUEBA Y ENTRENAMIENTO
#==========================================================================
set.seed(762) # Con la misma semilla que el otro modelo

train_xgb <- stratified(df_loan_xgb, group="loan_status",size=0.8,replace=FALSE)
test_xgb <- df_loan_xgb[!(rownames(df_loan_xgb) %in% rownames(train_xgb)), ]

# Conversion de valores para XGBoost
train_xgb_t <- train_xgb %>% mutate(across(carac_categoric, ~ as.numeric(.)) - 1)
test_xgb_t <- test_xgb %>% mutate(across(carac_categoric, ~ as.numeric(.)) - 1)
loan_status_train <- as.factor(train_xgb_t$loan_status)
loan_status_test <- as.factor(test_xgb_t$loan_status)
train_xgb_t$loan_status <- NULL
test_xgb_t$loan_status <- NULL

# Finalmente convertimos a valores matriciales para poder evaluar el modelo
train_xgb.matrix = as.matrix(train_xgb_t)
mode(train_xgb.matrix) = "numeric"
test_xgb.matrix = as.matrix(test_xgb_t)
mode(test_xgb.matrix) = "numeric"
y = as.matrix(as.integer(loan_status_train))

#==========================================================================
#                           MODELO XGBOOST
#==========================================================================

# Convertimos la matriz a data frame
train_data <- as.data.frame(train_xgb.matrix)

# Convertimos y a factor con dos niveles para clasificación binaria
train_data$label <- factor(y, labels = c("no", "yes"))

test_data <- as.data.frame(test_xgb.matrix)

# Control para validacion cruzada
control <- trainControl(
  method = "cv",                # validación cruzada
  number = 10,                  # número de pliegues
  verboseIter = TRUE,          # muestra el progreso
  allowParallel = TRUE,        # permite múltiples núcleos
  classProbs = TRUE,           # para obtener probabilidades
  summaryFunction = twoClassSummary # usa métricas como ROC, Sens, Spec
)

# Definimos la rejilla
xgb_grid <- expand.grid(
  nrounds = c(50, 100),
  max_depth = c(8, 16),
  eta = c(0.01, 0.1),
  gamma = c(0),
  colsample_bytree = c(0.8),
  min_child_weight = c(1, 6),
  subsample = c(0.8)
)

# Entrenamiento del modelo
set.seed(9746)
xgb_caret <- train(
  label ~ .,
  data = train_data,
  method = "xgbTree",
  trControl = control,
  tuneGrid = xgb_grid,
  metric = "ROC" 
)

# Mejores parametros
xgb_caret$bestTune

# Parte del arbol y reglas
trees_text <- xgb.dump(model, with_stats = TRUE)
head(trees_text, 20)

# Grafico de importancia de variables
model <- xgb_caret$finalModel
names <- dimnames(train_xgb.matrix)[[2]]
importance_matrix <- xgb.importance(feature_names = names, model = model)
gp <- xgb.plot.importance(importance_matrix)
print(gp)

# Hacemos las predicciones
pred_class <- predict(xgb_caret, newdata = test_data)
pred_class_numeric <- as.integer(pred_class) - 1

confusionMatrix(factor(loan_status_test), factor(pred_class_numeric), positive = "1")

# Metricas de evaluacion
kappa_xgb <- Kappa(table(loan_status_test,factor(pred_class_numeric)))
mcc_xgb <- mcc(loan_status_test,factor(pred_class_numeric))
sensitivity_xgb <- sensitivity(factor(pred_class_numeric), loan_status_test, positive = "1")
specificity_xgb <- specificity(factor(pred_class_numeric), loan_status_test,negative = "0")
precision_xgb <- posPredValue(factor(pred_class_numeric), loan_status_test,positive = "1")
f1_score_xgb <- 2 * (precision_xgb * sensitivity_xgb) / (precision_xgb + sensitivity_xgb)
conf_matrix_xgb_t <- table(loan_status_test, factor(pred_class_numeric))
acurracy_xgb <- sum(diag(conf_matrix_xgb_t)) / sum(conf_matrix_xgb_t)

kappa_xgb
mcc_xgb
sensitivity_xgb
specificity_xgb
precision_xgb
f1_score_xgb
acurracy_xgb

# Curva ROC
par(mfrow = c(1, 1)) 
predicciones_prob_xgb <- predict(xgb_caret, newdata = test_data, type = "prob")[, 2]
roc_curve_xgb <- roc(loan_status_test, predicciones_prob_xgb,levels = c("0", "1"))
plot(roc_curve_xgb, main = "Curva ROC para XGBoost", col = "blue")
auc(roc_curve_xgb)

# Ploteo de las curvas ROC
plot(roc_curve_xgb, main = "Curvas ROC", col = "blue")  # XGBoost
plot(roc_curve, col = "red", add = TRUE)                # Random Forest
plot(roc_curve_rpart, col = "green", add = TRUE)         # rpart

# Añadir leyenda
legend("bottomright", 
       legend = c("XGBoost", "Random Forest", "rpart"),
       col = c("blue", "red", "green"),
       lwd = 2)

