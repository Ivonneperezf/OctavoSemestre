library(dplyr)
library(gmodels)
library(caret)
library(vcd)
library(irr)
library(mltools)
library(pROC)

# EVALUACION DEL MODELO NAIVE BAYES DE SMS
sms_results <- read.csv("sms_results.csv")
# String as factor
sms_results <- sms_results |>
  mutate(across(where(is.character), as.factor))

head(sms_results)
# Probabilidades no tan seguras
head(subset(sms_results, prob_spam > 0.40 & prob_spam < 0.60))
# Casos erroneos
head(subset(sms_results, actual_type != predict_type))

# Matriz de confusion
table(sms_results$actual_type, sms_results$predict_type)
CrossTable(sms_results$actual_type, sms_results$predict_type)

# Matriz de confusion con carret
confusionMatrix(sms_results$predict_type,sms_results$actual_type, positive = "spam")

# Funcion para calcular kappa directamente desde R
Kappa(table(sms_results$actual_type, sms_results$predict_type))

# Funcion para calcular kappa  a partir de vectores de valores previstos y reales 
# almacenados en un frame de datos
kappa2(sms_results[1:2])

# Coeficiente de Matthews
mcc(sms_results$actual_type, sms_results$predict_type)
# Obtenido para una variable binaria con valores de correlacion
cor(ifelse(sms_results$actual_type == "spam", 1, 0),ifelse(sms_results$predict_type == "spam", 1, 0))

# Sensibilidad y especificidad
sensitivity(sms_results$predict_type, sms_results$actual_type,positive = "spam")
specificity(sms_results$predict_type, sms_results$actual_type,negative = "ham")

# Precision y Recuperacion
posPredValue(sms_results$predict_type, sms_results$actual_type,positive = "spam")
sensitivity(sms_results$predict_type, sms_results$actual_type,positive = "spam")

# Creacion de curvas ROC
sms_roc <- roc(sms_results$actual_type, sms_results$prob_spam)
plot(sms_roc, main = "ROC curve for SMS spam filter",col = "blue", lwd = 2, legacy.axes = TRUE)

sms_results_knn <- read.csv("sms_results_knn.csv")
sms_roc_knn <- roc(sms_results$actual_type, sms_results_knn$p_spam)
plot(sms_roc_knn, col = "red", lwd = 2, add = TRUE)
auc(sms_roc)
auc(sms_roc_knn)

# Metodo de retencion
credit <-read.csv("credit.csv")




