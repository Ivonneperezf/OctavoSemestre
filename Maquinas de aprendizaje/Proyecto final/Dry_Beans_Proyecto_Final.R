library(ggplot2)
library(tidyr)
library(GGally)
library(corrplot)
library(scales)
library(caret)
library(themis)
library(smotefamily)
library(dplyr)
library(tidyverse)
library(irlba)
library(caret)
library(splitstackshape)
library(rsample)
library(nnet)
library(boot)
library(rpart)
library(e1071)
library(randomForest)
library(xgboost)
library(pROC) 
library(vcd)
library(mltools)
library(nnet)

beans <- read.csv("Dry_Bean_Dataset.csv", stringsAsFactors = TRUE)
str(beans)
summary(beans)
colSums(is.na(beans))
beans$Class <- as.factor(beans$Class)

carac_numeric <- names(beans)[sapply(beans, is.numeric)]
carac_numeric

# Histogramas
hist_carac <- function(name_col) {
  hist(beans[[name_col]],main = paste("Histograma de", name_col),xlab = name_col, 
       col = "skyblue",border = "white",breaks = 80) 
  abline(v = mean(beans[[name_col]], na.rm = TRUE), col = "red", lwd = 2)
}


par(mfrow = c(2, 2))
for (i in 1:4) {
  hist_carac(carac_numeric[i])
}

par(mfrow = c(2, 2))
for (i in 5:8) {
  hist_carac(carac_numeric[i])
}

par(mfrow = c(2, 2))
for (i in 9:12) {
  hist_carac(carac_numeric[i])
}

par(mfrow = c(2, 2))
for (i in 12:15) {
  hist_carac(carac_numeric[i])
}

par(mfrow = c(1,1))
for (i in 16) {
  hist_carac(carac_numeric[i])
}

# Boxplot
boxplot_carac <- function(data_name){
  boxplot(beans[[data_name]], main = paste("Boxplot de", data_name), ylab = data_name, col = "skyblue")
}

par(mfrow = c(2,3))
for (i in 1:6) {
  boxplot_carac(carac_numeric[i])
}

par(mfrow = c(2,3))
for (i in 7:12) {
  boxplot_carac(carac_numeric[i])
}

par(mfrow = c(2,2))
for (i in 13:16) {
  boxplot_carac(carac_numeric[i])
}

# GRAFICOS DE VIOLIN
beans_long <- beans %>%
  pivot_longer(cols = -Class, names_to = "variable", values_to = "value")
ggplot(beans_long, aes(x = Class, y = value)) +
  geom_violin(aes(fill = Class), trim = FALSE) +
  facet_wrap(~variable, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  labs(y = "Feature Value", x = "Class")

# Matriz de correlacion
par(mfrow = c(1,1))
corrplot(cor(beans[, sapply(beans, is.numeric)], use = "complete.obs"), 
         method = "circle",addCoef.col = "white", tl.cex = 0.8,number.cex = 0.7)

# Diagramas de dispersion
ggpairs(beans[, sapply(beans, is.numeric)], 
        title = "Matriz de Diagramas de Dispersión")

# Grafica de pastel
beans_pas <- as.data.frame(prop.table(table(beans$Class)))
colnames(beans_pas) <- c("categorias", "porcentaje")
beans_pas$porcentaje <- beans_pas$porcentaje * 100 

ggplot(beans_pas, aes(x = "", y = porcentaje, fill = categorias)) +
  geom_bar(stat = "identity", color = "white") +
  geom_text(aes(label = percent(porcentaje / 100)),
            position = position_stack(vjust = 0.5),
            color = "white", size = 5) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("salmon", "steelblue", "orange", "gray", "purple", "darkolivegreen", "brown")) +
  theme_void() +
  labs(title = "Distribución proporcional de clases (Dry Beans)")

prop.table(table(beans$Class))

#======================================================
# SMOTE
#======================================================

# Funcion para normalizar usando ejemplos sinteticos
# Normalizacion Min-Max
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Funcion para desnormalizar
unnormalize <- function(norm_vals, col_name) {
  old_vals <- beans[[col_name]]
  min_val <- min(old_vals, na.rm = TRUE)
  max_val <- max(old_vals, na.rm = TRUE)
  unnormalized_vals <- norm_vals * (max_val - min_val) + min_val
  if (is.integer(old_vals)) {
    rounded_vals <- round(unnormalized_vals)
  } else {
    rounded_vals <- unnormalized_vals
  }
  return(rounded_vals)
}

# Normalizar solo las columnas numéricas del dataframe beans
beans_normalized <- beans
beans_normalized[sapply(beans, is.numeric)] <- lapply(beans[sapply(beans, is.numeric)], normalize)
summary(beans_normalized)

beans_balanced <- beans_normalized |> smote("Class", over_ratio = 0.55)
table(beans_normalized$Class)
table(beans_balanced$Class)

prop.table(table(beans_normalized$Class))
prop.table(table(beans_balanced$Class))

# Quitaremos algunos regitsros de la clase Dermason
other_classes <- beans_balanced %>% filter(Class != "DERMASON")
dermason_subset <- beans_balanced %>%
  filter(Class == "DERMASON") %>%
  sample_frac(0.8) 
beans_reduced <- bind_rows(other_classes, dermason_subset)

prop.table(table(beans_normalized$Class))
prop.table(table(beans_reduced$Class))

# Grafica nueva de Pastel
beans_pas_reduced <- as.data.frame(prop.table(table(beans_reduced$Class)))
colnames(beans_pas_reduced) <- c("categorias", "porcentaje")
beans_pas_reduced$porcentaje <- beans_pas_reduced$porcentaje * 100 

ggplot(beans_pas_reduced, aes(x = "", y = porcentaje, fill = categorias)) +
  geom_bar(stat = "identity", color = "white") +
  geom_text(aes(label = percent(porcentaje / 100)),
            position = position_stack(vjust = 0.5),
            color = "white", size = 5) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("salmon", "steelblue", "orange", "gray", "purple", "darkolivegreen", "brown")) +
  theme_void() +
  labs(title = "Distribución proporcional de clases (Dry Beans)")

# Desnormalizamos los datos
num_cols <- sapply(beans_reduced, is.numeric)
beans_reduced[num_cols] <- lapply(names(beans_reduced)[num_cols], function(col_name) {
  unnormalize(beans_reduced[[col_name]], col_name)
})
summary(beans_reduced)

# Hacemos una exploracion basica

# Histogramas
hist_carac_reduced <- function(name_col) {
  hist(beans_reduced[[name_col]],main = paste("Histograma de", name_col),xlab = name_col, 
       col = "skyblue",border = "white",breaks = 80) 
  abline(v = mean(beans_reduced[[name_col]], na.rm = TRUE), col = "red", lwd = 2)
}


par(mfrow = c(2, 2))
for (i in 1:4) {
  hist_carac_reduced(carac_numeric[i])
}

par(mfrow = c(2, 2))
for (i in 5:8) {
  hist_carac_reduced(carac_numeric[i])
}

par(mfrow = c(2, 2))
for (i in 9:12) {
  hist_carac_reduced(carac_numeric[i])
}

par(mfrow = c(2, 2))
for (i in 12:15) {
  hist_carac_reduced(carac_numeric[i])
}

par(mfrow = c(1,1))
for (i in 16) {
  hist_carac_reduced(carac_numeric[i])
}

length(beans$Class)
length(beans_reduced$Class)

# GRAFICOS DE VIOLIN
beans_long_reduced <- beans_reduced %>%
  pivot_longer(cols = -Class, names_to = "variable", values_to = "value")
ggplot(beans_long_reduced, aes(x = Class, y = value)) +
  geom_violin(aes(fill = Class), trim = FALSE) +
  facet_wrap(~variable, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  labs(y = "Feature Value", x = "Class")

# Guardamos el nuevo dataframe en un CSV 
write.csv(beans_reduced, "Dry_Bean_Dataset_balanced.csv", row.names = FALSE)
# Diagramas de dispersion y correlaciones
ggpairs(beans_reduced[, sapply(beans, is.numeric)], 
        title = "Matriz de Diagramas de Dispersión y Correlaciones")

par(mfrow = c(1,1))
corrplot(cor(beans_reduced[, sapply(beans_reduced, is.numeric)], use = "complete.obs"), 
         method = "circle",addCoef.col = "white", tl.cex = 0.8,number.cex = 0.7)

#=============================================================
# PCA
#=============================================================
# Usaremos PCA
# Para iniciar y asegurarnos de usar los datos correctos, cargamos nuevamente los datos
df_beans <- read_csv("Dry_Bean_Dataset_balanced.csv")
str(df_beans)
View(df_beans)
beans_terms <- df_beans |> select(Area:ShapeFactor4)
# Aplicamos PCA
set.seed(4655)
beans_pca <- beans_terms |>
  prcomp_irlba(n = 7, center = TRUE, scale = TRUE)
# Grafico para escoger ls componentes optimos segun la varianza
screeplot(beans_pca, npcs = 7, type = "lines",
          main = "Scree Plot of Dry Beans Data Principal Components")
# Resumen de los datos
summary(beans_pca)
str(beans_pca$x)
head(beans_pca$x)

# Vamos a visualizar la importancia de los componentes
beans_pca_long <- tibble(Beans_Term = colnames(beans_terms),
                       as_tibble(beans_pca$rotation)) |> 
  pivot_longer(PC1:PC5, names_to = "PC", values_to = "Contribution")

# Fucnion para visualizar componentes
plot_pca <- function(component) {
  beans_pca_long |>
    filter(PC == component) |>
    top_n(15, abs(Contribution)) |>
    mutate(Beans_Term = reorder(Beans_Term, Contribution)) |> 
    ggplot(aes(Beans_Term, Contribution, fill = Beans_Term)) +
    geom_col(show.legend = FALSE, alpha = 0.8) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
          axis.ticks.x = element_blank()) +
    labs(x = "Terminos Dry Beans",
         y = "Relative Importance to Principal Component",
         title = paste("Top 16 Contributors to", component))
}

plot_pca("PC1")
plot_pca("PC2")
plot_pca("PC3")
plot_pca("PC4")
plot_pca("PC5")

# Unimos con los datos de PCA

boxplot_carac_PCA <- function(data_name){
  boxplot(beans_data_pca[[data_name]], main = paste("Boxplot de", data_name), ylab = data_name, col = "skyblue")
}

beans_data_pca <- data.frame(
  beans_pca$x[, 1:5],        
  Class = df_beans$Class     
)
carac_numeric_PCA <- names(beans_data_pca)[sapply(beans_data_pca, is.numeric)]
carac_numeric_PCA

par(mfrow = c(2,3))
for (i in 1:5) {
  boxplot_carac_PCA(carac_numeric_PCA[i])
}

# Histogramas
hist_carac_PCA <- function(name_col) {
  hist(beans_data_pca[[name_col]],main = paste("Histograma de", name_col),xlab = name_col, 
       col = "skyblue",border = "white",breaks = 80) 
  abline(v = mean(beans_data_pca[[name_col]], na.rm = TRUE), col = "red", lwd = 2)
}

par(mfrow = c(2, 2))
for (i in 1:4) {
  hist_carac_PCA(carac_numeric_PCA[i])
}
par(mfrow = c(1, 1))
hist_carac_PCA(carac_numeric_PCA[5])

# Obtener el índice del valor mínimo en PC3
idx_min_pc3 <- which.min(beans_data_pca$PC3)
# Eliminar esa fila
beans_data_pca <- beans_data_pca[-idx_min_pc3, ]
beans_data_pca$Class <- as.factor(beans_data_pca$Class)

summary(beans_data_pca)
str(beans_data_pca)
write.csv(beans_data_pca, "Dry_Bean_Dataset_CleanData_PCA.csv", row.names = FALSE)

# Visualizaremos las correlaciones de la reduccion ya hecha por componentes
ggpairs(beans_data_pca[, sapply(beans_data_pca, is.numeric)], 
        title = "Matriz de Diagramas de Dispersión y Correlaciones")

par(mfrow = c(1,1))
corrplot(cor(beans_data_pca[, sapply(beans_data_pca, is.numeric)], use = "complete.obs"), 
         method = "circle",addCoef.col = "white", tl.cex = 0.8,number.cex = 0.7)

#======================================================
# SEPARACION DE ENTRENAMIENTO Y PRUEBA
#======================================================
df_data_beans <- read.csv("Dry_Bean_Dataset_CleanData_PCA.csv", stringsAsFactors = TRUE)
str(df_data_beans)

set.seed(8534)
# Dividiremos en proporcion 80-20
split <- initial_split(df_data_beans, prop = 0.8, strata = Class)

# Obtener conjuntos
train_df_data_beans <- training(split)
test_df_data_beans  <- testing(split)

# Tabla de proporciones
tabla_proporciones <- data.frame(
  Clase = levels(df_data_beans$Class),
  Total     = as.numeric(table(df_data_beans$Class)),
  Train     = as.numeric(table(train_df_data_beans$Class)),
  Test      = as.numeric(table(test_df_data_beans$Class))
)
tabla_proporciones$Prop_Total <- round(tabla_proporciones$Total / nrow(df_data_beans), 3)
tabla_proporciones$Prop_Train <- round(tabla_proporciones$Train / nrow(train_df_data_beans), 3)
tabla_proporciones$Prop_Test  <- round(tabla_proporciones$Test / nrow(test_df_data_beans), 3)
tabla_proporciones

# Verificamos las distribuciones de las variables
for (col in carac_numeric_PCA){
  cat("\n--- KS Test para:", col, "---\n")
  print(ks.test(df_data_beans[[col]], train_df_data_beans[[col]], alternative = "two.sided"))
}

comparar_hist_train <- function(name_data){
  par(mfrow =c(1,2))
  hist(df_data_beans[[name_data]], breaks=50, col="red", xlab=name_data,main="Original")
  hist(train_df_data_beans[[name_data]], breaks=50, col="green",xlab=name_data, main="Stratified Sample")
}
comparar_hist_train(carac_numeric_PCA[1])
comparar_hist_train(carac_numeric_PCA[2])
comparar_hist_train(carac_numeric_PCA[3])
comparar_hist_train(carac_numeric_PCA[4])
comparar_hist_train(carac_numeric_PCA[5])

#==============================================================
# ENTRENAMIENTOS DE MODELOS BASICOS
#==============================================================
# Implemntacio sin bootstrap
rl_multinom <- multinom(
  Class ~ PC1 + PC2 + PC3 + PC4 + PC5,
  data = train_df_data_beans,
  maxit = 300,
  trace = TRUE,
  decay = 1e-4
)
pred_rl_multinom <- predict(rl_multinom, newdata = test_df_data_beans)
confusionMatrix(pred_rl_multinom, test_df_data_beans$Class)

# Implemntacion con bootstrap
boot_accuracy <- function(data, indices) {
  # Crear subconjunto bootstrap del train
  boot_data <- data[indices, ]
  # Entrenar modelo multinomial
  modelo <- multinom(Class ~ PC1 + PC2 + PC3 + PC4 + PC5,
                     data = boot_data,
                     maxit = 300,
                     trace = FALSE,
                     decay = 1e-4)
  # Predecir sobre el test fijo
  pred <- predict(modelo, newdata = test_df_data_beans)
  # Calcular accuracy (porcentaje de aciertos)
  acc <- mean(pred == test_df_data_beans$Class)
  return(acc)
}

# Ejecutar bootstrap no paramétrico
set.seed(8835)  
resultado_boot <- boot(data = train_df_data_beans, 
                       statistic = boot_accuracy,
                       R = 100)  
resultado_boot

# Intervalo de confianza del 95% para la accuracy
boot.ci(resultado_boot, type = "perc")

########################################################


# Con validacion cruzada
ctrl <- trainControl(method = "cv", number = 10)  # 10-fold CV
modelo_cv <- train(
  Class ~ PC1 + PC2 + PC3 + PC4 + PC5,
  data = train_df_data_beans,
  method = "multinom",
  trControl = ctrl,
  trace = FALSE,
  tuneGrid = expand.grid(decay = c(1e-4, 1e-3, 1e-2))
)
pred_rl_vc_multinom <- predict(modelo_cv, newdata = test_df_data_beans)
confusionMatrix(pred_rl_vc_multinom, test_df_data_beans$Class)

boot_accuracy_cv <- function(data, indices) {
  boot_data <- data[indices, ]
  ctrl <- trainControl(method = "cv", number = 10)
  modelo_cv <- train(
    Class ~ PC1 + PC2 + PC3 + PC4 + PC5,
    data = boot_data,
    method = "multinom",
    trControl = ctrl,
    trace = FALSE,
    tuneGrid = expand.grid(decay = c(1e-4, 1e-3, 1e-2))
  )
  pred <- predict(modelo_cv, newdata = test_df_data_beans)
  acc <- mean(pred == test_df_data_beans$Class)
  return(acc)
}

# Ejecutar bootstrap no paramétrico
set.seed(8835)  
resultado_boot <- boot(data = train_df_data_beans, 
                       statistic = boot_accuracy,
                       R = 80)  
resultado_boot

# Intervalo de confianza del 95% para la accuracy
boot.ci(resultado_boot, type = "perc")

#=====================================================
# IMPLEMENTACION CON SVM
#=====================================================
# Implementacion simple
simple_svm = svm(Class ~ ., data = train_df_data_beans, scale = FALSE,
                 kernel = "radial", cost = 30, probability = TRUE)
simple_predictions_svm <- predict(simple_svm, newdata = test_df_data_beans)

confusionMatrix(simple_predictions_svm, test_df_data_beans$Class)

# Buscamos el mejor modelo por medio de una rejilla en kernel radial

set.seed(7324) 

# SVM con rejilla
tuning_grid <- expand.grid(
  sigma = c(0.01, 0.1, 1, 10),
  C = c(0.1, 1, 10, 100)       
)

# Entrenar modelo SVM 
model_svm_tuned <- train(
  Class ~ ., data = train_df_data_beans, method = "svmRadial",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid=tuning_grid
)

model_svm_tuned$bestTune
# Importancia de las variables
varImp(model_svm_tuned)

plot(model_svm_tuned)

predictions_svm <- predict(model_svm_tuned, newdata = test_df_data_beans)

confusionMatrix(predictions_svm, test_df_data_beans$Class)

best_model_svm <- model_svm_tuned$bestTune

#=====================================================
# IMPLEMENTACION CON RANDOM FOREST
#=====================================================
# Conjuntos de prueba y entrenamiento dividido
X_train_df_data_beans <- train_df_data_beans[,1:5]
Y_train_df_data_beans <- train_df_data_beans$Class
X_test_df_data_beans <- test_df_data_beans[,1:5]
Y_test_df_data_beans <- test_df_data_beans$Class

seed <- 235
metric <- 'Kappa'

# Creacion del modelo de clasificacion para random forest usando caret
customRF <- list(type = "Classification", library = "randomForest", loop = NULL)

# Definicion de los hiperparametros a buscar
customRF$parameters <- data.frame(
  parameter = c("maxnodes", "ntree"),
  class = rep("numeric", 2),
  label = c("maxnodes", "ntree")
)

# Funcion para grid
customRF$grid <- function(x, y, len = NULL, search = "grid") {}

# Clasificacion para factor
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, as.factor(y), maxnodes = param$maxnodes, ntree = param$ntree, ...)
}

# Funcion de prediccion
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL) {
  predict(modelFit, newdata)
}

# Habilitar prediccion de probabilidades
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL) {
  predict(modelFit, newdata, type = "prob")
}

# Difinido para clasificacion
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

# Entrenamiento por validacion cruzada
control <- trainControl(method="repeatedcv", number=10, repeats=3, search='grid')

# Definimos la rejilla
tunegrid <- expand.grid(.maxnodes = c(100,150,200), .ntree = c(200,250,300))

# Entrenamiento del modelo usando rejilla
set.seed(seed)
rf_gridsearch <- train(
  x = X_train_df_data_beans,
  y = Y_train_df_data_beans,
  method = customRF,
  metric = metric,
  tuneGrid = tunegrid,
  trControl = control
)

par(mfrow = c(1,1))
# Visualizacion de bosque
plot(rf_gridsearch)

rf_gridsearch$bestTune

# Importancia de variables
varImpPlot(rf_gridsearch$finalModel, main ='Importancia')
pred_beans_rf <- predict(rf_gridsearch, newdata = X_test_df_data_beans)
conf_matrix_beans_rf <- confusionMatrix(pred_beans_rf, Y_test_df_data_beans)
conf_matrix_beans_rf

#=====================================================
# IMPLEMENTACION CON XGBOOST
#=====================================================
# Necesitams convinar los datos de prueba y entrenamiento
train_data <- cbind(X_train_df_data_beans, Class = Y_train_df_data_beans)
test_data <- cbind(X_test_df_data_beans, Class = Y_test_df_data_beans)

control <- trainControl(
  method = "cv",
  number = 10,
  verboseIter = TRUE,
  allowParallel = TRUE,
  classProbs = TRUE  # opcional para probabilidades
)

xgb_grid <- expand.grid(
  nrounds = c(50, 100),
  max_depth = c(8, 16),
  eta = c(0.01, 0.1),
  gamma = c(0),
  colsample_bytree = c(0.8),
  min_child_weight = c(1, 6),
  subsample = c(0.8)
)

set.seed(9746)
xgb_caret <- train(
  Class ~ .,
  data = train_data,
  method = "xgbTree",
  trControl = control,
  tuneGrid = xgb_grid,
  metric = "Accuracy"
)

# Mejor modelo
xgb_caret$bestTune

# Importancia de las variables
model <- xgb_caret$finalModel
names <- dimnames(X_train_df_data_beans)[[2]]
importance_matrix <- xgb.importance(feature_names = names, model = model)
gp <- xgb.plot.importance(importance_matrix)
print(gp)

# Hacemos las predicciones
pred_class <- predict(xgb_caret, newdata = test_data)
# Matriz de confusión para clasificación multiclase
confusionMatrix(factor(pred_class), factor(test_data$Class))


#===========================================================
# INTENTO DE MEJORA CON STACKING
#===========================================================
# Paso 1: Obtener probabilidades de entrenamiento
rl_train_prob          <- predict(rl_multinom,     newdata = X_train_df_data_beans, type = "prob")
rf_train_prob          <- predict(rf_gridsearch,   newdata = X_train_df_data_beans, type = "prob")
xgb_train_prob         <- predict(xgb_caret,       newdata = X_train_df_data_beans, type = "prob")

# Paso 2: Renombrar columnas para evitar colisiones
colnames(rl_train_prob)         <- paste0("rl_", colnames(rl_train_prob))
colnames(rf_train_prob)         <- paste0("rf_", colnames(rf_train_prob))
colnames(xgb_train_prob)        <- paste0("xgb_", colnames(xgb_train_prob))

# Paso 3: Combinar en un único data frame
stack_train <- data.frame(
  rl_train_prob,
  #svm_simple_train_prob,
  rf_train_prob,
  xgb_train_prob,
  Class = Y_train_df_data_beans
)

# Paso 4: Entrenar el meta-modelo (stacking con regresión logística multinomial)
set.seed(9875)
meta_model <- train(
  Class ~ .,
  data = stack_train,
  method = "multinom",
  trControl = trainControl(
    method = "cv",
    number = 10
  ),
  trace = FALSE
)

# Paso 5: Obtener probabilidades de test
rl_test_prob          <- predict(rl_multinom,     newdata = X_test_df_data_beans, type = "prob")
rf_test_prob          <- predict(rf_gridsearch,   newdata = X_test_df_data_beans, type = "prob")
xgb_test_prob         <- predict(xgb_caret,       newdata = X_test_df_data_beans, type = "prob")

# Paso 6: Renombrar columnas del test para que coincidan
colnames(rl_test_prob)         <- paste0("rl_", colnames(rl_test_prob))
colnames(rf_test_prob)         <- paste0("rf_", colnames(rf_test_prob))
colnames(xgb_test_prob)        <- paste0("xgb_", colnames(xgb_test_prob))

# Paso 7: Crear conjunto de prueba del stacking
stack_test <- data.frame(
  rl_test_prob,
  rf_test_prob,
  xgb_test_prob
)

# Paso 8: Predecir con el meta-modelo
final_pred <- predict(meta_model, newdata = stack_test)

# Paso 9: Evaluar el rendimiento
confusionMatrix(final_pred, Y_test_df_data_beans)

# Repetimos a partir del paso 4 con otro modelo
set.seed(9875)
meta_model_xgb <- train(
  Class ~ .,
  data = stack_train,
  method = "xgbTree",
  trControl = trainControl(
    method = "cv",
    number = 10
  ),
  verbose = FALSE
)

# Paso 5: Obtener probabilidades de test
rl_test_prob          <- predict(rl_multinom,     newdata = X_test_df_data_beans, type = "prob")
rf_test_prob          <- predict(rf_gridsearch,   newdata = X_test_df_data_beans, type = "prob")
xgb_test_prob         <- predict(xgb_caret,       newdata = X_test_df_data_beans, type = "prob")

# Paso 6: Renombrar columnas del test para que coincidan
colnames(rl_test_prob)         <- paste0("rl_", colnames(rl_test_prob))
colnames(rf_test_prob)         <- paste0("rf_", colnames(rf_test_prob))
colnames(xgb_test_prob)        <- paste0("xgb_", colnames(xgb_test_prob))

# Paso 7: Crear conjunto de prueba del stacking
stack_test <- data.frame(
  rl_test_prob,
  rf_test_prob,
  xgb_test_prob
)

# Paso 8: Predecir con el meta-modelo
final_pred_xgb <- predict(meta_model_xgb, newdata = stack_test)

# Paso 9: Evaluar el rendimiento
confusionMatrix(final_pred_xgb, Y_test_df_data_beans)



#===========================================================
# Curvas ROC BARBUNYA
#===========================================================
clase_interes <- levels(Y_test_df_data_beans)[1]

# Obtener probabilidades para la clase de interés
prob_rl         <- predict(rl_multinom, newdata = X_test_df_data_beans, type = "prob")[, clase_interes]
prob_rf         <- predict(rf_gridsearch, newdata = X_test_df_data_beans, type = "prob")[, clase_interes]
prob_xgb        <- predict(xgb_caret, newdata = X_test_df_data_beans, type = "prob")[, clase_interes]
prob_meta       <- predict(meta_model, newdata = stack_test, type = "prob")[, clase_interes]
prob_meta_xgb   <- predict(meta_model_xgb, newdata = stack_test, type = "prob")[, clase_interes]

# Obtener probabilidades para SVM
pred_svm <- predict(simple_svm, newdata = X_test_df_data_beans, probability = TRUE)
prob_svm <- attr(pred_svm, "probabilities")[, clase_interes]

# Crear objetos ROC
roc_rl         <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_rl)
roc_rf         <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_rf)
roc_xgb        <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_xgb)
roc_meta       <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_meta)
roc_meta_xgb   <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_meta_xgb)
roc_svm        <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_svm)

# Graficar curvas ROC
plot(roc_rl, col = "red", main = paste("ROC Curves para clase", clase_interes), lwd = 2)
plot(roc_svm, col = "blue", add = TRUE, lwd = 2)
plot(roc_rf, col = "green", add = TRUE, lwd = 2)
plot(roc_xgb, col = "purple", add = TRUE, lwd = 2)
plot(roc_meta, col = "black", add = TRUE, lwd = 3)
plot(roc_meta_xgb, col = "orange", add = TRUE, lwd = 3)

legend("bottomright",
       legend = c("RL", "SVM", "RF", "XGB", "Stacking (Multinom)", "Stacking (XGB)"),
       col = c("red", "blue", "green", "purple", "black", "orange"),
       lwd = c(2, 2, 2, 2, 3, 3))

# Calcular AUCs
auc_roc_rl         <- auc(roc_rl)
auc_roc_svm        <- auc(roc_svm)
auc_roc_rf         <- auc(roc_rf)
auc_roc_xgb        <- auc(roc_xgb)
auc_roc_meta       <- auc(roc_meta)
auc_roc_meta_xgb   <- auc(roc_meta_xgb)

# Mostrar AUCs
auc_roc_rl
auc_roc_svm
auc_roc_rf
auc_roc_xgb
auc_roc_meta
auc_roc_meta_xgb

#===========================================================
# Curvas ROC BOMBAY
#===========================================================
clase_interes <- levels(Y_test_df_data_beans)[2]

# Obtener probabilidades para la clase de interés
prob_rl         <- predict(rl_multinom, newdata = X_test_df_data_beans, type = "prob")[, clase_interes]
prob_rf         <- predict(rf_gridsearch, newdata = X_test_df_data_beans, type = "prob")[, clase_interes]
prob_xgb        <- predict(xgb_caret, newdata = X_test_df_data_beans, type = "prob")[, clase_interes]
prob_meta       <- predict(meta_model, newdata = stack_test, type = "prob")[, clase_interes]
prob_meta_xgb   <- predict(meta_model_xgb, newdata = stack_test, type = "prob")[, clase_interes]

# Obtener probabilidades para SVM
pred_svm <- predict(simple_svm, newdata = X_test_df_data_beans, probability = TRUE)
prob_svm <- attr(pred_svm, "probabilities")[, clase_interes]

# Crear objetos ROC
roc_rl         <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_rl)
roc_rf         <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_rf)
roc_xgb        <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_xgb)
roc_meta       <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_meta)
roc_meta_xgb   <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_meta_xgb)
roc_svm        <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_svm)

# Graficar curvas ROC
plot(roc_rl, col = "red", main = paste("ROC Curves para clase", clase_interes), lwd = 2)
plot(roc_svm, col = "blue", add = TRUE, lwd = 2)
plot(roc_rf, col = "green", add = TRUE, lwd = 2)
plot(roc_xgb, col = "purple", add = TRUE, lwd = 2)
plot(roc_meta, col = "black", add = TRUE, lwd = 3)
plot(roc_meta_xgb, col = "orange", add = TRUE, lwd = 3)

legend("bottomright",
       legend = c("RL", "SVM", "RF", "XGB", "Stacking (Multinom)", "Stacking (XGB)"),
       col = c("red", "blue", "green", "purple", "black", "orange"),
       lwd = c(2, 2, 2, 2, 3, 3))

# Calcular AUCs
auc_roc_rl         <- auc(roc_rl)
auc_roc_svm        <- auc(roc_svm)
auc_roc_rf         <- auc(roc_rf)
auc_roc_xgb        <- auc(roc_xgb)
auc_roc_meta       <- auc(roc_meta)
auc_roc_meta_xgb   <- auc(roc_meta_xgb)

# Mostrar AUCs
auc_roc_rl
auc_roc_svm
auc_roc_rf
auc_roc_xgb
auc_roc_meta
auc_roc_meta_xgb

#===========================================================
# Curvas ROC CALI
#===========================================================
clase_interes <- levels(Y_test_df_data_beans)[3]

# Obtener probabilidades para la clase de interés
prob_rl         <- predict(rl_multinom, newdata = X_test_df_data_beans, type = "prob")[, clase_interes]
prob_rf         <- predict(rf_gridsearch, newdata = X_test_df_data_beans, type = "prob")[, clase_interes]
prob_xgb        <- predict(xgb_caret, newdata = X_test_df_data_beans, type = "prob")[, clase_interes]
prob_meta       <- predict(meta_model, newdata = stack_test, type = "prob")[, clase_interes]
prob_meta_xgb   <- predict(meta_model_xgb, newdata = stack_test, type = "prob")[, clase_interes]

# Obtener probabilidades para SVM
pred_svm <- predict(simple_svm, newdata = X_test_df_data_beans, probability = TRUE)
prob_svm <- attr(pred_svm, "probabilities")[, clase_interes]

# Crear objetos ROC
roc_rl         <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_rl)
roc_rf         <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_rf)
roc_xgb        <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_xgb)
roc_meta       <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_meta)
roc_meta_xgb   <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_meta_xgb)
roc_svm        <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_svm)

# Graficar curvas ROC
plot(roc_rl, col = "red", main = paste("ROC Curves para clase", clase_interes), lwd = 2)
plot(roc_svm, col = "blue", add = TRUE, lwd = 2)
plot(roc_rf, col = "green", add = TRUE, lwd = 2)
plot(roc_xgb, col = "purple", add = TRUE, lwd = 2)
plot(roc_meta, col = "black", add = TRUE, lwd = 3)
plot(roc_meta_xgb, col = "orange", add = TRUE, lwd = 3)

legend("bottomright",
       legend = c("RL", "SVM", "RF", "XGB", "Stacking (Multinom)", "Stacking (XGB)"),
       col = c("red", "blue", "green", "purple", "black", "orange"),
       lwd = c(2, 2, 2, 2, 3, 3))

# Calcular AUCs
auc_roc_rl         <- auc(roc_rl)
auc_roc_svm        <- auc(roc_svm)
auc_roc_rf         <- auc(roc_rf)
auc_roc_xgb        <- auc(roc_xgb)
auc_roc_meta       <- auc(roc_meta)
auc_roc_meta_xgb   <- auc(roc_meta_xgb)

# Mostrar AUCs
auc_roc_rl
auc_roc_svm
auc_roc_rf
auc_roc_xgb
auc_roc_meta
auc_roc_meta_xgb

#===========================================================
# Curvas ROC DERMASON
#===========================================================
clase_interes <- levels(Y_test_df_data_beans)[4]

# Obtener probabilidades para la clase de interés
prob_rl         <- predict(rl_multinom, newdata = X_test_df_data_beans, type = "prob")[, clase_interes]
prob_rf         <- predict(rf_gridsearch, newdata = X_test_df_data_beans, type = "prob")[, clase_interes]
prob_xgb        <- predict(xgb_caret, newdata = X_test_df_data_beans, type = "prob")[, clase_interes]
prob_meta       <- predict(meta_model, newdata = stack_test, type = "prob")[, clase_interes]
prob_meta_xgb   <- predict(meta_model_xgb, newdata = stack_test, type = "prob")[, clase_interes]

# Obtener probabilidades para SVM
pred_svm <- predict(simple_svm, newdata = X_test_df_data_beans, probability = TRUE)
prob_svm <- attr(pred_svm, "probabilities")[, clase_interes]

# Crear objetos ROC
roc_rl         <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_rl)
roc_rf         <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_rf)
roc_xgb        <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_xgb)
roc_meta       <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_meta)
roc_meta_xgb   <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_meta_xgb)
roc_svm        <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_svm)

# Graficar curvas ROC
plot(roc_rl, col = "red", main = paste("ROC Curves para clase", clase_interes), lwd = 2)
plot(roc_svm, col = "blue", add = TRUE, lwd = 2)
plot(roc_rf, col = "green", add = TRUE, lwd = 2)
plot(roc_xgb, col = "purple", add = TRUE, lwd = 2)
plot(roc_meta, col = "black", add = TRUE, lwd = 3)
plot(roc_meta_xgb, col = "orange", add = TRUE, lwd = 3)

legend("bottomright",
       legend = c("RL", "SVM", "RF", "XGB", "Stacking (Multinom)", "Stacking (XGB)"),
       col = c("red", "blue", "green", "purple", "black", "orange"),
       lwd = c(2, 2, 2, 2, 3, 3))

# Calcular AUCs
auc_roc_rl         <- auc(roc_rl)
auc_roc_svm        <- auc(roc_svm)
auc_roc_rf         <- auc(roc_rf)
auc_roc_xgb        <- auc(roc_xgb)
auc_roc_meta       <- auc(roc_meta)
auc_roc_meta_xgb   <- auc(roc_meta_xgb)

# Mostrar AUCs
auc_roc_rl
auc_roc_svm
auc_roc_rf
auc_roc_xgb
auc_roc_meta
auc_roc_meta_xgb

#===========================================================
# Curvas ROC HOROZ
#===========================================================
clase_interes <- levels(Y_test_df_data_beans)[5]

# Obtener probabilidades para la clase de interés
prob_rl         <- predict(rl_multinom, newdata = X_test_df_data_beans, type = "prob")[, clase_interes]
prob_rf         <- predict(rf_gridsearch, newdata = X_test_df_data_beans, type = "prob")[, clase_interes]
prob_xgb        <- predict(xgb_caret, newdata = X_test_df_data_beans, type = "prob")[, clase_interes]
prob_meta       <- predict(meta_model, newdata = stack_test, type = "prob")[, clase_interes]
prob_meta_xgb   <- predict(meta_model_xgb, newdata = stack_test, type = "prob")[, clase_interes]

# Obtener probabilidades para SVM
pred_svm <- predict(simple_svm, newdata = X_test_df_data_beans, probability = TRUE)
prob_svm <- attr(pred_svm, "probabilities")[, clase_interes]

# Crear objetos ROC
roc_rl         <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_rl)
roc_rf         <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_rf)
roc_xgb        <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_xgb)
roc_meta       <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_meta)
roc_meta_xgb   <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_meta_xgb)
roc_svm        <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_svm)

# Graficar curvas ROC
plot(roc_rl, col = "red", main = paste("ROC Curves para clase", clase_interes), lwd = 2)
plot(roc_svm, col = "blue", add = TRUE, lwd = 2)
plot(roc_rf, col = "green", add = TRUE, lwd = 2)
plot(roc_xgb, col = "purple", add = TRUE, lwd = 2)
plot(roc_meta, col = "black", add = TRUE, lwd = 3)
plot(roc_meta_xgb, col = "orange", add = TRUE, lwd = 3)

legend("bottomright",
       legend = c("RL", "SVM", "RF", "XGB", "Stacking (Multinom)", "Stacking (XGB)"),
       col = c("red", "blue", "green", "purple", "black", "orange"),
       lwd = c(2, 2, 2, 2, 3, 3))

# Calcular AUCs
auc_roc_rl         <- auc(roc_rl)
auc_roc_svm        <- auc(roc_svm)
auc_roc_rf         <- auc(roc_rf)
auc_roc_xgb        <- auc(roc_xgb)
auc_roc_meta       <- auc(roc_meta)
auc_roc_meta_xgb   <- auc(roc_meta_xgb)

# Mostrar AUCs
auc_roc_rl
auc_roc_svm
auc_roc_rf
auc_roc_xgb
auc_roc_meta
auc_roc_meta_xgb

#===========================================================
# Curvas ROC SEKER
#===========================================================
clase_interes <- levels(Y_test_df_data_beans)[6]

# Obtener probabilidades para la clase de interés
prob_rl         <- predict(rl_multinom, newdata = X_test_df_data_beans, type = "prob")[, clase_interes]
prob_rf         <- predict(rf_gridsearch, newdata = X_test_df_data_beans, type = "prob")[, clase_interes]
prob_xgb        <- predict(xgb_caret, newdata = X_test_df_data_beans, type = "prob")[, clase_interes]
prob_meta       <- predict(meta_model, newdata = stack_test, type = "prob")[, clase_interes]
prob_meta_xgb   <- predict(meta_model_xgb, newdata = stack_test, type = "prob")[, clase_interes]

# Obtener probabilidades para SVM
pred_svm <- predict(simple_svm, newdata = X_test_df_data_beans, probability = TRUE)
prob_svm <- attr(pred_svm, "probabilities")[, clase_interes]

# Crear objetos ROC
roc_rl         <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_rl)
roc_rf         <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_rf)
roc_xgb        <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_xgb)
roc_meta       <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_meta)
roc_meta_xgb   <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_meta_xgb)
roc_svm        <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_svm)

# Graficar curvas ROC
plot(roc_rl, col = "red", main = paste("ROC Curves para clase", clase_interes), lwd = 2)
plot(roc_svm, col = "blue", add = TRUE, lwd = 2)
plot(roc_rf, col = "green", add = TRUE, lwd = 2)
plot(roc_xgb, col = "purple", add = TRUE, lwd = 2)
plot(roc_meta, col = "black", add = TRUE, lwd = 3)
plot(roc_meta_xgb, col = "orange", add = TRUE, lwd = 3)

legend("bottomright",
       legend = c("RL", "SVM", "RF", "XGB", "Stacking (Multinom)", "Stacking (XGB)"),
       col = c("red", "blue", "green", "purple", "black", "orange"),
       lwd = c(2, 2, 2, 2, 3, 3))

# Calcular AUCs
auc_roc_rl         <- auc(roc_rl)
auc_roc_svm        <- auc(roc_svm)
auc_roc_rf         <- auc(roc_rf)
auc_roc_xgb        <- auc(roc_xgb)
auc_roc_meta       <- auc(roc_meta)
auc_roc_meta_xgb   <- auc(roc_meta_xgb)

# Mostrar AUCs
auc_roc_rl
auc_roc_svm
auc_roc_rf
auc_roc_xgb
auc_roc_meta
auc_roc_meta_xgb


#===========================================================
# Curvas ROC SIRA
#===========================================================
clase_interes <- levels(Y_test_df_data_beans)[7]

# Obtener probabilidades para la clase de interés
prob_rl         <- predict(rl_multinom, newdata = X_test_df_data_beans, type = "prob")[, clase_interes]
prob_rf         <- predict(rf_gridsearch, newdata = X_test_df_data_beans, type = "prob")[, clase_interes]
prob_xgb        <- predict(xgb_caret, newdata = X_test_df_data_beans, type = "prob")[, clase_interes]
prob_meta       <- predict(meta_model, newdata = stack_test, type = "prob")[, clase_interes]
prob_meta_xgb   <- predict(meta_model_xgb, newdata = stack_test, type = "prob")[, clase_interes]

# Obtener probabilidades para SVM
pred_svm <- predict(simple_svm, newdata = X_test_df_data_beans, probability = TRUE)
prob_svm <- attr(pred_svm, "probabilities")[, clase_interes]

# Crear objetos ROC
roc_rl         <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_rl)
roc_rf         <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_rf)
roc_xgb        <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_xgb)
roc_meta       <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_meta)
roc_meta_xgb   <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_meta_xgb)
roc_svm        <- roc(response = (Y_test_df_data_beans == clase_interes), predictor = prob_svm)

# Graficar curvas ROC
plot(roc_rl, col = "red", main = paste("ROC Curves para clase", clase_interes), lwd = 2)
plot(roc_svm, col = "blue", add = TRUE, lwd = 2)
plot(roc_rf, col = "green", add = TRUE, lwd = 2)
plot(roc_xgb, col = "purple", add = TRUE, lwd = 2)
plot(roc_meta, col = "black", add = TRUE, lwd = 3)
plot(roc_meta_xgb, col = "orange", add = TRUE, lwd = 3)

legend("bottomright",
       legend = c("RL", "SVM", "RF", "XGB", "Stacking (Multinom)", "Stacking (XGB)"),
       col = c("red", "blue", "green", "purple", "black", "orange"),
       lwd = c(2, 2, 2, 2, 3, 3))

# Calcular AUCs
auc_roc_rl         <- auc(roc_rl)
auc_roc_svm        <- auc(roc_svm)
auc_roc_rf         <- auc(roc_rf)
auc_roc_xgb        <- auc(roc_xgb)
auc_roc_meta       <- auc(roc_meta)
auc_roc_meta_xgb   <- auc(roc_meta_xgb)

# Mostrar AUCs
auc_roc_rl
auc_roc_svm
auc_roc_rf
auc_roc_xgb
auc_roc_meta
auc_roc_meta_xgb
