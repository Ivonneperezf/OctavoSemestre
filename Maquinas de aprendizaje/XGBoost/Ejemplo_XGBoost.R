library(caret)
library(corrplot)
library(Rtsne)
library(xgboost)
library(stats)
library(ggplot2)
library(knitr)
knitr::opts_chunk$set(cache=TRUE)

# Carga de los datos
train = read.csv("pml-training.csv")
test = read.csv("pml-testing.csv")
dim(train)
dim(test)
View(train)
# Observamos que contiene la variable X que refiere a id

# Estraemos la variable objetivo "classe"
outcome.org <- train[["classe"]]
outcome <- as.factor(outcome.org)
# Verificar los niveles de la variable de resultado
levels(outcome)

# Dado que el algorimto deXGBoost solo reconoce datos numericos, se debe de transformar las categorias 
# a datos numericos
num.class = length(levels(outcome))
levels(outcome) = 1:num.class
head(outcome)

# Finalmente, ya no requerimos de usar las etiquetas en los datos de entranamiento general, por lo que
# podemos eliminarlas
train$classe = NULL

# Hacemos un filtro de datos para cinturon, antebrazo, brazo y mancuerna
# columnas de filtro por: cinturón, antebrazo, brazo, mancuerna
filter = grepl("belt|arm|dumbell", names(train))
train = train[, filter]
test = test[, filter]

# Dada que la imputacion es menos precisa y requiere de mayor exploracion, eliminamos NA
# eliminar columnas con NA, usar datos de prueba como referencia para NA
cols.without.na = colSums(is.na(test)) == 0
train = train[, cols.without.na]
test = test[, cols.without.na]

# Comporbamos si en los datos hay varianzas de cero, con el fin de ver si estas contienen una 
# distribucion separable
# comprobar si hay varianza cero
zero.var = nearZeroVar(train, saveMetrics=TRUE)
zero.var
# En caso de varianza cero se puede eliminar

# Revisamos graficamente la distribucion de las clases y sus caracteriticas
featurePlot(train, outcome, "strip")

# Observamos una matriz de correlacion respecto a cada variable
corrplot.mixed(cor(train), lower="circle", upper="color",tl.pos="lt", 
               diag="n", order="hclust", hclust.method="complete")

# Grafico t-SNE
# t-Distributed Stochastic Neighbor Embedding
tsne = Rtsne(as.matrix(train), check_duplicates=FALSE, pca=TRUE, 
             perplexity=30, theta=0.5, dims=2)
embedding = as.data.frame(tsne$Y)
embedding$Class = outcome
g = ggplot(embedding, aes(x=V1, y=V2, color=Class)) +
  geom_point(size=1.25) +
  guides(colour=guide_legend(override.aes=list(size=6))) +
  xlab("") + ylab("") +
  ggtitle("t-SNE 2D Embedding of 'Classe' Outcome") +
  theme_light(base_size=20) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())
print(g)

# Recordemos que este algoritmo solo admite resultados matriciales, por lo que debemos hacer
# a conversion antes de comenzar
# convert data to matrix
train.matrix = as.matrix(train)
mode(train.matrix) = "numeric"
test.matrix = as.matrix(test)
mode(test.matrix) = "numeric"
# convert outcome from factor to numeric matrix
# xgboost takes multi-labels in [0, numOfClass)
y = as.matrix(as.integer(outcome)-1)
y

# Esablecemos los parametros para generar validacion cruzada de 10-folds, objetivo de 
# funcion multicase como funcion de aprendizaje de la potenciacion del gardiente
# y la metrica de medicion por error
# xgboost parameters
param <- list("objective" = "multi:softprob", # multiclass classification 
              "num_class" = num.class, # number of classes 
              "eval_metric" = "merror", # evaluation metric 
              "nthread" = 8, # number of threads to be used
              "max_depth" = 16, # maximum depth of tree 
              "eta" = 0.3, # step size shrinkage 
              "gamma" = 0, # minimum loss reduction 
              "subsample" = 1, # part of data instances to grow tree 
              "colsample_bytree" = 1, # subsample ratio of columns when constructing each tree 
              "min_child_weight" = 12 # minimum sum of instance weight needed in a child 
              )

# Realizamos validacion cruzada con 4 folds
# set random seed, for reproducibility
set.seed(1234)
# k-fold cross validation, with timing
nround.cv = 200
system.time( bst.cv <- xgb.cv(param=param, data=train.matrix, label=y,
                              nfold=4, nrounds=nround.cv, prediction=TRUE, verbose=FALSE) ) 
tail(bst.cv$evaluation_log)
# index of minimum merror
evaluation_df <- as.data.frame(bst.cv$evaluation_log)
min.merror.idx <- which.min(evaluation_df$test_merror_mean)
min.merror.idx
bst.cv$evaluation_log[min.merror.idx,]

# Ya que implementamos el modelo y notamos que es un buen modelo, podemos crear la matriz 
# de confusion, usando la validacion cruzada
# get CV's prediction decoding
pred.cv = matrix(bst.cv$pred, nrow=length(bst.cv$pred)/num.class, ncol=num.class)
pred.cv = max.col(pred.cv, "last")
# confusion matrix
confusionMatrix(factor(y+1), factor(pred.cv))

# Ahora ajustaremos a todos los datos de entrenamiento
system.time( bst <- xgboost(param=param, data=train.matrix, label=y, 
                            nrounds=min.merror.idx, verbose=0) ) 

# Prediccion de los datos de prueba
# xgboost predict test data using the trained model
pred <- predict(bst, test.matrix)
head(pred, 10)

# Como observamos, los datos resultantes son probabilidades, por lo que debemos de 
# hacer un postprocesamiento para identificar la prediccion
# decode prediction
pred = matrix(pred, nrow=num.class, ncol=length(pred)/num.class)
pred = t(pred)
pred = max.col(pred, "last")
pred.char = toupper(letters[pred])

# Importancia de las caracteriticas
# get the trained model
model = xgb.dump(bst, with_stats=TRUE)
# get the feature real names
names = dimnames(train.matrix)[[2]]
# compute feature importance matrix
importance_matrix = xgb.importance(names, model=bst)
# plot
gp = xgb.plot.importance(importance_matrix)
print(gp)
