library(ParamHelpers)
library(mlr)
library(tidyverse)
library(mlbench)

#Extraccion de los datos
data(Zoo, package = "mlbench")
zooTib <- as_tibble(Zoo)
zooTib

#Convertimos a factor las logicas
zooTib <- mutate_if(zooTib, is.logical, as.factor)

#Construccion del modelo
zooTask <- makeClassifTask(data = zooTib, target = "type")
tree <- makeLearner("classif.rpart")
#Casos faltantes para considerar parametros de sustituto maxsurrogate
map_dbl(zooTib,~sum(is.na(.)))
#ver parametros
getParamSet(tree)

#Ajuste de parametros
treeParamSpace <- makeParamSet(
makeIntegerParam("minsplit", lower = 5, upper = 20),
makeIntegerParam("minbucket", lower = 3, upper = 10),
makeNumericParam("cp", lower = 0.01, upper = 0.1),
makeIntegerParam("maxdepth", lower = 3, upper = 10))

#Para definir el espacio de hiperparametros, podemos hacer una busqueda aleatoria
#esto por medio de 200 iteraciones, asi tambien se implementa k-folds
randSearch <- makeTuneControlRandom(maxit = 200)
cvForTuning <- makeResampleDesc("CV", iters = 5)

#Realizamos ajuste de hiperparametros
library(parallel) # está ya instalado, es parte de la base de R
library(parallelMap)
parallelStartSocket(cpus = detectCores())
tunedTreePars <- tuneParams(tree, task = zooTask,
                            resampling = cvForTuning,
                            par.set = treeParamSpace,
                            control = randSearch)
parallelStop()
tunedTreePars

#Ahora que hemos ajustado nuestros hiperparámetros, podemos entrenar nuestro
#modelo final usándolos.
tunedTree <- setHyperPars(tree, par.vals = tunedTreePars$x)
tunedTreeModel <- train(tunedTree, zooTask)


#Dibujamos el arbol
library(rpart.plot)
treeModelData <- getLearnerModel(tunedTreeModel)
rpart.plot(treeModelData, roundint = FALSE,
           box.palette = "BuBn",
           type = 5)

#valores cp
printcp(treeModelData, digits = 3)
summary(treeModelData)

#Evaluacion del modelo por validacion cruzada
outer <- makeResampleDesc("CV", iters = 5)
treeWrapper <- makeTuneWrapper("classif.rpart", resampling = cvForTuning,
                               par.set = treeParamSpace,
                               control = randSearch)
parallelStartSocket(cpus = detectCores())
cvWithTuning <- resample(treeWrapper, zooTask, resampling = outer)
parallelStop()

#Ahora veamos el resultado de la validación cruzada y veamos cómo se desempeñó
#nuestro proceso de creación de modelos.
cvWithTuning