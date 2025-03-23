# Carga de librerías
library(mlr)
library(tidyverse)
library(GGally)
library(clue)
library(clusterSim)

# Carga de tibble para analizar datos
data(GvHD, package = "mclust")
gvhdPosTib <- as_tibble(GvHD.pos)
gvhdPosTib

# Escalado de datos
gvhdPosScaled <- gvhdPosTib %>% scale()

ggpairs(gvhdPosTib,
        upper = list(continuous = "density"),
        lower = list(continuous = wrap("points", size = 0.5)),
        diag = list(continuous = "densityDiag")) +
  theme_bw()

# Implementación de k-means
gvhdPosTask <- makeClusterTask(data = as.data.frame(gvhdPosScaled))
listLearners("cluster")$class
kMeans <- makeLearner("cluster.kmeans",
                      par.vals = list(iter.max = 400, nstart = 10))

# Parámetros por medio del método de la rejilla y k-folds
kMeansParamSpace <- makeParamSet(
  makeDiscreteParam("centers", values = 2:8),
  makeDiscreteParam("algorithm",
                    values = c("Hartigan-Wong", "Lloyd", "MacQueen")))
gridSearch <- makeTuneControlGrid()
kFold <- makeResampleDesc("CV", iters = 10)

# Evaluación de parámetros
tunedK <- tuneParams(kMeans, task = gvhdPosTask,
                     resampling = kFold,
                     par.set = kMeansParamSpace,
                     control = gridSearch,
                     measures = list(db, G1)) 

# Gráficos de evaluación de métricas
kMeansTuningData <- generateHyperParsEffectData(tunedK)
kMeansTuningData$data
gatheredTuningData <- gather(kMeansTuningData$data,
                             key = "Metric",
                             value = "Value",
                             c(-centers, -iteration, -algorithm))
ggplot(gatheredTuningData, aes(centers, Value, col = algorithm)) +
  facet_wrap(~ Metric, scales = "free_y") +
  geom_line() +
  geom_point() +
  theme_bw()

# Implementación óptima de k-means
tunedKMeans <- setHyperPars(kMeans, par.vals = tunedK$x)
tunedKMeansModel <- train(tunedKMeans, gvhdPosTask)
kMeansModelData <- getLearnerModel(tunedKMeansModel)
kMeansModelData$iter

# Visualización de los resultados
gvhdPosTib <- mutate(gvhdPosTib, kMeansCluster = as.factor(kMeansModelData$cluster))
ggpairs(gvhdPosTib, aes(col = kMeansCluster), upper = list(continuous = "density")) + theme_bw()
