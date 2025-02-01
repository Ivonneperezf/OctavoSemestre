library(mlr)
library(tidyverse)
install.packages("mlr")
install.packages("tidyverse")
install.packages("mlbench")
data(HouseVotes84, package = "mlbench")
library(mlbench)
votesTib <- as_tibble(HouseVotes84)
votesTib
#Devuelve el numeroo de valores faltantes
map_dbl(votesTib, ~sum(is.na(.)))

votesUntidy <- gather(votesTib, "Variable", "Value", -Class)
ggplot(votesUntidy, aes(Class, fill = Value)) +
  facet_wrap(~ Variable, scales = "free_y") +
  geom_bar(position = "fill") +
  theme_bw()

votesTask <- makeClassifTask(data = votesTib, target = "Class")
bayes <- makeLearner("classif.naiveBayes")
bayesModel <- train(bayes, votesTask)


kFold <- makeResampleDesc(method = "RepCV", folds = 10, reps = 50,
                          stratify = TRUE)
bayesCV <- resample(learner = bayes, task = votesTask,
                    resampling = kFold,
                    measures = list(mmce, acc, fpr, fnr))
bayesCV$aggr
politician <- tibble(V1 = "n", V2 = "n", V3 = "y", V4 = "n", V5 = "n",
                     V6 = "y", V7 = "y", V8 = "y", V9 = "y", V10 = "y",
                     V11 = "n", V12 = "y", V13 = "n", V14 = "n",
                     V15 = "y", V16 = "n")
politicianPred <- predict(bayesModel, newdata = politician)
getPredictionResponse(politicianPred)