library(foreign)
data <- read.dta("hsbdemo.dta")
head(data,5)
View(data)
str(data)
summary(data)

#Separacion 70-30 del conjunto de prueba y entrenamiento
library(caret)
set.seed(1)
trainIndex <- createDataPartition(data$prog, p=0.7)$Resample1
train <- data[trainIndex, ]
test<- data[-trainIndex, ]

# Classifier 
library(e1071)
NBclassfier <- naiveBayes(prog~., data=train)
# Prior
NBclassfier$apriori

# Posterior
NBclassfier$tables$science
NBclassfier$tables$honors

trainPred <- predict(NBclassfier, newdata = train, type = "class")
trainTable <- table(train$prog, trainPred)
testPred <- predict(NBclassfier, newdata=test, type="class")
testTable <- table(test$prog, testPred)
trainAcc <- sum(diag(trainTable))/sum(trainTable)
testAcc<- sum(diag(testTable))/sum(testTable)
message("Confusion Matrix for Training Data")
print(trainTable)

message("Confusion Matrix for Test Data")
print(testTable)

message("Accuracy")
print(round(cbind(trainAccuracy=trainAcc, testAccuracy=testAcc),3))

unique(data$prog)
#------------------------------------------------------
# Filtrar datos por categoría de prog y calcular las matrices de correlación
library(dplyr)

corr_gen <- cor(filter(data, prog == "general") %>% select(read, write, math, science, socst))
corr_acad <- cor(filter(data, prog == "academic") %>% select(read, write, math, science, socst))
corr_voc <- cor(filter(data, prog == "vocation") %>% select(read, write, math, science, socst))


# Imprimir las matrices
print("Correlation matrix within general")
print(corr_gen)

print("Correlation matrix within academic")
print(corr_acad)

print("CCorrelation matrix within vocational")
print(corr_voc)


# Boxplots para algunas variables según prog
par(mfrow=c(2, 2)) # Crear una grilla de gráficos
boxplot(read ~ prog, data = data, main = "Read Scores by Program")
boxplot(write ~ prog, data = data, main = "Write Scores by Program")
boxplot(math ~ prog, data = data, main = "Math Scores by Program")
boxplot(science ~ prog, data = data, main = "Science Scores by Program")

# Gráficos de densidad por programa
par(mfrow = c(2, 2))
plot(density(subset(data, prog == "general")$read), main = "Read Density (General)", col = "blue")
plot(density(subset(data, prog == "academic")$read), main = "Read Density (Academic)", col = "green")
plot(density(subset(data, prog == "vocation")$read), main = "Read Density (Vocational)", col = "red")


library(ggplot2)
ggplot(data, aes(x = prog, y = science, fill = prog)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Science Score boxplot", y = "Science Score") +
  theme_minimal()

ggplot(data, aes(x = prog, y = socst, fill = prog)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Socst Score boxplot", y = "Socst Score") +
  theme_minimal()

ggplot(data, aes(x = prog, y = read, fill = prog)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Socst Score boxplot", y = "Read Score") +
  theme_minimal()

ggplot(data, aes(x = prog, y = write, fill = prog)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Socst Score boxplot", y = "Write Score") +
  theme_minimal()

ggplot(data, aes(x = prog, y = math, fill = prog)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Socst Score boxplot", y = "Math Score") +
  theme_minimal()

variables <- c("science","socst","read", "write", "math")
plots <- lapply(variables, function(var) {
  ggplot(data, aes_string(x = var, fill = "prog")) +
    geom_density(alpha = 0.5) +
    labs(title = paste("Density of", var), x = paste("Score in", var)) +
    theme_minimal()
})

# Mostrar los gráficos (si estás en RStudio, puedes usar gridExtra o patchwork para visualizarlos juntos)
library(gridExtra)
do.call(grid.arrange, c(plots, ncol = 2))









# Gráficos de Densidad
ggplot(data, aes(x = science, fill = prog)) +
  geom_density(alpha = 0.5) +
  labs(title = "Science Score Density", x = "Score in Science") +
  theme_minimal()

ggplot(data, aes(x = socst, fill = prog)) +
  geom_density(alpha = 0.5) +
  labs(title = "Socst Score Density", x = "Score in Socst") +
  theme_minimal()
