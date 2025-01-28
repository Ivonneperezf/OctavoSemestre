install.packages("e1071")
install.packages("mlr")
library(e1071)
data("Titanic")
Titanic_df=as.data.frame(Titanic)
View(Titanic_df)
str(Titanic_df)
#TRATAMIENTO DE LA INFORMACION
#Creando datos de la tabla
#La tabla se encontraba resumida, entonces se expande basandose en su frecuencia
repeating_sequence=rep.int(seq_len(nrow(Titanic_df)), Titanic_df$Freq)
repeating_sequence
table(seq_len(nrow(Titanic_df)))
#Esto repetirá cada combinación igual a la frecuencia de cada combinación
#Crear el conjunto de datos por repetición de fila creada
Titanic_dataset=Titanic_df[repeating_sequence,]
View(Titanic_dataset)
#Ya no necesitamos la frecuencia, elimina la característica
Titanic_dataset$Freq=NULL
str(Titanic_dataset)


#Ajuste del modelo Naïve Bayes
#Va a predecir Survived con todos los atributos(los atributos van despues de la tilde)
#en el conjunto de dats Titanic_dataset
Naive_Bayes_Model=naiveBayes(Survived ~., data=Titanic_dataset)
#¿Qué dice el modelo? Imprimir el resumen del modelo
#Nos indica las probabilidades a priori y las condicionales
Naive_Bayes_Model
#Predicción en el dataset
NB_Predictions=predict(Naive_Bayes_Model,Titanic_dataset)
#Matriz de Confusion para checar la precisión
table(NB_Predictions,Titanic_dataset$Survived)
#Da una presicion del 77.8%, lo que supone a malos resultaods






#Mejora del modelo
#Usando los modelos de mlr, intentamos mejorar
library(mlr)
#Create a classification task for learning on Titanic Dataset and specify the target feature
task = makeClassifTask(data = Titanic_dataset, target = "Survived")
#Initialize the Naive Bayes classifier
selected_model = makeLearner("classif.naiveBayes")
#Train the model
NB_mlr = train(selected_model, task)

#Read the model learned
NB_mlr$learner.model
#Predict on the dataset without passing the target feature
predictions_mlr = as.data.frame(predict(NB_mlr, newdata = Titanic_dataset[,1:3]))
##Confusion matrix to check accuracy
table(predictions_mlr[,1],Titanic_dataset$Survived)

