library(tm) #Libreria para text mining
library(NLP) #Necesaria para la libreria de text mining
library(SnowballC) #Necesaria para hacer steaming (reducir a palabra base)
library(wordcloud) #Nubes de palabras
library(RColorBrewer) #Necesaria para nubes de palabras
library(naivebayes) #Para el modelo de Naive Bayes
library(gmodels) #Para matriz de confusion

##########################################
    #EXPLORACION DE LOS DATOS#
##########################################
#Carga de dataset
sms_raw <- read.csv("sms_spam.csv")
View(sms_raw)
str(sms_raw)

#Pasar a factor la varible que clasifica los mensajes
sms_raw$type <- factor(sms_raw$type)
str(sms_raw$type)
table(sms_raw$type)

##########################################
#LIMPIEZA Y ESTANDARIZACION DE TEXTO#
##########################################

#Creacion de bag of words
#Obtiene el cuerpo de origen del texto para suministrar el corpus separado por 
#documentos
sms_corpus <- VCorpus(VectorSource(sms_raw$text))
#View(sms_corpus)
#vignette("tm")
print(sms_corpus)
#Resumen para bolsa de palabras
inspect(sms_corpus[1:2])
#Visualizacion de texto
as.character(sms_corpus[[1]])
#Aplica la funcion de as.character para visualizar el texto
lapply(sms_corpus[1:2], as.character)
#Realizamos un mapeo con tm_map y estandarizamos en minusculas los registros
#es decir, transformamos los datos.
#content_transformer() es una funcion para funcion contenedora
sms_corpus_clean <- tm_map(sms_corpus,content_transformer(tolower))
as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[[1]])
#Eliminacion de numeros
#La funcion de removeNumbers esta incluida en tm, por lo que no es necesario
#aplicar una transformacion, eso se hace porque tolower no es de tm
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)
as.character(sms_corpus[[4]])
as.character(sms_corpus_clean[[4]])

stopwords()
#Filtramos las stopwords del corpus 
sms_corpus_clean <- tm_map(sms_corpus_clean,removeWords, stopwords())
as.character(sms_corpus[[4]])
as.character(sms_corpus_clean[[4]])
#Remover la puntuacion
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)
as.character(sms_corpus[[4]])
as.character(sms_corpus_clean[[4]])

wordStem(c("learn", "learned", "learning", "learns")) #stemming de learn
#Hacer stemming a todo el documento
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)
as.character(sms_corpus[[4]])
as.character(sms_corpus_clean[[4]])

#Eliminacion de espacios en blanco
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)
as.character(sms_corpus[1:3])
as.character(sms_corpus_clean[1:3])

############################################
#EXPLORACION DE TEXTO PREPARACION DE DATOS #
############################################
#Creacion de una matriz dispersa, lleva a un conteo de palabras (tokenizacion)
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)

#Tambien se pueden ajustar los parameros aqui para hacer la limpieza
sms_dtm2 <- DocumentTermMatrix(sms_corpus, control = list(tolower = TRUE,removeNumbers = TRUE,
                                                          stopwords =  function(x) { removeWords(x, stopwords()) },
                                                          removePunctuation = TRUE,
                                                          stemming = TRUE))
sms_dtm
sms_dtm2

##################################################
#CREACION DE CONJUNTOS DE PRUEBA Y ENTRENAMIENTO #
##################################################
#DTM funciona similar a un dataset, se puede manipular como tal
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test <- sms_dtm[4170:5559, ]
#Con etiquetas
sms_train_labels <- sms_raw[1:4169, ]$type
sms_test_labels <- sms_raw[4170:5559, ]$type
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

##################################################
#VISUALIZACION DE TEXTO #
##################################################

#Visualizacion de datos por nubes de palabras
wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE)
#Visualizacion de nubes de palabras por categorias
spam <- subset(sms_raw, type == "spam")
ham <- subset(sms_raw, type == "ham")
#Visualizacion en nubes del texto sin procesar
wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))

##################################################
#CREACION DE CARACTERISTICAS INDICADORAS DE      #
#PALABRAS FRECUENTES                             #
##################################################
#Filtramos las palabras caracteriticas poco frecuentes con la fucnion findFreqTerms
#Palabras que aparecen al menos en 5 sms
sms_freq_words <- findFreqTerms(sms_dtm_train, 5)
str(sms_freq_words)
#Por medio de las caracteristicas con mas frecuencia tomamos aquellas en el 
#conjuto d eentrenamiento y prueba
sms_dtm_freq_train <- sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words]
sms_dtm_freq_test

#Cambiamos los valores numericos 1 y 0 a si y no
convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

sms_train <- apply(sms_dtm_freq_train, MARGIN = 2,convert_counts)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2,convert_counts)
str(sms_train)

##################################################
#ENTRENAMIENTO DEL MODELO #
##################################################

#Entrenamiento del modelo
sms_classifier <- naive_bayes(sms_train, sms_train_labels)
warnings()

##################################################
#EVALUACION DEL RENDIMIENTO DEL MODELO #
##################################################
sms_test_pred <- predict(sms_classifier, sms_test)
CrossTable(sms_test_pred, sms_test_labels,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

##################################################
#MEJORANDO EL MODELO #
##################################################
sms_classifier2 <- naive_bayes(sms_train, sms_train_labels,laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_test_labels,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
