#Cargamos el dataset
misdatos<-data.frame(iris)
attach(misdatos)
library(rpart)
#Usamos un arbol CART
#rpart incluye criterios flexibles de poda
modelo<-rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
                data=misdatos, method="class")
plot(modelo)
#En cada nodo vamos a ver la clase prediminante y la distribucion de los datos
# en estas clases
text(modelo, use.n=TRUE, all=TRUE, cex=0.8)

#por medio de tree
library(tree)
#Aplicamos el modelo tratando de minimizar las impurezas por medio del
# indice Gini
modelo1<-tree(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=misdatos, 
                method="class", split="gini")
plot(modelo1)
text(modelo1, all=TRUE, cex=0.6)

#por medio de party
library(party)
modelo2<-ctree(Species ~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data=misdatos)
plot(modelo2)


#CONTROLAR EL NUMERO DE NODOS
library(tree)
misdatos<-data.frame(iris)
attach(misdatos)

modelo1<-tree(Species ~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=misdatos, 
              method="class", control=tree.control(nobs=150, mincut=10))
plot(modelo1)
text(modelo1, all=TRUE, cex=0.6)
predict(modelo1, iris) #esta salida nos muestra las probabilidades de 
# que una flor pertenezca a una clase de flor


modelo2<-ctree(Species~Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
               data=misdatos, controls=ctree_control(maxdepth=2))
plot(modelo2)

