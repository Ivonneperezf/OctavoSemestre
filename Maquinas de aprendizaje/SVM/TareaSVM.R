# Creamos un conjunto de datos 
set.seed(10111)
x = matrix(rnorm(40), 20, 2)
y = rep(c(-1, 1), c(10, 10))
x[y == 1,] = x[y == 1,] + 1
plot(x, col = y + 3, pch = 19)

#Cargamos la libreria para SVM
library(e1071)
dat = data.frame(x, y = as.factor(y))
svmfit = svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = FALSE)
print(svmfit)
#str(dat)

#Funcion para graficar
make.grid = function(x, n = 75) {
  grange = apply(x, 2, range) 
  x1 = seq(from = grange[1,1], to = grange[2,1], length = n) 
  x2 = seq(from = grange[1,2], to = grange[2,2], length = n) 
  expand.grid(X1 = x1, X2 = x2) 
}

xgrid = make.grid(x)
xgrid[1:10,]

#Prediccion de los puntos de la malla
ygrid = predict(svmfit, xgrid)
plot(xgrid, col = c("red","blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)
points(x[svmfit$index,], pch = 5, cex = 2)

#oeficientes de la ecuacion
beta = drop(t(svmfit$coefs)%*%x[svmfit$index,])
beta0 = svmfit$rho

#Grafico con limites
plot(xgrid, col = c("red", "blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)
points(x[svmfit$index,], pch = 5, cex = 2)
abline(beta0 / beta[2], -beta[1] / beta[2])
abline((beta0 - 1) / beta[2], -beta[1] / beta[2], lty = 2)
abline((beta0 + 1) / beta[2], -beta[1] / beta[2], lty = 2)

#===================================================
#               SVM no lineal
#===================================================
load(file = "ESL.mixture.rda")
names(ESL.mixture)

rm(x, y)
attach(ESL.mixture)
plot(x, col = y + 1)

dat = data.frame(y = factor(y), x)
fit = svm(factor(y) ~ ., data = dat, scale = FALSE, kernel = "radial", cost = 5)

xgrid = expand.grid(X1 = px1, X2 = px2)
ygrid = predict(fit, xgrid)
plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = .2)
points(x, col = y + 1, pch = 19)


#Trazamos el limite del conjunto con los datos predichos de la regilla
func = predict(fit, xgrid, decision.values = TRUE)
func = attributes(func)$decision
xgrid = expand.grid(X1 = px1, X2 = px2)
ygrid = predict(fit, xgrid)
plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = .2)
points(x, col = y + 1, pch = 19)
contour(px1, px2, matrix(func, 69, 99), level = 0, add = TRUE)
contour(px1, px2, matrix(func, 69, 99), level = 0.5, add = TRUE, col = "blue", lwd = 2)


#Trabajo extra
set.seed (1)
x=matrix (rnorm (200*2) , ncol =2)
x[1:100 ,]=x[1:100 ,]+2
x[101:150 ,]= x[101:150 ,] -2
y=c(rep (1 ,150) ,rep (2 ,50) )
dat=data.frame(x=x,y=as.factor (y))
plot(x, col =y)

str(dat)

fit_extra = svm(factor(y) ~ ., data = dat, scale = FALSE, kernel = "radial", cost = 30)
fit_extra
accuracy_values


#========================================================
#             GRAFICA
#========================================================
xgrid = expand.grid(x.1 = px1, x.2 = px2)
ygrid = predict(fit_extra, xgrid)
plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = .2)
points(x, col = y + 1, pch = 19)
func = predict(fit_extra, xgrid, decision.values = TRUE)
func = attributes(func)$decision
contour(px1, px2, matrix(func, 69, 99), level = 0, add = TRUE)
contour(px1, px2, matrix(func, 69, 99), level = 0.5, add = TRUE, col = "blue", lwd = 2)







cost_values <- c(1, seq(from = 5, to = 40, by = 5))
accuracy_values <- sapply(cost_values, function(costo) {
  set.seed(12345) 
  modelo <- svm(factor(y) ~ ., data = dat, scale = FALSE, kernel = "radial", cost = costo)
  predicciones <- predict(modelo, dat)
  aciertos <- ifelse(predicciones == dat$y, 1, 0)
  precision <- sum(aciertos) / nrow(dat) 
  return (precision) 
})
plot(cost_values, accuracy_values, type = "b")





