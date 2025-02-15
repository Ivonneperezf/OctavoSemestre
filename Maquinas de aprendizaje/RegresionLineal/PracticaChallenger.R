launch<- read.csv("challenger.csv")
b <- cov(launch$temperature, launch$distress_ct)/var(launch$temperature)
b
a <- mean(launch$distress_ct) - b * mean(launch$temperature)
a
r <- cov(launch$temperature, launch$distress_ct)/
  (sd(launch$temperature) * sd(launch$distress_ct))
r
cor(launch$temperature, launch$distress_ct)


reg <- function(y, x) {
  x <- as.matrix(x)
  x <- cbind(Intercept = 1, x)
  b <- solve(t(x) %*% x) %*% t(x) %*% y
  colnames(b) <- "estimate"
  print(b)
}

str(launch)
N
reg(y = launch$distress_ct, x = launch[2])

reg(y = launch$distress_ct, x = launch[2:4])





