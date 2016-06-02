#7.3
getwd()
setwd(/Users/Marcus/Desktop)
iris <- read.csv("Iris Data Set.csv")
library(MASS)
fit = lda(Species_No ~ Petal_width + Petal_length + Sepal_width + Sepal_length,data = iris, prior = c(1,1,1)/3)
fit
predict(fit,newdata = data.frame(Sepal_length = 5.5,Sepal_width = 3.0,Petal_length = 4.0,Petal_width = 1.5))

#8.3
dis <- read.csv("Workbook1.csv")
fit1 <- glm(y ~ x1 + x2 ,data = dis, family = poisson())
fit2 <- lm(y ~ x1 + x2 ,data = dis)
fit3 <- lm(sqrt(y) ~ x1 + x2 ,data = dis)
summary(fit1)
summary(fit2)
summary(fit3)