fit_beam <- lm(wood_beam$Strength~wood_beam$Specific.Gravity + wood_beam$Moisture)
hatvalues(fit_beam)
influence(fit_beam)// include hatvalues
cooks.distance(fit_beam)
plot(fit_beam ,which = c(4,6))
summary(fit_beam)
wood_beam <- wood_beam[-c(1,4),]

depe_data <- read.csv("MULTDEPEND DATA.CSV")
cor(depe_data)
x <- depe_data[,1:4]
cor(x)
fit_depend <- lm(y~x1 + x2 + x3 + x4,depe_data)
vif(fit_depend)

ace_data <- read.csv("acetylene data set.csv")
x <- ace_data[,1:3]
plot(x)
cor(x)
fit_ace <- lm(y ~ x1 + x2 + x3,ace_data)
vif(fit_ace)

//ridge regression
library(MASS)
lridge <- ridge(y,x,lambda = seq(from = 0, to = 10, by = 0.001))
traceplot(lridge)
lridge <- ridge(y,x,lambda = 2)
summary(lridge)

traceplot(lridge,pch = 20, xlab = "Ridge parameter k",ylab = "cofficient")

//lasso regression
library("glmnet")
fit.lasso = glmnet(x,y,alpha = 1)
plot(fit.lasso,xvar = "lambda", label = TRUE)
fit.lasso = glmnet(x,y,alpha = 1,lambda = 0.6)
fit.lasso
coef(fit.lasso)