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
//centering
data_1 <- data.frame(scale(data,center = TRUE, scale = FALSE))
vif(fit_ace)

//ridge and lasso regression
x = model.matrix(y~.,data_)
x = as.matrix(x)
ridge_fit = glmnet(x,y,alpha = 0)
lasso_fit = glmnet(x,y,alpha = 1)
plot(ridge_fit,xvar = "lambda", label = TRUE)
plot(lasso_fit,xvar = "lambda", label = TRUE)
ridgecv=cv.glmnet(x,y,alpha = 0,nfold = 3)
lassocv=cv.glmnet(x,y,alpha = 1,nfold = 3)
