# load data and data pre_process
getwd()
setwd("/Users/Marcus/Desktop")
pre_data <- read.csv("Pregnancy.csv")
evenrow <- seq(2,nrow(pre_data),2)
oddrow <- seq(1,nrow(pre_data),2)
train <- pre_data[oddrow,]
test <- pre_data[evenrow,]
age_1 <- pre_data$Age == 1
age_3 <- pre_data$Age == 3
age_1 <- age_1 * 1
age_3 <- age_3 * 1
pre <- cbind(pre_data,age_1,age_2,age_3)
# nominal logistic regression
library(nnet)
fit_1 <- multinom(Duration~.,data = train)
testpredict <- predict(fit_1,newdata = test)
summary(testpredict)
tab = table(test$Duration,testpredict)
tab
CCR = sum(diag(tab))/sum(tab)
CCR
# ordinal logistic regression
library('ordinal')
train$Duration.ordered = ordered(train$Duration, levels = c(1,2,3),labels=c(1,2,3))
train$Duration.ordered = as.ordered(train$Duration)
fit2 <- clm(Duration.ordered ~ Nutrition + Alcohol + Smoking + age_1 + age_3,data = train)
test_predict <- predict(fit2,newdata = test)
n = dim(train)[1];
Y.hat.2 = rep(0,n);
Y.prob.2 = test_predict$fit;
for(i in 1:n){if(max(Y.prob.2[i,]) == Y.prob.2[i,1]){Y.hat.2[i]=1;}else if(max(Y.prob.2[i,]) == Y.prob.2[i,2]){Y.hat.2[i]=2;}else if(max(Y.prob.2[i,]) == Y.prob.2[i,3]){Y.hat.2[i]=3;}}
ctable2 = table(MBA$admit, Y.hat.2);
ctable2;
correct.rate2 = sum(diag(ctable2)[1:3])/n;
correct.rate2