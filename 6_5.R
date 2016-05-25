getwd()
setwd("/Users/Marcus/Desktop")
m_data <- read.csv("Mammography.csv")
library(mlogit)
library(nnet)
evenrow <- seq(2,nrow(m_data),2)
oddrow <- seq(1,nrow(m_data),2)
train <- m_data[oddrow,]
test <- m_data[evenrow,]
#train_data<-mlogit.data(data = train, choice = "ME",shape = "wide",varying = NULL)
#fit1 = mlogit(ME ~ 0 | PB + HIST,data = train_data,reflevel = 3)
fit1=multinom(ME~PB + HIST, data = train)
summary(fit1)
#test_data <- mlogit.data(data = test, choice = "ME",shape = "wide",varying = NULL)
Y.prob.1 =predict(fit1,newdata = test)
n = dim(test)[1];
Y.hat.1 = rep(0,n);
for(i in 1:n){if(max(Y.prob.1[i,]) == Y.prob.1[i,1]){Y.hat.1[i]=2;}else if(max(Y.prob.1[i,]) == Y.prob.1[i,2]){Y.hat.1[i]=0;}else if(max(Y.prob.1[i,]) == Y.prob.1[i,3]){Y.hat.1[i]=1;}}
prediction = Y.hat.1;
ctable1 = table(test$ME, prediction);
ctable1;
correct.rate1 = sum(diag(ctable1)[1:3])/n;
correct.rate1
# Ordinal logistic regression
library(ordinal)
m_data <- read.csv("Mammography.csv")
evenrow <- seq(2,nrow(m_data),2)
oddrow <- seq(1,nrow(m_data),2)
train <- m_data[oddrow,]
test <- m_data[evenrow,]
train$ME.ordered = ordered(train$ME,levels=c(0,1,2),labels=c(0,2,1))
train$ME.ordered = as.ordered(train$ME)
fit2 = clm(ME.ordered ~ PB + HIST,data = train)
summary(fit2)
test_predict = predict(fit2,newdata = test)
n = dim(train)[1];
Y.hat.2 = rep(0,n);
Y.prob.2 = test_predict$fit;
for(i in 1:n){if(max(Y.prob.2[i,]) == Y.prob.2[i,1]){Y.hat.2[i]=0;}else if(max(Y.prob.2[i,]) == Y.prob.2[i,2]){Y.hat.2[i]=1;}else if(max(Y.prob.2[i,]) == Y.prob.2[i,3]){Y.hat.2[i]=2;}}
ctable2 = table(test$ME, Y.hat.2);
ctable2;
correct.rate2 = sum(diag(ctable2)[1:3])/n;
correct.rate2