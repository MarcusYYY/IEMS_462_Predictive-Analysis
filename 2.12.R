
> setwd("/Users/Marcus/Desktop")
> data_ibm = read.csv("IBM.csv")
> data_apple = read.csv("Apple.csv")
> data_500 = read.csv("S&P500.csv")
> source("Compute_Return.R")
> ibm_adj <- data_ibm[,7]
> apple_adj <- data_apple[,7]
> five_adj <- data_500[,7]
> return_ibm = com_Return(ibm_adj,1,121)
> return_apple = com_Return(apple_adj,1,121)
> return_five = com_Return(five_adj,1,121)
> outcome_ibm = cbind(return_five,return_ibm)
> View(outcome_ibm)
> outcome_apple = cbind(return_five,return_ibm)
> outcome_apple = cbind(return_five,return_apple)
> outcome_ibm <- data.frame(outcome_ibm)
> outcome_apple <- data.frame(outcome_apple)
> fit_apple = lm(return_five~return_apple,outcome_apple)
> fit_ibm = lm(return_five~return_ibm,outcome_ibm)
> summary(fit_apple)