X <- matrix(1, nrow = 51, nocl = 1)
tX <- t(X)
rX <- solve(X)
XX <- tX %*% X
H <- X %*% solve(XX) %*% tX
hatvalues(fit_business)
names(s) <- c("Salary","YrsEm","PriorYr","Education","Super")
plot(salary_fit1,which = 2)