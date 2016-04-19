getwd()
data_temo <- read.csv("TEMCO.CSV")
sub = data_temo[7]
sales = (sub[] == "Sales")
Eng = (sub[] == "Engineer")
Adv = (sub[] == "Purchase")
Purchase = (sub[] == "Purchase")

new_data = cbind(log(data_temo[,1]),data_temo[,2],data_temo[,3],data_temo[,4],data_temo[,8],sales,Eng,Adv,Purchase)
new_data <- data.frame(new_data)
new_data <- setNames(new_data,c("Salary","YrsEm","PriorYr","Education","Super","Sales","Engineer","Adv","Purchase"))
sub <- data_temo[6]

male <- sub[] == "Male"
female <- sub[] == "Female"
new_data <- cbind(new_data, female, male)
names(new_data)[10] <- paste("Female")
names(new_data)[11] <- paste("Male")
new_data$Female <- new_data$Female * 1
new_data$Male <- new_data$Male * 1
fit_temo <- lm(Salary ~ YrsEm + PriorYr + Education + Super + Sales + Engineer + Adv + Female,new_data)
summary(fit_temo)

new_data$Salary <- log10(data_temo$Salary)
fit_temo <- lm(Salary ~ YrsEm + PriorYr + Education + Super + Sales + Engineer + Adv + Female,new_data)
summary(fit_temo)
anova(fit_temo)
fit_temo <- lm(Salary ~ YrsEm + PriorYr + Education + Super + Purchase + Engineer + Adv + Male,new_data)
summary(fit_temo)

