setwd("~/ds_project/datsets")
data_1 <- read.csv("~/ds_project/datsets/Dataset_1.csv")
print(data_1)

nrow(data_1)
ncol(data_1)
dim(data_1)
length(data_1)

names(data_1)

str(data_1)

get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

loan_mean <- mean(data_1$Loan)
print(loan_mean)
loan_median <- median(data_1$Loan)
print(loan_median)
loan_mode <- get_mode(data_1$Loan)
print(loan_mode)

intRate_mean <- mean(data_1$Interest_rate)
print(intRate_mean)
intRate_median <- median(data_1$Interest_rate)
print(intRate_median)
intRate_mode <- get_mode(data_1$Interest_rate)
print(intRate_mode)

loan_range <- range(data_1$Loan)
print(loan_range)
loan_std <- sd(data_1$Loan)
print(loan_std)

intRate_range <- range(data_1$Interest_rate)
print(intRate_range)
intRate_std <- sd(data_1$Interest_rate)
print(intRate_std)

crdSc_mode <- get_mode(data_1$Credit_Score)
print(crdSc_mode)
