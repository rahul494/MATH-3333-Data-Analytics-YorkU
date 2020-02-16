################################################################################################
# Name: Rahul Sharma
# ID: 214393508
# Assignment: 2
# Question: 1
################################################################################################

library(lars)

day <- read.csv("C:/Users/Rahul/Downloads/Bike-Sharing-Dataset/day.csv")

# a) Use the LASSO package to analyze the data with a collection of predictors and provide
# the summary of the output of the command "lars".

# b) Based on the output of "lars", please provide the sequence of candidate models.

y <- day$registered
drop <- c("registered", "dteday")
x_list <- day[,!(names(day) %in% drop)]
x <- matrix(unlist(x_list), ncol = 14, byrow = TRUE)
lasso <- lars(x=x, y=y, trace=TRUE)
summary(lasso)

# c) Use the cross validation method, select the best value for the fraction s based on the
# plot of cross validation error againt the fraction s. The fraction s measures the ratio of the
# L1 norm of the penalized estimate over the L1 norm of the regular penalized estimate.

cv.lars(x=x, y=y, K=10)

# d) Use the optimum s you select, perform the penalized regression and output the optimum model 
# and the estimated coefficients.

predict(lasso, x, s=1/2, mode="fraction")
coef(lasso, s=1/2, mode="fraction")
