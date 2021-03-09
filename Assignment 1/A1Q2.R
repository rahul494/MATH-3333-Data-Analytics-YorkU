################################################################################################
# Name: Rahul Sharma
# ID: 214393508
# Assignment: 1
# Question: 2
################################################################################################

day <- read.csv("C:/Users/Rahul/Downloads/Bike-Sharing-Dataset/day.csv")

# a) Provide the summary result of the regression model with "registered" as the response
# variable and "temp", "hum" as the predictors. You can copy and paste the regression
# result from the R output

lm = lm(formula = registered ~ temp + hum, day)
summary(lm)

# b) What other predictors do you think might be important for the modelling of the
# variable "registered"? Please construct another linear model including more predictors
# and provide the summary result of the second model.

# parameters exluded: instant, holiday, mnth, weekday, workingday, atemp, cnt
#
# reason: adding these parameters into our model results in either high multicolinearity
# or does not a significant independent variable

reg.lm = lm(formula = registered ~ temp + hum + season + yr + windspeed + casual, day)
summary(reg.lm.all)

# c. Use adjusted R-square to determine which model is a better model.

summary(lm)
summary(reg.lm)

# simply looking at the adjusted R-squared values of both models, it appears that the second model
# created in part b) is the better model
