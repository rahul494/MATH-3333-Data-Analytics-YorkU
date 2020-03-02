################################################################################################
# Name: Rahul Sharma
# ID: 214393508
# Assignment: 2
# Question: 5
################################################################################################

# a) Perform the logistic regression on the dataset. You can some of the predictors in the
# model. Please use 900 observations as the training set and use your model to predict the
# default status of the remaining 100 loans.

credit <- read.csv("C:/Users/Rahul/Downloads/credit.csv")

# use 90% of the dataset to be used for training
sample_size = round(nrow(credit)*.90)
index <- sample(seq_len(nrow(credit)), size = sample_size)

credit.train <- credit[index, ]
credit.test <- credit[-index, ]
ynew <- credit$credit.rating[-index]

cred.glm = glm(credit.rating ~., family=binomial, data=credit.train)
summary(cred.glm)

ptest <- predict(cred.glm, newdata = credit.test, type="response")
data.frame(ynew, ptest)
gg1 = floor(ptest + (5/6))
ttt = table(ynew, gg1)
ttt
error = (ttt[1,2] + ttt[2,1])/100
error

# b) Please investigate how the false negative number and false positive number change
# with respect to the different cutoff value of probability.

gg1 = floor(ptest + (1/2))
ttt = table(ynew, gg1)
ttt
error = (ttt[1,2] + ttt[2,1])/100
error
