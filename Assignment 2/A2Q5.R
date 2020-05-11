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

# create factors for qualitative variables

credit$credit.rating <- as.factor(credit$credit.rating)
credit$account.balance <- as.factor(credit$account.balance)
credit$previous.credit.payment.status <- as.factor(credit$previous.credit.payment.status)
credit$credit.purpose <- as.factor(credit$credit.purpose)
credit$savings <- as.factor(credit$savings)
credit$employment.duration <- as.factor(credit$employment.duration)
credit$marital.status <- as.factor(credit$marital.status)
credit$guarantor <- as.factor(credit$guarantor)
credit$current.assets <- as.factor(credit$current.assets)
credit$other.credits <- as.factor(credit$other.credits)
credit$apartment.type <- as.factor(credit$apartment.type)
credit$occupation <- as.factor(credit$occupation)
credit$foreign.worker <- as.factor(credit$foreign.worker)

# drop the variables that are not significant 
# this includes telephone and dependents

credit <- subset(credit, select=-c(telephone, dependents))

Xcred <- model.matrix(credit.rating~., data = credit)[,-1]
train <- sample(1:1000, 900)
xtrain <- Xcred[train,]
xtest <- Xcred[-train,]
ytrain <- credit$credit.rating[train]
ytest <- credit$credit.rating[-train]

train.frame <- data.frame(ytrain, xtrain)
test.frame <- data.frame(ytest, xtest)

cred.glm = glm(ytrain~., family = binomial, data = data.frame(ytrain, xtrain))

summary(cred.glm)

ptest <- predict(cred.glm, newdata = test.frame, type="response")
data.frame(test.frame$ytest, ptest)
gg1 = floor(ptest + (5/6))
ttt = table(test.frame$ytest, gg1)
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
