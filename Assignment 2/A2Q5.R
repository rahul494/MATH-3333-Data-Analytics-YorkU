################################################################################################
# Name: Rahul Sharma
# ID: 214393508
# Assignment: 2
# Question: 5
################################################################################################

# a) Perform the logistic regression on the dataset. You can some of the predictors in the
# model. Please use 900 observations as the training set and use your model to predict the
# default status of the remaining 100 loans.

credit <- read.csv("C:/Users/Rahul/Downloads/german_credit_dataset.csv")

sample_size = round(nrow(credit)*.90)
index <- sample(seq_len(nrow(credit)), size = sample_size)

credit.train <- credit[index, ]
credit.test <- credit[-index, ]

# b) Please investigate how the false negative number and false positive number change
# with respect to the different cutoff value of probability.
