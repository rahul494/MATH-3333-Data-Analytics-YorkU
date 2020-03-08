################################################################################################
# Name: Rahul Sharma
# ID: 214393508
# Assignment: 3
# Question: 4
################################################################################################

library(MASS)

credit <- read.csv("C:/Users/Rahul/Downloads/credit.csv")

# use 80% of the dataset to be used for training
sample_size = round(nrow(credit)*.80)
index <- sample(seq_len(nrow(credit)), size = sample_size)

credit.train <- credit[index, ]
credit.test <- credit[-index, ]
ynew <- credit$credit.rating[-index]

cred.lda = lda(credit.rating ~., data=credit.train)
cred.lda

predict(cred.lda,newdata=credit.test)
