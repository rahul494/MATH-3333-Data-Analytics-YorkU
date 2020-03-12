################################################################################################
# Name: Rahul Sharma
# ID: 214393508
# Assignment: 3
# Question: 4
################################################################################################

library(MASS)

credit <- read.csv("C:/Users/Rahul/Downloads/credit.csv")

n =1000 # number of rows
nt=800 # number of rows to be used for training
neval=n-nt # remaining rows used for testing
rep=5 # repeat cross-validation 5 times
errlin=dim(rep)

# find the misclasification for each fold in order to find the average misclassication error.
for (k in 1:rep) {
  train=sample(1:n,nt)
  m1=lda(credit.rating~.,credit[train,])
  predict(m1,credit[-train,])$class
  tablin=table(credit$credit.rating[-train],predict(m1,credit[-train,])$class)
  errlin[k]=(neval-sum(diag(tablin)))/neval
}

mean(errlin)
