################################################################################################
# Name: Rahul Sharma
# ID: 214393508
# Assignment: 1
# Question: 3
################################################################################################

# a) Formulate the response vector Y, which has nine entries.

Y = c(65, 76, 85, 106, 119, 129, 142, 144, 151)

# b) Formulate the data matrix of X, the first column should be all ones corresponding to
# the intercept, and the second column should be the predictors. The dimension of X should
# be 9 × 2.

X = rbind(c(1,23),c(1,26),c(1,30),c(1,34),c(1,43),c(1,48),c(1,52),c(1,57), c(1,58))

# c) Write R code to compute XtX.

t(X) %*% X

# e) Write R code to compute (XtX)-1

solve(t(X) %*% X)

# g) Write R code to compute θ = ((XtX)−1)XtY. This is the estimated linear regression
# coefficient of the linear model with Y as the response and X as the data matrix.


solve(t(X) %*% X)%*%(t(X)%*%Y)
