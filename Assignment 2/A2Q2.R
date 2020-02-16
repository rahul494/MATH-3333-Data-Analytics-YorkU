################################################################################################
# Name: Rahul Sharma
# ID: 214393508
# Assignment: 2
# Question: 2
################################################################################################

# a) In assignment one, we have computed the least square regresson for this data set and we
# have the estimated linear model.

Y = c(65, 76, 85, 106, 119, 129, 142, 144, 151)
X = rbind(c(1,23),c(1,26),c(1,30),c(1,34),c(1,43),c(1,48),c(1,52),c(1,57), c(1,58))

constants = solve(t(X)%*%X)%*%(t(X)%*%Y)
theta0 = constants[1,1]
theta1 = constants[2,1]

# according to our estimated model, we have a y-intercept of 16.622585 and slope of 2.337997

update.est.model <- function(sales, advert){
  # find error on future/newly added observation  
  error = sales - (theta0 + theta1*advert)
  error.now = -2*error*c(1, advert)
  
  # recompute our coefficients with a learning rate of 0.0001
  theta0 <<- theta0 - 0.0001 * error.now[1]
  theta1 <<- theta1 - 0.0001 * error.now[2]
}

update.est.model(154, 61)
update.est.model(157, 63)

# b) Based on the original 9 observation data and perform a ridge regression. Program it
# with R. Output the ridge regression results at a few different values of λ.

# to create our ridge regression, we can modify the least square solution to consider the weighted
# factor. Thus we compute θ = ((XtX + λI)-1)XTY.

LI = diag(2)*0.0001
ridge = solve((t(X)%*%X) + LI)%*%(t(X)%*%Y)
ridge

# c) In your ridge regression, when λ increaes, what do you observe from the values of the estimated 
# coefficients. Does any of the estimated coefficients shrink to zero like the L1 LASSO regression?

LI = diag(2)*0.1
ridge = solve((t(X)%*%X) + LI)%*%(t(X)%*%Y)
ridge

LI = diag(2)*1
ridge = solve((t(X)%*%X) + LI)%*%(t(X)%*%Y)
ridge

LI = diag(2)*10
ridge = solve((t(X)%*%X) + LI)%*%(t(X)%*%Y)
ridge

# Yes, we slowly begin to see our value for the coefficient theta0 shinks to 0
