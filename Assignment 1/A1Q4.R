################################################################################################
# Name: Rahul Sharma
# ID: 214393508
# Assignment: 1
# Question: 4
################################################################################################

day <- read.csv("C:/Users/Rahul/Downloads/day.csv")

# a) Using R to partition the bike sharing data into two equal parts. Run the regression 
# model using ”registered” as the response and ”temp” and ”hum” only using the first half of the data.

day.1half <- head(day, nrow(day)/2)

lm.1half = lm(formula = registered ~ temp + hum, day.1half)
summary(lm.1half)

predict <- function(temp, hum){
  reg = 1181.3 + 3972.2*temp -1579.1*hum
  return (reg)
}

# b) Apply the predictive function on each of the observations in the second half of the data.

day.2half <- tail(day, nrow(day)/2)

sse = 0
for(i in 1:nrow(day.2half)) {
  sse = sse + (day.2half$registered[i] - predict(day.2half$temp[i], day.2half$hum[i]))^2
}
print(sse)
 
# c) Repeat part (a) and part (b) using the regression model you proposed in Question 2.b.

reg.lm = lm(formula = registered ~ temp + hum + season + yr + windspeed + casual, day.1half)
summary(reg.lm)

predict <- function(temp, hum){
  reg = 1616.91583 + 3504.17705*temp -1452.01927*hum
  return (reg)
}

sse = 0
for(i in 1:nrow(day.2half)) {
  sse = sse + (day.2half$registered[i] - predict(day.2half$temp[i], day.2half$hum[i]))^2
}

print(sse)
