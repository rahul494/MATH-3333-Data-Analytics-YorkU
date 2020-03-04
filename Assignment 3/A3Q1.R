################################################################################################
# Name: Rahul Sharma
# ID: 214393508
# Assignment: 3
# Question: 1
################################################################################################

c1 <- rbind(c(2, 3), c(3, 5), c(4, 4), c(5, 6), c(6, 6))
c2 <- rbind(c(2, 0), c(3, 2), c(4, 2), c(4, 3), c(6, 4), c(7, 6))

# a) Comute the mean of the first class µ1, and the mean of the second class µ2

# initialize 1x2 matrices
µ1 <- matrix(c(0, 0), nrow=2, ncol=1,byrow = TRUE)
µ2 <- matrix(c(0, 0), nrow=2, ncol=1,byrow = TRUE) 


# find mean values for Class 1
for(i in 1:length(c1[,1])){
  µ1[1,1] = µ1[1,1] + c1[i,1]
  µ1[2,1] = µ1[2,1] + c1[i,2]
}

µ1 <- µ1 / length(c1[,1])

# find mean values for Class 2
for(i in 1:length(c2[,1])){
  µ2[1,1] = µ2[1,1] + c2[i,1]
  µ2[2,1] = µ2[2,1] + c2[i,2]
}

µ2 <- µ2 / length(c2[,1])

# b) Compute the within class variation Sw = S1 + S2, where S1 and S2 are the variations
# within C1 and C2, respectively.  

s1 <- matrix(c(var(c1[,1]), 
                       cov(c1[,1], c1[,2]),
                       cov(c1[,2], c1[,1]),
                       var(c1[,2])
                      ), nrow=2, ncol=2,byrow = TRUE)*(length(c1[,1])-1)

s2 <- matrix(c(var(c2[,1]), 
                       cov(c2[,1], c2[,2]),
                       cov(c2[,2], c2[,1]),
                       var(c2[,2])
                      ), nrow=2, ncol=2,byrow = TRUE)*(length(c2[,1])-1)

sw <- s1 + s2

# c) Find the optimum projection v which can lead to the maximum seperation of the 
# projected observations

# v = sw-1(µ1 - µ2)
v <- solve(sw)%*%(µ1 - µ2)
v

# d) Find the cutoff point

cutoff <- ((1/2)*t(v)%*%µ1) + ((1/2)*t(v)%*%µ2)
cutoff

# e) Given a new observation (5.5, 5.5), which class does it belong to?

# according to our cutoff value, the new observation belong to 
# Class 1 if vT*x > -0.076
# Class 2 if vT*x < -0.076

x <- c(5.5, 5.5)
t(v)%*%x

# the new is below the cutoff point (0.1534058) and therefore in Class 1
