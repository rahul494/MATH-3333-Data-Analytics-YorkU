################################################################################################
# Name: Rahul Sharma
# ID: 214393508
# Assignment: 1
# Question: 1
################################################################################################

library(lattice)

day <- read.csv("C:/Users/Rahul/Downloads/Bike-Sharing-Dataset/day.csv")

# a)  The variable "registered" records the number of registered users used the bike sharing
# service on a particular day. Please provide the mean value of the variable "registered" for
# each day of the week.

regs.grp.mean <- aggregate(registered~weekday,day,mean)
regs.grp.mean


# b) Plot the conditional density plot of the variable "registered" conditional on each day
# of the weak.

densityplot(~registered,groups=weekday,
            data=day, 
            plot.points=FALSE, 
            auto.key=list(space = "right"), 
            xlab="# of Registered Users", 
            main="Registered Users in a Given Day")

# c) Produce a two-dimensional levelplot of the variable "registered"
# against the combination of temperature (variable "temp") and humidity (variable "hum").

reg.temp.hum.mean =tapply(day$registered,INDEX=list(cut(day$hum,breaks=10), cut(day$temp,breaks=10)),FUN=mean,na.rm=TRUE)
levelplot(reg.temp.hum.mean, scales = list(x = list(rot = 90)), xlab="Humidity", ylab = "Tempurature")
