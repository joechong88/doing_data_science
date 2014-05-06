#*********************************************************************************************************
#
# Chapter 03 - Algorithms
#
#*********************************************************************************************************

# Linear Regression

# Exercise
# From a normal distribution, simulate 1000 values with mean of 5 and sd of 7
x_1 <- rnorm(1000, 5, 7)

# plot p(x)
hist(x_1, col="grey")

true_error <- rnorm(1000, 0, 2)
true_beta_0 <- 1.1
true_beta_1 <- -8.2

y <- true_beta_0 + true_beta_1*x_1 + true_error
hist(y) # plot p(y)
plot(x_1, y, pch=20, col="red") # plot p(x, y)

model <- lm(y ~ x_1) # build regression model
coefs <- coef(model) # check the coefficients
plot(x_1, y, pch=20, col="red") # plot p(x, y)
abline(coefs[1], coefs[2]) # plot the absolute line of the coefficients
summary(model)

############################################################################################
# k-Nearest Neighbours (k-NN)
############################################################################################

###### Credit Scores Example ######
# read the data points

