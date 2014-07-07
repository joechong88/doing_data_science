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
# read the data points from Excel randomizer
creditscore <- read.xls("creditscore.xlsx")
plot(creditscore$age, creditscore$income, main="Scatterplot", xlab="age", ylab="income", pch=20)
scatterplot(income ~ age | credit, data=creditscore, 
            xlab="age", ylab="income", main="Enhanced Scatterplot", pch=20)

# PREPARATION OF TRAINING AND TEST SETS

n.points <- 1000 # number of rows in the dataset
sampling.rate <- 0.8

# need the number of points in the test set to calculate the misclassification rate
num.test.set.labels <- n.points * (1 - sampling.rate)

# randomly sample which rows will go in the training set
training <- sample(1:n.points, sampling.rate*n.points, replace=FALSE)

# define the training set to be those rows
train <- subset(creditscore[training, ], select=c(age, income))

# the other rows are going into the test set
testing <- setdiff(1:n.points, training)
test <- subset(creditscore[testing, ], select=c(age, income))

# this is the subset of labels for the training set
c1 <- creditscore$credit[training]
# subset of labels for the test set that we're witholding these
true.labels <- creditscore$credit[testing]

# PICK AN EVALUATION METRIC
# Not an easy thing or universal thing to do. Need to work with domain expert. 
require(class)
library(class)
knn(train, test, c1, k=3)

# the other way to find out which k is correct is to loop through and see what the 
# misclassifcation rate is for different values of k
for (k in 1:20)
{
  print(k)
  predicted.labels <- knn(train, test, c1, k)
  num.incorrect.labels <- sum(predicted.labels != true.labels)
  misclassification.rate <- num.incorrect.labels / num.test.set.labels
  print(misclassification.rate) 
}

# choose the k value with the lowest mis-classification rate
test <- c(57, 37)

#*********************************************************************************************************
# Linear Regression on the housing dataset
#*********************************************************************************************************
