# ch. 3 linear regression

# load needed libraries
library(MASS)
# if you have an error, install the package by typing 
# install.packages("MASS")
library(ISLR)
# if you have an error, install the package by typing 
# install.packages("ISLR")

# clear workspace
rm (list=ls())
# see what we have 
#ls()

###############################################################################
## 3.6.2 Simple linear regression
###############################################################################

# let's take a look at Boston dataset
#fix(Boston)
?Boston
head(Boston)
names(Boston)


# fit a simple linear regression model
lm.fit=lm(medv ~ lstat ,data=Boston )
# basic info
lm.fit
# more detail info
summary(lm.fit)
# see the estimated coefficients 
coef(lm.fit)
# obtain a confidence interval
confint(lm.fit)
# predict medv for a given value of lstat
predict(lm.fit, data.frame(lstat=c(5,10,15) ), interval="confidence") # 95% confidence interval
predict(lm.fit, data.frame(lstat=c(5,10,15) ), interval="prediction") # 95% prediction interval
# plot 
# 1) scatter plot
plot(Boston$lstat, Boston$medv)
#plot(Boston$lstat,Boston$medv,pch="+")
#abline(lm.fit)
# 2) add fitted line
abline(lm.fit,lwd=3,col="red")   # thicker, red line
#plot(Boston$lstat,Boston$medv,pch=20)


# plot results automatically 
plot(lm.fit)

# view all four plots together.
par(mfrow=c(2,2))
plot(lm.fit)

# Q. how to cancel par?
# A. Just close the plot

# compute the residuals and plot
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

# leverage statistics
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit)) # identify the index of max

###############################################################################
## 3.6.3 Multiple linear regression
###############################################################################

# use three variables 
lm.fit=lm(medv~lstat+age ,data=Boston )
summary(lm.fit)
# use all the variables 
lm.fit=lm(medv~. ,data=Boston )
summary(lm.fit)

# compute variance inflation factors (VIF)
#install.packages("car") # may require a newer version of R
library(car)
vif(lm.fit)

# let's remove a predictor
lm.fit1=lm(medv~. -age, data=Boston)
summary(lm.fit1)
# alternatively, use the update function
lm.fit1=update(lm.fit, ~.-age)
summary(lm.fit1)

###############################################################################
## 3.6.4 Interaction terms
###############################################################################
lm.fit.interaction <- lm(medv~lstat*age, data=Boston)
#lm.fit.interaction <- lm(medv~lstat + age + lstat:age, data=Boston) # same thing!
summary(lm.fit.interaction)


###############################################################################
## 3.6.5 Non-linear Transformations of the Predictors
###############################################################################
lm.fit=lm(medv~lstat, data=Boston)
summary(lm.fit)
lm.fit2=lm(medv~lstat +I(lstat ^2), data=Boston)
summary(lm.fit2)
anova(lm.fit,lm.fit2)

# plot
par(mfrow=c(2,2))
plot(lm.fit2)

## a fifth-order polynomial fit:
lm.fit5=lm(medv~poly(lstat ,5), data=Boston)
summary(lm.fit5)

## log 
summary(lm(medv~log(rm),data=Boston ))

###############################################################################
## 3.6.6 Qualitative Predictors
###############################################################################
fix(Carseats)
names(Carseats)

# fit with interaction terms 
lm.fit=lm(Sales~ . +Income:Advertising +Price:Age, data=Carseats)
summary(lm.fit)
attach(Carseats)
contrasts(ShelveLoc)

###############################################################################
## 3.6.7 Writing Functions
###############################################################################
# some exercises ...