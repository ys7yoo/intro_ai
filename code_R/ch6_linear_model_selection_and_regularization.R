###############################################################################
## 6.5 Subset Selection Methods
###############################################################################

# Let's predict a baseball player`s Salary
library(ISLR)
#fix(Hitters)
names(Hitters)
dim(Hitters) # 322 x 20
sum(is.na(Hitters$Salary))  # there are some missing data in 59 observations
Hitters=na.omit(Hitters)    # removes rows that have missing values in any variable.
dim(Hitters) # 263 x 20
sum(is.na(Hitters))



## 6.5.1 Best Subset Selection

library(leaps)
# Model selection by exhaustive search
regfit.full=regsubsets(Salary~., Hitters)  
summary(regfit.full)  # by default, best up to eight-variable model are compared

# to get more combinations, use nvmax argument
regfit.full=regsubsets(Salary~., data=Hitters, nvmax=19)
summary(regfit.full)

# check Rsquare
reg.summary=summary(regfit.full)
names(reg.summary)
reg.summary$rsq    
plot(reg.summary$rsq,type='o')

# plot others
par(mfrow=c(2,2))
# R2
plot(reg.summary$rss, xlab="Number of Variables", ylab="RSS", type="l")
# adjusted R2
plot(reg.summary$adjr2, xlab="Number of Variables", ylab="Adjusted RSq", type="l")
imax<-which.max(reg.summary$adjr2)
points(imax,reg.summary$adjr2[imax], col="red", cex=2, pch=20)
# Cp
plot(reg.summary$cp, xlab="Number of Variables", ylab="Cp", type="l")
imin<-which.min(reg.summary$cp)
points(imax,reg.summary$cp[imin], col="red", cex=2, pch=20)
# BIC
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC", type="l")
imin<-which.min(reg.summary$bic)
points(imin,reg.summary$bic[imin],col="red",cex=2,pch=20)


# use built in plot
# ?plot.regsubsets.
# a black square - the selected variable
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")

# to see the coef for a model
coef(regfit.full,6)



## 6.5.2 Forward and Backward Stepwise Selection
regfit.fwd=regsubsets(Salary~., data=Hitters, nvmax=19, method="forward")
summary(regfit.fwd)
regfit.bwd=regsubsets(Salary~., data=Hitters, nvmax =19, method="backward")
summary(regfit.bwd)

# compare
coef(regfit.full,7)
coef(regfit.fwd,7)
coef(regfit.bwd,7)




###############################################################################
## 6.6 Ridge Regression and the Lasso
###############################################################################

# Make sure you installed "glmnet" package
# install.packages("glmnet")


# prepare x and y 
x=model.matrix(Salary~.,Hitters )[,-1]  # create a matrix!
dim(x)
# 263  19
y=Hitters$Salary

## 6.6.1 Ridge Regression (set alpha=0)
library(glmnet)
grid=10^seq(10,-2,length=100)  # prepare 100 lambdas from 10^10 to 10^-2
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod))
# 20 rows (one for each predictor, plus an intercept) 
# 100 columns (one for each value of λ).

# result for a large lambda (coefficients are suppressed toward zero!)
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2)) # L2 norm is small
# 6.36

# result for a small lambda
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2)) # L2 norm is large
# 57.11

# get coeff for a new regression coefficients with a value of lambda, 50
predict(ridge.mod,s=50,type="coefficients")[1:20 ,]

# divide data into train and test sets
set.seed(1)
idx.train=sample(1:nrow(x), nrow(x)/2)
idx.test=(-idx.train)
y.test=y[idx.test]

# 
ridge.mod=glmnet(x[idx.train,], y[idx.train], alpha=0, lambda=grid, thresh=1e-12)

# set lambda=0 -> least square
ridge.pred=predict(ridge.mod, s=0, newx=x[idx.test,], exact=T)
mean((ridge.pred-y.test)^2)
# 114783.1

# refit with lambda= 4
ridge.pred=predict(ridge.mod,s=4,newx=x[idx.test,])  # predict for test data
mean((ridge.pred-y.test)^2)
# 101036.8

# refit with a very large lambda
ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)
# 193253.1

# This is the same as the null model
mean((mean(y[idx.train])-y.test)^2)
# 193253.1



# use cross-validation to choose the tuning parameter λ.
# Use built-in cross-validation function, cv.glmnet().
set.seed(1)
cv.out=cv.glmnet(x[idx.train,], y[idx.train], alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
# 211.7416

# the test MSE for the best lambda is 
ridge.pred=predict(ridge.mod, s=bestlam, newx=x[idx.test,])
mean((ridge.pred-y.test)^2)
# 96015.51

# refit our ridge regression model on the full data set using the  best lambda
out=glmnet(x, y, alpha=0)
predict(out, type="coefficients", s=bestlam)[1:20,]
# none of the coefficients are zero! 
# limitation of ridge regression


###############################################################################
## 6.6.2 The Lasso
###############################################################################

# again, use glmnet, but with alpha=1
lasso.mod=glmnet(x[idx.train,], y[idx.train], alpha=1, lambda=grid)
plot(lasso.mod)

# perform cross-validation and compute the associated test error
set.seed(1)
cv.out=cv.glmnet(x[idx.train ,], y[idx.train], alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod, s=bestlam, newx=x[idx.test,])
mean((lasso.pred-y.test)^2)
# 100743.4
# similar to the test MSE of ridge regression with λ chosen by cross-validation.

# lasso coefficient estimates are sparse!
# refit with all the data
out=glmnet(x, y, alpha=1, lambda=grid)
lasso.coef=predict(out,type="coefficients", s=bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef!=0]
# sparse coefficients!



###############################################################################
## 6.7 Lab 3: PCR and PLS Regression
###############################################################################

##  6.7.1 Principal Components Regression
library(pls)
set.seed(2)
pcr.fit=pcr(Salary~., data=Hitters, scale=TRUE, validation ="CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type="MSEP")

# to find optimal M, perform PCR on the training data and evaluate its test set performance.
set.seed(1)
pcr.fit=pcr(Salary~., data=Hitters, subset=idx.train, scale=TRUE, validation ="CV")
validationplot(pcr.fit, val.type="MSEP")
pcr.pred=predict(pcr.fit, x[idx.test,], ncomp=7)
mean((pcr.pred-y.test)^2)
# 96556.22

# re-fit PCR on the full data set, using M = 7
pcr.fit=pcr(y~x, scale=TRUE, ncomp=7)
summary(pcr.fit)

## 6.7.2 Partial Least Squares
set.seed(1)
pls.fit=plsr(Salary~., data=Hitters, subset=idx.train, scale=TRUE, validation ="CV")
summary(pls.fit)
validationplot(pls.fit, val.type="MSEP")
# The lowest cross-validation error occurs when M = 2

# evaluate the corresponding test set MSE.
pls.pred=predict(pls.fit, x[idx.test,], ncomp=2)
mean((pls.pred-y.test)^2)
# 101417.5
# slightly higher. But, we are using only M=2!

# perform PLS using the full data set, using M = 2
pls.fit=plsr(Salary~., data=Hitters, scale=TRUE, ncomp =2)
summary(pls.fit)

