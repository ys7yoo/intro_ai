###############################################################################
## 5.3.1 The Validation Set Approach
###############################################################################

rm(list=ls())
library(ISLR)
set.seed(1)
idx.train=sample(392,196)     # select a random subset of 196 observations out of 392 observations
idx.train   # check 

# fit with the chosen training data set
lm.fit=lm(mpg~horsepower, data=Auto, subset=idx.train)

# predict and calc MSE for the remaining (validation) data set
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-idx.train]^2)
# 26.14

# increase the order to quadratic and cubic
lm.fit2=lm(mpg~poly(horsepower,2), data=Auto, subset=idx.train)
mean((mpg-predict(lm.fit2 ,Auto))[-idx.train ]^2)
# 19.82

lm.fit3=lm(mpg~poly(horsepower,3), data=Auto, subset=idx.train)
mean((mpg-predict(lm.fit3 ,Auto))[-idx.train ]^2)
# 19.78


## choose a different training set
set.seed(2)
idx.train=sample(392,196)
lm.fit=lm(mpg~horsepower, subset=idx.train)

mean((mpg-predict(lm.fit,Auto))[-idx.train]^2)
# 23.30

lm.fit2=lm(mpg~poly(horsepower,2), data=Auto, subset=idx.train)
mean((mpg-predict(lm.fit2 ,Auto))[-idx.train ]^2)
# 18.90
lm.fit3=lm(mpg~poly(horsepower,3), data=Auto, subset=idx.train)
mean((mpg-predict(lm.fit3 ,Auto))[-idx.train ]^2)
# 19.26


###############################################################################
## 5.3.2 Leave-One-Out Cross-Validation
###############################################################################
rm(list=ls())
glm.fit=glm(mpg~horsepower, data=Auto)    # simple linear regression
coef(glm.fit)

library(boot)
glm.fit=glm(mpg~horsepower, data=Auto)
summary(glm.fit)
cv.err=cv.glm(Auto,glm.fit) # by default k=n (leave-one-out cross-validation)
summary(cv.err)
cv.err$delta  # cross-validation estimate for the test error
# 24.23151 24.23114
# raw cv estimate and bias compensated estimate

# increasingly complex polynomial fits
cv.error=rep(0,5)
for (i in 1:5){
    glm.fit=glm(mpg~poly(horsepower,i), data=Auto)
    cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
}
cv.error
plot(cv.error,type='o')


###############################################################################
## 5.3.3 k-Fold Cross-Validation
###############################################################################
set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
    glm.fit=glm(mpg~poly(horsepower,i), data=Auto)
    cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]    # 10 fold CV
}
cv.error.10
plot(cv.error.10,type='o')


###############################################################################
## 5.3.4 The Bootstrap
###############################################################################
# define a function
alpha.fn=function(data,index){
    X=data$X[index]
    Y=data$Y[index]
    return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
# test 
alpha.fn(Portfolio,1:100)

# sample with replacement
set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T))

# bootstrap
boot(Portfolio,alpha.fn,R=1000)

## Next, let's compare bootstrap vs linear regression
# define a function
boot.fn=function(data,index) {
    return(coef(lm(mpg~horsepower, data=data, subset =index)))
}
# test 
boot.fn(Auto,1:392)


set.seed(1)
# bootstrap
boot(Auto,boot.fn,1000)

# linear regression + random sample with replacement
boot.fn(Auto,sample(392,392,replace=T))  
boot.fn(Auto,sample(392,392,replace=T))
boot.fn(Auto,sample(392,392,replace=T))

# standard linear regression (using all data)
summary(lm(mpg~horsepower,data=Auto))$coef
# SE are quite different. See the textbook p.196 for discussion. 


## quadratic regression
boot.fn=function(data,index) {
    coefficients(lm(mpg~horsepower+I(horsepower ^2), data=data, subset=index))
}
set.seed(1)
boot(Auto,boot.fn,1000)

# standard formula
summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef


