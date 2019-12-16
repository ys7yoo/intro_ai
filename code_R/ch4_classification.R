# Lab for ch4 classification (p.154)

#setwd("~/Dropbox/srcm/data_science_R")
rm(list=ls())

###############################################################################
## 4.6.1 The Stock Market Data
###############################################################################

library(ISLR) 
names(Smarket)
head(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)

# calc correlation
cor(Smarket[,-9])    # last variable (Direction) is categorical

# volume is increasing over time
attach(Smarket)
plot(Year,Volume)

###############################################################################
## 4.6.2 Logistic Regression
# predict Direction using Lag1 through Lag5 and Volume.
###############################################################################

# fit the model
glm.fit=glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket,family=binomial )
summary(glm.fit)

# check coefficients
coef(glm.fit)
summary(glm.fit)$coef       # show coeffs with other details
summary(glm.fit)$coef[,4]   # show p-values only

# predict
glm.probs=predict(glm.fit,type="response")
glm.probs[1:10]
contrasts(Direction)
# convert probabilities to labels
glm.pred=rep("Down",1250)
glm.pred[glm.probs>.5]="Up"
# compute a confusion matrix 
table(glm.pred,Direction)
mean(glm.pred==Direction)   # correct prediction 52 % of the time (48 % training error!)


## divide training and test sets 
train=(Year<2005)     # condition for training set 
# train for the subset 
glm.fit=glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket, family=binomial, subset=train) # subset with date befor 2005
# test for the remaining data
Smarket.2005=Smarket[!train,]   
Direction.2005=Direction[!train]
dim(Smarket.2005)
glm.probs=predict(glm.fit, Smarket.2005, type="response")
# check performance
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
#mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005)  # calc error rate

## refit with only Lag1 and Lag2 as predictors 
glm.fit=glm(Direction ~ Lag1+Lag2, data=Smarket, family=binomial,
             subset =train)
glm.probs=predict(glm.fit, Smarket.2005, type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
#mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005)  # calc error rate


###############################################################################
## 4.6.3 Linear Discriminant Analysis
###############################################################################
# use lda() function in the MASS library 
library(MASS)
lda.fit=lda(Direction~Lag1+Lag2 ,data=Smarket ,subset =train)
lda.fit
plot(lda.fit)   # plot linear discriminants for each class

# predict
lda.pred=predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class=lda.pred$class   # predicted class

# compare LDA with logistic regression
table(lda.class,Direction.2005)
mean(lda.class==Direction.2005)   # accuracy = 56 %

# apply different thresholds
sum(lda.pred$posterior[,1]>=.5)

sum(lda.pred$posterior[,1]>=.9)    # no certain day!

# higher prob means "Down"
# lda.pred$posterior[1:20, 1]
# lda.pred$class[1:20]

# to reverse the factor
# factor(Smarket$Direction, levels = c("Up", "Down"))
# factor(Smarket$Direction,levels=rev(levels(Smarket$Direction))) # reverse factor order 


###############################################################################
## 4.6.4 Quadratic Discriminant Analysis
###############################################################################
# the qda() function in the MASS library
qda.fit=qda(Direction~Lag1+Lag2, data=Smarket, subset=train)
qda.fit

qda.class=predict(qda.fit,Smarket.2005)$class
table(qda.class,Direction.2005)
mean(qda.class==Direction.2005)  # accurary = 60 %

###############################################################################
## 4.6.5 K-Nearest Neighbors
###############################################################################
# the knn() function in the class library
library(class)

# prepare training data matrix
train.X=cbind(Lag1,Lag2)[train,]
# prepare training label matrix
train.Direction=Direction[train]
# prepare test data matrix
test.X=cbind(Lag1,Lag2)[!train,]


# predict using KNN with K=1
# set a random seed
set.seed(1)
knn.pred=knn(train.X, test.X, train.Direction, k=1) # be careful orders 
?knn

table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005) # 50 % accuracy

# predict using KNN with K=3
knn.pred=knn(train.X,test.X,train.Direction,k=3)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005) # 54 % accuracy  (Not so impressive)


###############################################################################
## 4.6.6 An Application to Caravan Insurance Data
###############################################################################
# the Caravan data set in the ISLR library
dim(Caravan)  # 5822 x 86
head(Caravan)
attach(Caravan)
summary(Purchase)

348/(348+5474)
# only 6% of people purchased
# can we do better?


# the scale of the variable matters
# standardize the data 
# a mean of zero and a standard deviation of one.
standardized.X=scale(Caravan [,-86])
# confirm
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])  
var(standardized.X[,2])

# split data
idx.test=1:1000   # first 1000 rows are for testing
train.X=standardized.X[-idx.test,]
test.X=standardized.X[idx.test,]
train.Y=Purchase[-idx.test]
test.Y=Purchase[idx.test]

# run KNN with K=1
set.seed(1)
knn.pred=knn(train.X, test.X, train.Y, k=1)

mean(test.Y!=knn.pred)   # error prob = 11 %  <= not interested!
mean(test.Y!="No")       # only 6 % say Yes

table(knn.pred,test.Y)
# the bottom row matters!
# Given the prediction, how accurate is it? 
9/(68+9)    # 12%

# run KNN with K=3
knn.pred=knn(train.X, test.X, train.Y, k=3)
table(knn.pred,test.Y)
5/(21+5)   # 19%

# run KNN with K=5
knn.pred=knn(train.X, test.X, train.Y, k=5)
table(knn.pred,test.Y)
4/(11+4)   # 27%

# As a comparison, fit logistic regression.
glm.fit=glm(Purchase~., data=Caravan, family=binomial, subset=-idx.test)

# make prediction
glm.probs=predict(glm.fit, Caravan[idx.test ,], type="response")

# label with threshold=0.5
glm.pred=rep("No",1000)
glm.pred[glm.probs>.5]="Yes"
table(glm.pred,test.Y)

# lower threshold
glm.pred=rep("No",1000)
glm.pred[glm.probs>.25]="Yes"
table(glm.pred,test.Y)
11/(22+11)
# not bad!

