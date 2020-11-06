####################################################################
#question 3

setwd("~/Desktop/Math748/HW3/")
data.zip=read.table("../four_datasets_textbook/data-zipcode/zip.train")
data.filter = data.zip[which(data.zip$V1 < 4 & data.zip$V1 >0), ]

test=1:500
data.test = data.filter[test, ]
data.train = data.filter[-test, ]
dim(data.test)
dim(data.train)

library(MASS)
library(class)
library(ISLR)

##############################################################################
#3a
#function for applying KNN and calculate training error
knn_training <- function (train, test, k){
  knn.pred <- knn(train[,-1], test[,-1], cl=data.train$V1, k=k)
  train_error_knn <- mean(knn.pred != data.train$V1)
}

#function for applying KNN and calculate testing error
knn_testing <- function(train, test, k){
  knn.pred <- knn(train[,-1], test[,-1], cl=data.train$V1, k=k)
  test_error_knn <- mean(knn.pred != data.test$V1)
}

a =knn_training(data.train, data.test, 5)
b=knn_testing(data.train, data.test, 5)

for (i in c(1, 3, 5, 7, 15)) {
  train_error <- knn_training(data.train, data.test, i)
  test_error <- knn_testing(data.train, data.test, i)
  cat('For k =',i, ', train error:',train_error,', test error:',test_error,"\n")
}

#For k= 1 , train error: 0.6647307 , test error: 0.008 
#For k= 3 , train error: 0.6615628 , test error: 0.012 
#For k= 5 , train error: 0.6652587 , test error: 0.004 
#For k= 7 , train error: 0.6642027 , test error: 0.01 
#For k= 15 , train error: 0.6615628 , test error: 0.016 

##############################################################################
#3b
train.data.removed = data.train[ ,18:257]
test.data.removed = data.test[ ,18:257]

lda.model = lda(train.data.removed, grouping= data.train[,1])
summary(lda.model)

lda.train = predict(lda.model,train.data.removed)$class
lda.train.error = mean(lda.train != data.train[,1])
lda.train.error #0.004223865

lda.test = predict(lda.model,test.data.removed)$class
lda.test.error = mean(lda.test != data.test[,1])
lda.test.error #0.022

##############################################################################
#question 4
set.seed(45436) 
n_test = 1000
n_train = 400

#4b & 4c
class1_mu = t(c(1,1))
class0_mu =t(c(-1,-1))
cov_class1 = matrix(c(4,0,0,4), ncol=2)
cov_class0 = matrix(c(4,0,0,4), ncol=2)

train.class1 = mvrnorm(n_train, mu=class1_mu, Sigma = cov_class1)
train.class0 = mvrnorm(n_train, mu=class0_mu, Sigma = cov_class0)

test.class1 = mvrnorm(n_test, mu=class1_mu, Sigma = cov_class1)
test.class0 = mvrnorm(n_test, mu=class0_mu, Sigma = cov_class0)

plot(test.class1,type="p",main="All Linear Classifiers",xlab="", ylab="", xlim=c(-4,4), ylim=c(-4,4), col="red")
points(test.class0, col ="blue")
#4a
#Bayes decision boundry
abline(a=0,b=-1, col="red")  

test.data2 = rbind(test.class1, test.class0)
train.data2 = rbind(train.class1, train.class0)

##########################################

## Bayes 
response_training = c(rep(1,200), rep(0,200))
training = cbind(train.data2, response_training)
training_bayes = 1*(training[,1] > training[,2])
training_bayes_error = mean(training_bayes != training[,3])
print(training_bayes_error) #0.5075

response_testing = c(rep(1,500), rep(0,500))
testing = cbind(test.data2 , response_testing)
testing_bayes = 1*(testing[ ,1] > testing[ ,2])
testing_bayes_error = mean(testing_bayes != testing[,3])
print(testing_bayes_error) #0.502

##########################################
#Linear Regression
response_training_lm = c(rep(1,400), rep(0,400))
training_linear_model = lm(response_training_lm ~ train.data2)
summary(training_linear_model)
intercept1 = training_linear_model$coef[1]
slope1 = training_linear_model$coef[-1]

#linear Decision boundry
abline(coef=c(((intercept1-0.5)/(-slope1[2])), slope1[1]/(-slope1[2])), col="green") 


#train error
training_linear = as.vector(1*((intercept + trainig_data %*% slope) >0.5))
training_linear_error = mean(training_linear != response_training)
print(training_linear_error) #0.5

#testing error
response_testing = c(rep(1,500), rep(0,500))
test_data = rbind(green_test, red_test)
testing_linear = as.vector(1*((intercept + test_data %*% slope) >0.5))
testing_linear_error = mean(testing_linear != response_testing)
print(testing_linear_error) #0.511

##########################################
# LDA
train.data2.frame = as.data.frame(train.data2)

glm.fit <- glm(response_training_lm ~ train.data2, data=train.data2.frame ,family=binomial) #family=binomial
slope2 = (-glm.fit$coefficients[2])/(glm.fit$coefficients[3])
intercept2= (-glm.fit$coefficients[1])/(glm.fit$coefficients[3])
abline(a=intercept2, b=slope2, col="orange")
summary(glm.fit)

##########################################

