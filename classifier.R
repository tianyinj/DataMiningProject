library(e1071)
library(randomForest)
train <- read.csv("train.csv")
test <- read.csv("test.csv")
guess <- read.csv("guess.csv")

remove.columns = function(data.frame) {
  remove.idx = which(names(data.frame) %in% 
                       unlist(strsplit("DEP_TIME, DEP_DELAY, DEP_DELAY_NEW, DEP_DELAY_GROUP, DEP_TIME_BLK, TAXI_OUT, WHEELS_OFF, WHEELS_ON, TAXI_IN, ARR_TIME, ARR_DELAY, ARR_DELAY_NEW, ARR_DEL15, ARR_DELAY_GROUP, ARR_TIME_BLK, CANCELLED, CANCELLATION_CODE, DIVERTED, ACTUAL_ELAPSED_TIME, AIR_TIME, CARRIER_DELAY, WEATHER_DELAY, NAS_DELAY, SECURITY_DELAY, LATE_AIRCRAFT_DELAY, FIRST_DEP_TIME, TOTAL_ADD_GTIME, LONGEST_ADD_GTIME, NA_DELAY, AIRLINE_ID, TAIL_NUM, ORIGIN_AIRPORT_SEQ_ID, ORIGIN, DEST_AIRPORT_SEQ_ID, FULL_DATE, DISTANCE, CRS_ARR_TIME, FL_NUM",
                                       split=", ")))
  return(data.frame[, -remove.idx])
}

guess <- remove.columns(guess)
test <- remove.columns(test)
train <- remove.columns(train)

test$UNIQUE_CARRIER = factor(test$UNIQUE_CARRIER, levels=levels(train$UNIQUE_CARRIER))
test$DEST = factor(test$DEST, levels=levels(train$DEST))
guess$UNIQUE_CARRIER = factor(guess$UNIQUE_CARRIER, levels=levels(train$UNIQUE_CARRIER))
guess$DEST = factor(guess$DEST, levels=levels(train$DEST))
guess$weather = factor(guess$weather, levels=levels(train$weather))

forest = randomForest(as.factor(DEP_DEL15) ~ ., data=train, ntree=500)
sum(predict(forest, train)==1 & train[,9]==1)/sum(train[,9]==1) ##recall in train 0.6045
sum(predict(forest, test)==1 & test[,9]==1)/sum(test[,9]==1) ##recall in test 0.1413161 
sum(predict(forest, train)!= train[,9])/29931                ##Training error 0.048311
sum(predict(forest, test)!= test[,9])/8000                 ##Testing error 0.1125

svmfit = svm(DEP_DEL15~., data = train, kernel='linear', cost=10, scale=FALSE)


#Fit the svm with a linear fit and a cost of 10
#Remember, the cost in this function is the reverse of the C in class
#Big C here means small margin
svmfit=svm(as.factor(train$DEP_DEL15) ~ pcaobj$scores[,1:2],kernel='linear',cost=10,scale=FALSE)
plot(svmfit,dat)
summary(svmfit)

#Let's fit and visualize a plot of the svm
svmfit=svm(y~.,data=dat,kernel='linear',cost=.1,scale=FALSE)
plot(svmfit,dat)
#See what happened to the margin?


#Now follows lab from ISL 9.6.2

#Let's generate a trickier example for the radial kernel
set.seed(1)
x=matrix(rnorm(200*2),ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y = c(rep(1,150),rep(2,50))
dat=data.frame(x=x,y=as.factor(y))

#and plot the example
plot(x,col=(3-y))


#Fit with radial kernel
svmfit=svm(y~.,data=dat,kernel='radial',gamma=1,cost=1)
plot(svmfit,dat)
summary(svmfit)

#Higher cost gives tighter margins...do we want this?
svmfit=svm(y~.,data=dat,kernel='radial',gamma=1,cost=1e5)
plot(svmfit,dat)
summary(svmfit)

#We can tune by cross-validation
#The cross validation takes a list of parameter values to try.  The rest of the parameters are fixed in the main function call
tune.out = tune(svm, DEP_DEL15~., data=train,kernel='radial',ranges=list(cost=c(.1,1,10,100,1000),gamma=c(.5,1,2,3,4)))
summary(tune.out)
#tune.out$best.model corresponds to the best model found according to cross-validated error.  We can plot it.
plot(tune.out$best.model,dat)
