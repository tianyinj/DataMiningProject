library(randomForest)

train = flight.data.tr[, -which(names(flight.data.tr) == "TAIL_NUM" | 
                                  names(flight.data.tr) == "FL_NUM" |
                                  names(flight.data.tr) == "HUMIDITY")]

train$UNIQUE_CARRIER = factor(train$UNIQUE_CARRIER, levels=unique(c(levels(test$UNIQUE_CARRIER),
                                                                  levels(train$UNIQUE_CARRIER))))
train$ORIGIN = factor(train$ORIGIN, levels=unique(c(levels(test$ORIGIN),
                                                  levels(train$ORIGIN))))
train$DEST = factor(train$DEST, levels=unique(c(levels(test$DEST),
                                              levels(train$DEST))))

set.seed(123)
forest = randomForest(as.factor(DEP_DEL15) ~ ., data=train, ntree=500)

# out-of-bag error
oob_predictions = forest$predicted
mean(oob_predictions != train$DEP_DEL15)

# training error
yhat_train = predict(forest, newdata=train)
mean(yhat_train != train$DEP_DEL15)

# explore
par(mfrow=c(1,2), mar=c(4,4,2,2), cex.main=0.8)
varImpPlot(forest)
partialPlot(forest, pred.data=train, x.var='DAY_OF_MONTH')


################## TEST ###########################

test = flight.data.te[, -which(names(flight.data.te) == "TAIL_NUM" | 
                                  names(flight.data.te) == "FL_NUM" |
                                  names(flight.data.te) == "HUMIDITY")]

test$UNIQUE_CARRIER = factor(test$UNIQUE_CARRIER, levels=unique(c(levels(test$UNIQUE_CARRIER),
                                                                  levels(train$UNIQUE_CARRIER))))
test$ORIGIN = factor(test$ORIGIN, levels=unique(c(levels(test$ORIGIN),
                                                  levels(train$ORIGIN))))
test$DEST = factor(test$DEST, levels=unique(c(levels(test$DEST),
                                              levels(train$DEST))))

yhat_test = predict(forest, newdata=test)
