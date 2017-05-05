library(randomForest)

train = flight.data.tr[, -which(names(flight.data.tr) == "TAIL_NUM")]

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
