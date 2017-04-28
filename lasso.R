library(glmnet)
source('remove_columns.R')

# read in training data
flight.data.tr = read.csv('dep2015.csv')
flight.data.tr = remove.columns(flight.data.tr)
delay15.idx = which(names(flight.data.tr) == 'DEP_DEL15')
X_train = flight.data.tr[,-delay15.idx]
y_train = unlist(flight.data.tr[,delay15.idx])

# read in test data
flight.data.te = read.csv('dep2016_visible.csv')
flight.data.te = remove.columns(flight.data.te)
delay16.idx = which(names(flight.data.te) == 'DEP_DEL15')
X_test = flight.data.te[,-delay16.idx]
y_test = unlist(flight.data.te[,delay16.idx])

# only look at numeric columns
X_train = as.matrix(X_train[, which(sapply(X_train, is.numeric))])
X_test = as.matrix(X_test[, which(sapply(X_test, is.numeric))])

# fit lasso
fit_cv_lasso = cv.glmnet(X_train, y_train, nfolds=10, alpha=1)
fit_lasso = glmnet(X_train,y_train,family='gaussian',alpha=1)

# print summaries
coef(fit_lasso, s=fit_cv_lasso$lambda.min)
coef(fit_lasso, s=fit_cv_lasso$lambda.1se)

# Compute the predictions on new data using beta at min lambda
yhat_lasso = predict(fit_lasso,newx=X_test,s=fit_cv_lasso$lambda.min)
yhat_lasso = ifelse(yhat_lasso > 0.5, 1, 0)
# Check prediction error - min lambda
mean(yhat_lasso != y_test)

# Compute the predictions on new data using beta at 1 se lambda
yhat_lasso = predict(fit_lasso,newx=X_test,s=fit_cv_lasso$lambda.1se)
yhat_lasso = ifelse(yhat_lasso > 0.5, 1, 0)
# Check prediction error - 1 se lambda
mean(yhat_lasso != y_test)

# Compare them to estimated errors
# estimated error - min lambda
fit_cv_lasso$cvm[which(fit_cv_lasso$lambda == fit_cv_lasso$lambda.min)]
# estimated error - 1se lambda
fit_cv_lasso$cvm[which(fit_cv_lasso$lambda == fit_cv_lasso$lambda.1se)]

# plot cross validation curves
plot(fit_cv_lasso)