# clean up the environment before starting
rm(list = ls())
WAUS <- read.csv("WarmerTomorrow2022.csv", stringsAsFactors = TRUE)
L <- as.data.frame(c(1:49))
set.seed(31486347) # Your Student ID is the random seed
L <- L[sample(nrow(L), 10, replace = FALSE),] # sample 10 locations
WAUS <- WAUS[(WAUS$Location %in% L),]
WAUS <- WAUS[sample(nrow(WAUS), 2000, replace = FALSE),] # sample 2000 rows


library(tree)
library(e1071)
library(ROCR)
library(randomForest)
library(adabag)
library(rpart)
library(ggplot2)

View(WAUS)


#QUESTION 1
nrow(WAUS[WAUS$WarmerTomorrow == 1, ]) / nrow(WAUS)

nrow(WAUS[WAUS$WarmerTomorrow == 0, ]) / nrow(WAUS)

real_value_predictors = WAUS[, c(5:9,11, 14:23)]

summary(real_value_predictors)

apply(real_value_predictors, 2, sd, na.rm = TRUE)

#QUESTION 2
#warmerdata = WAUS[, c(5:24)]
warmerdata = WAUS[, c(5:9, 11, 14:24)]
warmerdata = warmerdata[complete.cases(warmerdata),]
warmerdata$WarmerTomorrow = as.factor(warmerdata$WarmerTomorrow)

str(warmerdata)

#QUESTION 3
set.seed(31486347) #Student ID as random seed
train.row = sample(1:nrow(warmerdata), 0.7*nrow(warmerdata))
warmer.train = warmerdata[train.row,]
warmer.test = warmerdata[-train.row,]

#QUESTION 4

#decision tree
warmer.tree = tree(WarmerTomorrow ~. ,data=warmer.train)
plot(warmer.tree)
text(warmer.tree, pretty=0, cex=0.7)

#naive bayes
warmer.bayes = naiveBayes(WarmerTomorrow ~. ,data=warmer.train)

#bagging
warmer.bag <- bagging(WarmerTomorrow ~. ,data=warmer.train, mfinal=10)

#boosting
warmer.boost <- boosting(WarmerTomorrow ~. ,data=warmer.train, mfinal=10)

#Random Forest
warmer.rf <- randomForest(WarmerTomorrow ~. ,data=warmer.train, na.action = na.exclude)

#QUESTION 5
#decision tree
warmer.tree.pred = predict(warmer.tree, warmer.test, type = "class")
t1=table(Predicted_Class = warmer.tree.pred, Actual_Class = warmer.test$WarmerTomorrow)

cat("\n#Decision Tree Confusion\n")
print(t1)

cat("\n#Accuracy\n")
print((t1[1,1] + t1[2,2]) / (t1[1,1]+t1[1,2]+t1[2,1]+t1[2,2]))


#naive bayes
warmer.bayes.pred = predict(warmer.bayes, warmer.test)
t1=table(Predicted_Class = warmer.bayes.pred, Actual_Class = warmer.test$WarmerTomorrow)

cat("\n#Naive Bayes Confusion\n")
print(t1)

cat("\n#Accuracy\n")
print((t1[1,1] + t1[2,2]) / (t1[1,1]+t1[1,2]+t1[2,1]+t1[2,2]))

#bagging
warmer.bag.pred = predict.bagging(warmer.bag, warmer.test)
t1=table(Predicted_Class = warmer.bag.pred$class, Actual_Class = warmer.test$WarmerTomorrow)

cat("\n#Bagging Confusion\n")
print(t1)

cat("\n#Accuracy\n")
print((t1[1,1] + t1[2,2]) / (t1[1,1]+t1[1,2]+t1[2,1]+t1[2,2]))

#boosting
warmer.boost.pred = predict.boosting(warmer.boost, warmer.test)
t1=table(Predicted_Class = warmer.boost.pred$class, Actual_Class = warmer.test$WarmerTomorrow)

cat("\n#Boosting Confusion\n")
print(t1)

cat("\n#Accuracy\n")
print((t1[1,1] + t1[2,2]) / (t1[1,1]+t1[1,2]+t1[2,1]+t1[2,2]))

#Random Forest
warmer.rf.pred = predict(warmer.rf, warmer.test)
t1=table(Predicted_Class = warmer.rf.pred, Actual_Class = warmer.test$WarmerTomorrow)

cat("\n#Random Forest Confusion\n")
print(t1)

cat("\n#Accuracy\n")
print((t1[1,1] + t1[2,2]) / (t1[1,1]+t1[1,2]+t1[2,1]+t1[2,2]))


#QUESTION 6

#decision tree
warmer.tree.pred = predict(warmer.tree, warmer.test, type = "vector")
warmer.tree.pred = prediction(warmer.tree.pred[, 2], warmer.test$WarmerTomorrow)
warmer.tree.perf = performance(warmer.tree.pred, "tpr", "fpr")

warmer.tree.fpr = warmer.tree.perf@x.values
warmer.tree.tpr = warmer.tree.perf@y.values
df.tree = data.frame(warmer.tree.fpr, warmer.tree.tpr)

auc <- performance(warmer.tree.pred, measure = "auc")
cat("\n#AUC of Decision Tree\n")
print(auc@y.values[[1]])


#Naive bayes
warmer.bayes.pred = predict(warmer.bayes, warmer.test, type='raw')
warmer.bayes.pred = prediction(warmer.bayes.pred[, 2], warmer.test$WarmerTomorrow)
warmer.bayes.perf = performance(warmer.bayes.pred, "tpr", "fpr")

warmer.bayes.fpr = warmer.bayes.perf@x.values
warmer.bayes.tpr = warmer.bayes.perf@y.values
df.bayes = data.frame(warmer.bayes.fpr, warmer.bayes.tpr)

auc <- performance(warmer.bayes.pred, measure = "auc")
cat("\n#AUC of Naive Bayes\n")
print(auc@y.values[[1]])

#bagging
warmer.bag.pred = predict.bagging(warmer.bag, warmer.test)
warmer.bag.pred = prediction(warmer.bag.pred$prob[, 2], warmer.test$WarmerTomorrow)
warmer.bag.perf = performance(warmer.bag.pred, "tpr", "fpr")

warmer.bag.fpr = warmer.bag.perf@x.values
warmer.bag.tpr = warmer.bag.perf@y.values
df.bag = data.frame(warmer.bag.fpr, warmer.bag.tpr)

auc <- performance(warmer.bag.pred, measure = "auc")
cat("\n#AUC of Bagging\n")
print(auc@y.values[[1]])

#boosting
warmer.boost.pred = predict.boosting(warmer.boost, warmer.test)
warmer.boost.pred = prediction(warmer.boost.pred$prob[, 2], warmer.test$WarmerTomorrow)
warmer.boost.perf = performance(warmer.boost.pred, "tpr", "fpr")

warmer.boost.fpr = warmer.boost.perf@x.values
warmer.boost.tpr = warmer.boost.perf@y.values
df.boost = data.frame(warmer.boost.fpr, warmer.boost.tpr)

auc <- performance(warmer.boost.pred, measure = "auc")
cat("\n#AUC of Boosting\n")
print(auc@y.values[[1]])

#Random Forest
warmer.rf.pred = predict(warmer.rf, warmer.test, type='prob')
warmer.rf.pred = prediction(warmer.rf.pred[, 2], warmer.test$WarmerTomorrow)
warmer.rf.perf = performance(warmer.rf.pred, "tpr", "fpr")

warmer.rf.fpr = warmer.rf.perf@x.values
warmer.rf.tpr = warmer.rf.perf@y.values
df.rf = data.frame(warmer.rf.fpr, warmer.rf.tpr)

auc <- performance(warmer.rf.pred, measure = "auc")
cat("\n#AUC of Random Forest\n")
print(auc@y.values[[1]])

#Graph
library(ggplot2)

graph = ggplot() + 
  geom_line(data=df.tree, aes(x=df.tree[[1]], y=df.tree[[2]], color = "Decision Tree"), size=0.9)+
  geom_line(data=df.bayes, aes(x=df.bayes[[1]], y=df.bayes[[2]], color = "Naive Bayes"), size=0.9)+
  geom_line(data=df.bag, aes(x=df.bag[[1]], y=df.bag[[2]], color = "Bagging"), size=0.9)+
  geom_line(data=df.boost, aes(x=df.boost[[1]], y=df.boost[[2]], color = "Boosting"), size=0.9)+
  geom_line(data=df.rf, aes(x=df.rf[[1]], y=df.rf[[2]], color = "Random Forest"), size=0.9)+
  xlab("")+
  ylab("Mean")+
  geom_abline(intercept=0, slope=1, linetype="dashed", color="black", size=1)+
  ggtitle("Negative Emotions")
graph

#QUESTION 7

#QUESION 8
#Attribute importance

#Decision Tree
cat("\n#Decision Tree Attribute Importance\n")
print(summary(warmer.tree))

#Naive Bayes
#e1071 package does not have importance method, so we use the caret package
library(caret)
cat("\n#Naive Bayes Attribute Importance\n")
Grid = data.frame(usekernel=TRUE,laplace = 0,adjust=1)

mdl = train(WarmerTomorrow ~ .,data=warmer.train,method="naive_bayes",
            trControl=trainControl(method="none"),
            tuneGrid=Grid)
varImp(mdl)

#Bagging
cat("\n#Baging Attribute Importance\n")
print(warmer.bag$importance[order(-warmer.bag$importance)])

#Boosting
cat("\n#Boosting Attribute Importance\n")
print(warmer.boost$importance[order(-warmer.boost$importance)])

#Random Forest
cat("\n#Random Forest Attribute Importance\n")
print(warmer.rf$importance[,1][order(-warmer.rf$importance)])

warmer.rf$importance + warmer.boost$importance

#QUESTION 9
#our chosen model is a decision tree
simple.classifier = tree(WarmerTomorrow ~ Sunshine + MinTemp + Cloud9am,data=warmer.train)
plot(simple.classifier)
text(simple.classifier, pretty=0, cex=0.7)

#checking confusion matrix
simple.classifier.pred = predict(simple.classifier, warmer.test, type = "class")
t1=table(Predicted_Class = simple.classifier.pred, Actual_Class = warmer.test$WarmerTomorrow)

cat("\n#Decision Tree Confusion\n")
print(t1)

cat("\n#Accuracy\n")
print((t1[1,1] + t1[2,2]) / (t1[1,1]+t1[1,2]+t1[2,1]+t1[2,2]))

#getting tpr and fpr and AUC
#decision tree
simple.classifier.pred = predict(simple.classifier, warmer.test, type = "vector")
simple.classifier.pred = prediction(simple.classifier.pred[, 2], warmer.test$WarmerTomorrow)
simple.classifier.perf = performance(simple.classifier.pred, "tpr", "fpr")

simple.classifier.fpr = simple.classifier.perf@x.values
simple.classifier.tpr = simple.classifier.perf@y.values
df.simple.classifier = data.frame(simple.classifier.fpr, simple.classifier.tpr)

auc <- performance(simple.classifier.pred, measure = "auc")
cat("\n#AUC of Decision Tree\n")
print(auc@y.values[[1]])

#GRAPH
graph = ggplot() + 
  geom_line(data=df.tree, aes(x=df.tree[[1]], y=df.tree[[2]], color = "Decision Tree"), size=0.9)+
  geom_line(data=df.bayes, aes(x=df.bayes[[1]], y=df.bayes[[2]], color = "Naive Bayes"), size=0.9)+
  geom_line(data=df.bag, aes(x=df.bag[[1]], y=df.bag[[2]], color = "Bagging"), size=0.9)+
  geom_line(data=df.boost, aes(x=df.boost[[1]], y=df.boost[[2]], color = "Boosting"), size=0.9)+
  geom_line(data=df.rf, aes(x=df.rf[[1]], y=df.rf[[2]], color = "Random Forest"), size=0.9)+
  geom_line(data=df.simple.classifier, aes(x=df.simple.classifier[[1]], y=df.simple.classifier[[2]], color = "SIMPLE CLASSIFIER"), size= 1.5)+
  xlab("")+
  ylab("Mean")+
  geom_abline(intercept=0, slope=1, linetype="dashed", color="black", size=1)+
  ggtitle("Negative Emotions")
graph


#QUESTION 10
#Since there are less variables, less rows are removed due to NAs
warmerdata = WAUS[, c(9, 5, 8, 23, 17, 20, 24)]
warmerdata = warmerdata[complete.cases(warmerdata),]
warmerdata$WarmerTomorrow = as.factor(warmerdata$WarmerTomorrow)
set.seed(31486347) #Student ID as random seed
train.row = sample(1:nrow(warmerdata), 0.7*nrow(warmerdata))
warmer.train = warmerdata[train.row,]
warmer.test = warmerdata[-train.row,]

#I chose to use a random forest model with a 1000 tree (default is 500)

best.classifier <- randomForest(WarmerTomorrow ~ Sunshine + MinTemp + Evaporation + Temp3pm + Humidity3pm + Cloud9am, data=warmer.train, na.action = na.exclude, ntree=1000)

#get accuracy
best.classifier.pred = predict(best.classifier, warmer.test)
t1=table(Predicted_Class = best.classifier.pred, Actual_Class = warmer.test$WarmerTomorrow)

cat("\n#Random Forest Confusion\n")
print(t1)

cat("\n#Accuracy\n")
print((t1[1,1] + t1[2,2]) / (t1[1,1]+t1[1,2]+t1[2,1]+t1[2,2]))


#get AUC
best.classifier.pred = predict(best.classifier, warmer.test, type='prob')
best.classifier.pred = prediction(best.classifier.pred[, 2], warmer.test$WarmerTomorrow)
best.classifier.perf = performance(best.classifier.pred, "tpr", "fpr")

best.classifier.fpr = best.classifier.perf@x.values
best.classifier.tpr = best.classifier.perf@y.values
df.best.classifier = data.frame(best.classifier.fpr, best.classifier.tpr)

auc <- performance(best.classifier.pred, measure = "auc")
cat("\n#AUC of Random Forest\n")
print(auc@y.values[[1]])

graph = ggplot() + 
  geom_line(data=df.tree, aes(x=df.tree[[1]], y=df.tree[[2]], color = "Decision Tree"), size=0.9)+
  geom_line(data=df.bayes, aes(x=df.bayes[[1]], y=df.bayes[[2]], color = "Naive Bayes"), size=0.9)+
  geom_line(data=df.bag, aes(x=df.bag[[1]], y=df.bag[[2]], color = "Bagging"), size=0.9)+
  geom_line(data=df.boost, aes(x=df.boost[[1]], y=df.boost[[2]], color = "Boosting"), size=0.9)+
  geom_line(data=df.rf, aes(x=df.rf[[1]], y=df.rf[[2]], color = "BEST CLASSIFIER"), size=1.5)+
  geom_line(data=df.best.classifier, aes(x=df.best.classifier[[1]], y=df.best.classifier[[2]], color = "Random Forest"), size= 0.9)+
  xlab("")+
  ylab("Mean")+
  geom_abline(intercept=0, slope=1, linetype="dashed", color="black", size=1)+
  ggtitle("Negative Emotions")
graph


#QUESTION 11
library(neuralnet)

warmerdata = WAUS[, c(5:9, 11, 14:24)]
warmerdata = warmerdata[complete.cases(warmerdata),]
warmerdata$WarmerTomorrow = as.numeric(warmerdata$WarmerTomorrow)

str(warmerdata)

set.seed(31486347) #Student ID as random seed
train.row = sample(1:nrow(warmerdata), 0.7*nrow(warmerdata))
warmer.train = warmerdata[train.row,]
warmer.test = warmerdata[-train.row,]

#train neural net
warmer.nn = neuralnet(WarmerTomorrow ~ Sunshine + MinTemp + Evaporation + Temp3pm + Humidity3pm + Cloud9am, warmer.train, hidden=5)

#get predictions
warmer.nn.pred = compute(warmer.nn, warmer.test[, c(5, 1, 4, 16, 10, 13, 17)] )

#round results to integers
warmer.nn.pred = as.data.frame(round(warmer.nn.pred$net.result,0))

# plot confusion matrix
t1 = table(observed = warmer.test$WarmerTomorrow, predicted = warmer.nn.pred$V1)
cat("\n#Neural Network Confusion\n")
print(t1)

cat("\n#Accuracy\n")
print((t1[1,1] + t1[2,2]) / (t1[1,1]+t1[1,2]+t1[2,1]+t1[2,2]))
