library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(ggplot2)
library(lattice)
library(ROCR)
## load dataset
bank.df <- read.csv("C:/Users/JLi82/OneDrive/Documents/syllabus/6356/R scripts and datasets/bank-full.csv") 
## transfer yes or no in outcome variable as 1 or 0
bank.df$y = ifelse(bank.df$y == 'yes',1,0)
View(bank.df)
## scale the numerical data
bank.df[c(1,6,10,12,13)]=scale(bank.df[c(1,6,10,12,13)])
View(bank.df)

#Create a training and validation partition
numberOfRows <- nrow(bank.df)

set.seed(123)
train.index <- sample(numberOfRows, numberOfRows*0.7)
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]
View(train.df)
View(valid.df)

######## create a classification tree--- decision tree###############################

# fit the decision tree classification
classifier = rpart(formula = y ~ .,
                   data = train.df, method = "class")

# plot
prp(classifier, type = 2, extra = 104, fallen.leaves = TRUE, main="Decision Tree")


#cp	= complexity parameter. Any split that does not decrease the overall lack of fit by a factor of cp is not attempted. 
#For instance, with anova splitting, this means that the overall R-squared must increase by cp at each step. 
#The main role of this parameter is to save computing time by pruning off splits that are obviously not worthwhile. 
#Essentially,the user informs the program that any split which does not improve the fit by cp will likely be pruned off by cross-validation,

.ct <- rpart(y ~ ., data = train.df, method = "class", cp = 0.01, maxdepth = 5, minsplit = 1)

# print tree summary and plot tree. try different values for extra
printcp(.ct)
prp(.ct, type = 1, extra = 1, under = FALSE, split.font = 1, varlen = -10)

# classify records in the validation data using the classification tree.
# set argument type = "class" in predict() to generate predicted class membership.
ct.pred <- predict(.ct, valid.df, type = "class")

# generate confusion matrix for validation data
confusionMatrix(ct.pred, as.factor(valid.df$y))

# build a deeper classification tree
max.ct <- rpart(y ~ ., data = train.df, method = "class", cp = 0, minsplit = 1, maxdepth = 30)

# count number of leaves
length(max.ct$frame$var[max.ct$frame$var == "<leaf>"])

# plot tree
prp(max.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(max.ct$frame$var == "<leaf>", 'gray', 'white')) 

# classify records in the validation data.
# set argument type = "class" in predict() to generate predicted class membership.
max.pred <- predict(max.ct, valid.df, type = "class")

# generate confusion matrix for training data
confusionMatrix(max.pred, as.factor(valid.df$y))



# Create code to prune the tree
# xval refers to the number of partitions to use in rpart's built-in cross-validation
# procedure argument.  With xval = 5, bank.df is split into 5 partitions of 1000
# observations each.  A partition is selected at random to hold back for validation 
# while the remaining 4000 observations are used to build each split in the model. 
# Process is repeated for each parition and xerror is calculated as the average error across all partitions.
# complexity paramater (cp) sets the minimum reduction in complexity required for the model to continue.
# minsplit is the minimum number of observations in a node for a split to be attempted.
cv.ct <- rpart(y ~ ., data = bank.df, method = "class", 
               control = rpart.control(cp = 0.00001, minsplit = 5, xval = 5))

# use printcp() to print the table. 
printcp(cv.ct)
prp(cv.ct, type = 1, extra = 1, split.font = 1, varlen = -10)  

#prune the tree using the lowest value for xerror
#Note: the prune function requires cp as a parameter so we need to get cp for lowest value of xerror
pruned.ct <- prune(cv.ct, 
                   cp = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"])
which.min(cv.ct$cptable[,"xerror"])
#get count of the number of splits
cp_df <- data.frame(pruned.ct$cptable)
max(cp_df$nsplit)

#another way to get the count of the number of splits
pruned.ct$cptable[which.max(pruned.ct$cptable[,"nsplit"]),"nsplit"]

#get count of the number of nodes
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])

#plot the best fitting tree
prp(pruned.ct, type = 1, extra = 1, split.font = 1, varlen = -10,
    box.col=ifelse(pruned.ct$frame$var == "<leaf>", 'gray', 'white'))  

prune.pred <- predict(pruned.ct, valid.df, type = "class")
View(prune.pred)
# generate confusion matrix for validation data
confusionMatrix(prune.pred, as.factor(valid.df$y))

#######roc curve for best fitting tree##########
# predict test data by probability
pred.DT = predict(pruned.ct, newdata = valid.df[-17], type = 'prob')
print(pred.DT)
# calculate ROC curve
rocr.pred = prediction(predictions = pred.DT[,2] , labels = valid.df$y)
rocr.perf = performance(rocr.pred, measure = "tpr", x.measure = "fpr")
rocr.auc = as.numeric(performance(rocr.pred, "auc")@y.values)

# print ROC AUC
rocr.auc

# plot ROC curve
plot(rocr.perf,
     lwd = 3, colorize = TRUE,
     print.cutoffs.at = seq(0, 1, by = 0.1),
     text.adj = c(-0.2, 1.7),
     main = 'ROC Curve')
mtext(paste('Decision Tree - auc : ', round(rocr.auc, 5)))
abline(0, 1, col = "red", lty = 2)


##############use random forest to see if it improved prediction############################################
library(randomForest)

#mtry = number of variables randomly sampled  as candidates at each split
#creates ntree number of trees by randomly sampling with replacement
#results are the average of all the trees
rf <- randomForest(as.factor(y) ~ ., data = train.df, 
                   ntree = 500, mtry = 11, nodesize = 1, importance = TRUE, sampsize = 2000) 

#plot the variables by order of importance
varImpPlot(rf, type = 1)

#create a confusion matrix
valid.df$y <- factor(valid.df$y)
rf.pred <- predict(rf, valid.df)
confusionMatrix(rf.pred, valid.df$y)


