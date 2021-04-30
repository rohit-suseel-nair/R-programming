library(rpart)
library(rpart.plot)
library(caret)
library(e1071)

bank.df <- read.csv("UniversalBank.csv") 

#Remove the ID and zip code
bank.df <- bank.df[ , -c(1, 5)]
View(bank.df)

#Create a training and validation partition
numberOfRows <- nrow(bank.df)
set.seed(1)
train.index <- sample(numberOfRows, numberOfRows*0.6)
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]
View(train.df)
View(valid.df)

# create a classification tree
.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class", maxdepth = 5)

# plot tree
prp(.ct, type = 1, extra = 1, under = FALSE, split.font = 1, varlen = -10)


# classify records in the validation data using the classification tree.
# set argument type = "class" in predict() to generate predicted class membership.
ct.pred <- predict(.ct, valid.df, type = "class")

# generate confusion matrix for training data
confusionMatrix(ct.pred, as.factor(valid.df$Personal.Loan))

# build a deeper classification tree
max.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class", cp = 0, minsplit = 1, maxdepth = 30)

# count number of leaves
length(max.ct$frame$var[max.ct$frame$var == "<leaf>"])

# plot tree
prp(max.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(max.ct$frame$var == "<leaf>", 'gray', 'white'))  

# classify records in the validation data.
# set argument type = "class" in predict() to generate predicted class membership.
max.pred <- predict(max.ct, valid.df, type = "class")

# generate confusion matrix for training data
confusionMatrix(max.pred, as.factor(valid.df$Personal.Loan))
### repeat the code for the validation set, and the deeper tree


# Create code to prune the tree
# xval refers to the number of partitions to use in rpart's built-in cross-validation
# procedure argument.  With xval = 5, bank.df is split into 5 partitions of 1000
# observations each.  A partition is selected at random to hold back for validation 
# while the remaining 4000 observations are used to build each split in the model. 
# Process is repeated for each parition and xerror is calculated as the average error across all partitions.
# complexity paramater (cp) sets the minimum reduction in complexity required for the model to continue.
# minsplit is the minimum number of observations in a node for a split to be attempted.
cv.ct <- rpart(Personal.Loan ~ ., data = bank.df, method = "class", 
               control = rpart.control(cp = 0.00001, minsplit = 5, xval = 5))

# use printcp() to print the table. 
printcp(cv.ct)
prp(cv.ct, type = 1, extra = 1, split.font = 1, varlen = -10)  

#prune the tree using the lowest value for xerror
#Note: the prune function requires cp as a parameter so we need to get cp for lowest value of xerror
pruned.ct <- prune(cv.ct, 
                   cp = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"])

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

 
