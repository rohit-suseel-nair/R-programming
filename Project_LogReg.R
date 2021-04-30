library(ggplot2)
library(magrittr)
library(plotly)
library(caret)
library(ROCR)
library(party)
library(pROC)
library(kableExtra) # nice table html formating 
library(gridExtra)
library(grid)
library(gains)

#import dataset
bankMarketing <- read.csv("C:/Users/JLi82/OneDrive/Documents/syllabus/6356/R scripts and datasets/bank-full.csv")
View(bankMarketing)

############################data prepartion and data exploration#############################################
##structure of the dataset
str(bankMarketing)

##summary provides a quick review of the dataset
summary(bankMarketing)

##show the first 6 rows of the dataset
head(bankMarketing)

##what are these factor variables
str(bankMarketing$job)
levels(bankMarketing$job)
table(bankMarketing$job)
plot(table(bankMarketing$job))
plot(table(bankMarketing$marital))
plot(table(bankMarketing$education))
plot(table(bankMarketing$housing))
plot(table(bankMarketing$loan))
plot(table(bankMarketing$poutcome))
plot(table(bankMarketing$contact))
plot(table(bankMarketing$default))
plot(table(bankMarketing$month))
## missing values
sum(is.na(bankMarketing))

## detecting outliers

boxplot(bankMarketing$age, main = "Age Box plot",
        yaxt = "n", xlab = "Age", horizontal ="TRUE",
        col = terrain.colors(2))

##continus variables, histogram of age, first gives the frequencies, the other gives the density
par(mfcol = c(2, 2))
hist(bankMarketing$age, col = "navy")
hist(bankMarketing$balance,col = "navy")
hist(bankMarketing$duration, col = "navy")
hist(bankMarketing$day, col = "navy")
hist(bankMarketing$campaign,col = "navy")


##Check distribution of target variable, it is observed that there isn't any data imbalance
counts <- table(bankMarketing$y)
barplot(counts,col=c("darkblue","red"),legend = rownames(counts), main = "Term Deposit")

## look at the proporations across two categorical varibales
par(mfcol = c(2, 2))

Edu.Output <- table(bankMarketing$education, bankMarketing$y)
tabEduoutput
plot(Edu.Output)

Marital.Output <- table(bankMarketing$marital, bankMarketing$y)
tabMarioutput
plot(Marital.Output)

Job.Output <- table(bankMarketing$job, bankMarketing$y)
plot(Job.Output)

House.Output <- table(bankMarketing$housing,bankMarketing$y)
plot(House.Output)

Loan.Output <- table(bankMarketing$loan,bankMarketing$y)
plot(Loan.Output)

Default.Output <- table(bankMarketing$default,bankMarketing$y)
plot(Default.Output)

tabEdMar <- table(bankMarketing$education, bankMarketing$marital)
tabEduoutput

## get the proportions- of the entire population
round(prop.table(tabEdMar)*100,3)
mosaicplot(tabEdMar)

  
## relationship between age,job,education,marital status,housing,loan and term deposit histogram  
bankMarketing %>% 
  plot_ly(x = bankMarketing$age, 
          type = 'histogram', 
          color = bankMarketing$y) %>%
  layout(xaxis = list(title="Age"),
         yaxis = list(title="Count"))

bankMarketing %>% 
  plot_ly(x = bankMarketing$job, 
          type = 'histogram', 
          color = bankMarketing$y) %>%
  layout(xaxis = list(title="Job"),
         yaxis = list(title="Count"))

bankMarketing %>% 
  plot_ly(x = bankMarketing$education, 
          type = 'histogram', 
          color = bankMarketing$y) %>%
  layout(xaxis = list(title="Education"),
         yaxis = list(title="Count"))

bankMarketing %>% 
  plot_ly(x = bankMarketing$marital, 
          type = 'histogram', 
          color = bankMarketing$y) %>%
  layout(xaxis = list(title="Marital Status"),
         yaxis = list(title="Count"))

bankMarketing %>% 
  plot_ly(x = bankMarketing$housing, 
          type = 'histogram', 
          color = bankMarketing$y) %>%
  layout(xaxis = list(title="Housing"),
         yaxis = list(title="Count"))

bankMarketing %>% 
  plot_ly(x = bankMarketing$loan, 
          type ='histogram', 
          color = bankMarketing$y) %>%
  layout(xaxis = list(title="Loan"),
         yaxis = list(title="Count"))

bankMarketing %>% 
  plot_ly(x = bankMarketing$contact, 
          type ='histogram', 
          color = bankMarketing$y) %>%
  layout(xaxis = list(title="Contact"),
         yaxis = list(title="Count"))

## side-by-side boxplots
# use par() to split the plots into panels.
par(mfcol = c(2, 2))
boxplot(bankMarketing$age ~ bankMarketing$y, xlab = "Subscription", ylab = "Age")
boxplot(bankMarketing$balance ~ bankMarketing$y, xlab = "Subscription", ylab = "Balance")
boxplot(bankMarketing$day ~ bankMarketing$y, xlab = "Subscription", ylab = "Day")
boxplot(bankMarketing$duration ~ bankMarketing$y, xlab = "Subscription", ylab = "Duration")

# correlation maatrix with ggplot
library(reshape) # to generate input for the plot
bankMarketing.df <- data.frame(bankMarketing[,c(1,6,10,12:15)])
bankMarketing_nor.df <- scale(bankMarketing.df)
cor.mat <- round(cor(bankMarketing_nor.df),2) # rounded correlation matrix
melted.cor.mat <- melt(cor.mat)
ggplot(melted.cor.mat, aes(x = X1, y = X2, fill = value)) + 
  geom_tile() + 
  geom_text(aes(x = X1, y = X2, label = value))

##correlation between variables. This analysis can help us decide if we can drop some predictors depending upon its correlation with the outcome y
library(psych)
pairs.panels(bankMarketing[,c(1:4,17)])
pairs.panels(bankMarketing[,c(5:8,17)])
pairs.panels(bankMarketing[,c(9:12,17)])
pairs.panels(bankMarketing[,c(13:16,17)])

## scatter plot with axes names
par(mfcol = c(1, 1))
plot(bankMarketing$age, bankMarketing$day, xlab = "education", ylab = "Age")

ggplot(bankMarketing) + geom_point(aes(x = age, y = education), colour = "navy", alpha = 0.7)
ggplot(bankMarketing, aes(y = age, x = education, color= y)) +
  geom_point(alpha = 0.5) 

## scatter plot matrix with GGally for numerical variables
library(GGally)
ggpairs(bankMarketing.df)

## want to cretate a summay of all numeric values using a single loop
perc_buckets <- seq(0.05, 0.95, by = 0.05)
colNumeric = NULL
colArray = "Percentile"
bankMarketingDistribution <- data.frame(quantiles = perc_buckets)

for (col_id in 1: length(colnames(bankMarketing)))
{
  colName = colnames(bankMarketing)[col_id]
  if(is.numeric(bankMarketing[,col_id]))
  {
    colArray = c(colArray, colName)
    colNumeric = c(colNumeric, col_id)
    bankMarketingDistribution = cbind(bankMarketingDistribution, quantile(bankMarketing[, col_id], probs = perc_buckets))
  }
}
colnames(bankMarketingDistribution) = colArray
head(bankMarketing[,colNumeric])
bankMarketingDistribution
View(bankMarketingDistribution)

##how the data is distributed across variables
ggplot(bankMarketing, aes(x = education, y = balance)) + geom_boxplot() +
  geom_smooth(method = lm) +
  geom_smooth(method = lm, aes(group =1)) +
  facet_wrap(~ job, scales = "free_y") +
  labs(x = "Education", y = "Balance", title = "Balance by Education Group") +
  theme(axis.text.x = element_text(angle = 90))

## indicates whether the client has accepted the term deposit or not
ggplot(bankMarketing, aes(x = age, y = duration, color = y)) + geom_point() +
  facet_wrap(~ job, scales = "free_y") +
  labs(x = "Age", y = "Duration", title = "Duration by Age Group") +
  theme(axis.text.x = element_text(angle = 90))

## create a 100% stacked bar chart - job vs martital status vs Education
ggplot(bankMarketing, aes(x = job, fill = marital)) + geom_bar(position = "fill") + scale_fill_brewer(palette = "Set2") +
  facet_wrap(~ education, scales = "free_y") +
  labs(x = "Job", title = "Frequency Distribution") +
  theme(axis.text.x = element_text(angle = 90))

######### logistic regression model####################################################
##Recoding 'yes' or 'no' to binary '1' or '0'
bankMarketing$y = ifelse(bankMarketing$y=='yes',1,0)
View(bankMarketing)

## partition data
set.seed(123)
numberOfRows <- nrow(bankMarketing)
train.index <- sample(numberOfRows, numberOfRows*0.7)  
train.df <- bankMarketing[train.index,]
valid.df <- bankMarketing[-train.index,]
View(train.df)
View(valid.df)

## scale numerical data, question--why not pdays and 
train.df[c(1,6,10,12,13,14,15)] = scale(train.df[c(1,6,10,12,13,14,15)])
valid.df[c(1,6,10,12,13,14,15)] = scale(valid.df[c(1,6,10,12,13,14,15)])
View(train.df)

# run logistic regression
# use glm() (general linear model) with family = "binomial" to fit a logistic regression.
logit.reg <- glm(y ~ ., data = train.df, family = "binomial") 
options(scipen=999)
summary(logit.reg)

## evaluate the model
# use predict() with type = "response" to compute predicted probabilities. 
logit.reg.pred <- predict(logit.reg, valid.df[, -17], type = "response") 

##Create a function for plotting distribution
plot_pred_type_distribution <- function(df, threshold) {
  v <- rep(NA, nrow(df))
  v <- ifelse(df$pred >= threshold & df$y == 1, "TP", v)
  v <- ifelse(df$pred >= threshold & df$y == 0, "FP", v)
  v <- ifelse(df$pred < threshold & df$y == 1, "FN", v)
  v <- ifelse(df$pred < threshold & df$y == 0, "TN", v)
  
  df$pred_type <- v
  
  ggplot(data=df, aes(x=y, y=pred)) + 
    geom_violin(fill='black', color=NA) + 
    geom_jitter(aes(color=pred_type), alpha=0.6) +
    geom_hline(yintercept=threshold, color="red", alpha=0.6) +
    scale_color_discrete(name = "type") +
    labs(title=sprintf("Threshold at %.2f", threshold))
}

# plot the prediction distribution
predictions_LR <- data.frame(y = valid.df$y, pred = NA)
predictions_LR$pred <- logit.reg.pred
plot_pred_type_distribution(predictions_LR,0.30)

# first 100 actual and predicted records
difference <- data.frame(actual = valid.df$y[1:100], predicted = logit.reg.pred[1:100])
View(difference)
confusionMatrix(table(predict(logit.reg, newdata = valid.df, type="response") >= 0.3, valid.df$y == 1))

# calculate ROC
rocr.pred.lr = prediction(predictions = logit.reg.pred, labels = valid.df$y)
rocr.perf.lr = performance(rocr.pred.lr, measure = "tpr", x.measure = "fpr")
rocr.auc.lr = as.numeric(performance(rocr.pred.lr, "auc")@y.values)

# print ROC AUC
rocr.auc.lr

# plot ROC curve
plot(rocr.perf.lr,
     lwd = 3, colorize = TRUE,
     print.cutoffs.at = seq(0, 1, by = 0.1),
     text.adj = c(-0.2, 1.7),
     main = 'ROC Curve')
mtext(paste('Logistic Regression - auc : ', round(rocr.auc.lr, 5)))
abline(0, 1, col = "red", lty = 2)

gain <- gains(valid.df$y, logit.reg.pred, groups=10)

# plot lift chart
plot(c(0,gain$cume.pct.of.total*sum(valid.df$y))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="lift chart Logistic Regression", type="l")
lines(c(0,sum(valid.df$y))~c(0, dim(valid.df)[1]), col = "red", lty=2)


# compute deciles and plot decile-wise chart
heights <- gain$mean.resp/mean(valid.df$y)
midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,6), 
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise chart")

# add labels to columns
text(midpoints, heights+0.5, labels=round(heights, 1), cex = 0.8)
