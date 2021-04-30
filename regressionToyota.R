
# open ToyotaCorolla.csv
car.df <- read.csv("ToyotaCorolla.csv")
View(car.df)

# select variables for regression e.g. c(c, 4:9) add 11 to get colors
selected.var <- c(3, 4, 7, 8, 9, 10, 11, 12, 13, 14, 17, 18)

# partition the data 
set.seed(123)  # set seed for reproducing the partition
numberOfRows <- nrow(car.df)
train.index <- sample(numberOfRows, numberOfRows*0.6)  
train.df <- car.df[train.index, selected.var]
valid.df <- car.df[-train.index, selected.var]
View(train.df)

# use lm() to run a linear regression of Price on all 10 predictors in the
# use . after ~ to include all the remaining columns in train.df as predictors.
car.lm <- lm(Price ~ ., data = train.df)# PERIOD means WE ARE using EVERY VAIRABLE IN THE 
#DATA SET TO PREDICT DEPENDEnt VARIABLE

# use options(scipen = TRUE) to display in scientific notation
# use options(scipen = 999) to not use scientific notation.
#options(scipen = 999)
summary(car.lm) # CHECK SUMMARY IF THE b1 IS NEGATIVE THEN PRICE WILL GO LOWERS
View(car.lm)


# use predict() to make predictions on a new set. 
#options(scipen=999, digits = 0)
car.lm.pred <- predict(car.lm, valid.df)
summary(car.lm.pred)

#calculate rmse by hand to show how it works.  this will typically be done using accuracy()
residuals <- valid.df$Price - car.lm.pred
squaredResiduals <- residuals*residuals
df <- data.frame("Predicted" = car.lm.pred, "Actual" = valid.df$Price,
           "Residual" = residuals, "Squared Residuals" = residuals*residuals)
rmse <- sqrt(mean(df$Squared.Residuals))
View(df)


library(forecast)
library(leaps)

# use accuracy() from forecast package to compute common accuracy measures including rmse.
# From help file (??accuracy) the measures calculated are:
#  ME: Mean Error
#  RMSE: Root Mean Squared Error
#  MAE: Mean Absolute Error
#  MPE: Mean Percentage Error
#  MAPE: Mean Absolute Percentage Error
#  MASE: Mean Absolute Scaled Error
#options(scipen=999, digits = 3)

accuracy(car.lm.pred, valid.df$Price)# RMSE = 1304.966 EUROS BUT AVERAGE IS 10730(CHECK EXCEL PRICE COLUMN AVERAGE)



#plot residuals to examine
hist(df$Residual, breaks = 25, xlab = "Residuals", main = "")

# use regsubsets() in package leaps to run an exhaustive search. 
# regsubsets() will calculate adjusted r2 for every possible combination of predictors
# unlike with lm(), categorical predictors must be turned into dummies manually.


search <- regsubsets(Price ~ ., data = train.df, nbest = 1, nvmax = dim(train.df)[2],
                     method = "exhaustive")
sum <- summary(search)
summary(search)

# show models
sum$which

# show metrics
sum$rsq
sum$adjr2


#par(mfrow=c(1,1))
plot(search, scale="r2")

#best 4
car.best.lm <- lm(Price ~ Age_08_04 + HP + KM + Weight, data = train.df)
summary(car.best.lm)
car.best.lm.pred <- predict(car.best.lm, valid.df)
accuracy(car.best.lm.pred, valid.df$Price)


#library(gains)
library(dplyr)
library(Hmisc)
library(ggplot2)

###plot predicted price vs target price for range of prices
df <- df[order(-df$Actual),] 

df$bin = as.numeric(cut2(df$Actual, g = 21))
table(df$bin)

bin_stats = df %>%
  group_by(bin) %>% summarise(mean_Actual = mean(Actual), mean_Predicted = mean(Predicted), min_Actual = min(Actual), min_Predicted = min(Predicted), max_Actual = max(Actual), max_Predicted = max(Predicted) )

##Plotting actual vs predicted values for Training and Validation data
p1<- ggplot(bin_stats, aes(bin)) + 
  geom_line(aes(y = bin_stats$mean_Predicted, color ="Predicted Price" )) + 
  geom_line(aes(y = bin_stats$mean_Actual, color = "Actual Price")) 

p1 
# CHECK PUTPUT
#SPLIT IT UP INTO 20 BINS # THE EROOR IS GREATER AT HIGH RANGE IN ACTUAL VERSUS PREDICTED


write.csv(df, file = "df.csv")


# HOW DO PICK WHICH PREDICTOR VARIABLES WHEN U HAVE MANY PREDICTOR VARIABLES
# DO AN EXHAUSTIVE SERACH OF ALL COMBIANTIONS TO CHECK FOR RMSE
# BELOW IS THE EXPLANATION
# CHECK FOR BEST SUBSETS FUNC [plot(search, scale="r2")]
# R WILL TAKE A LOOK AT EVRY PREDICTION MODEL FOR EVRRY VARIABLE
# WHICH SINGLE ARIABLE HAS THR HIGHESR R SQUARED
# IT FOUND THAT AGE RESULTED IN THE BEST R SQUARED
# NOW IT WILL DO PAIRS OF VARIABLES
# AGAIN CHECK FOR R SQAURED
# IT SHOWED AGE AND WEIGHT
# NEXT WILL BE FOR THREE VARIABLES AND CHECK FOR R SQUARED AND THEN FOR 4 VARIABLES AND SO ON
# THE TO MOST LINE WILL TELL YOU WHICH VARIABLES

# MODELS WITH HIGHER R SQAURED WILL HAVE BETTER PREDICTABLE ACCUARCY




