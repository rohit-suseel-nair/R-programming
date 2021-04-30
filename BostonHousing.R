library(forecast) 
library(leaps)
library(ggplot2)

# open BostonHousing.csv
BostonHousing.df <- read.csv("BostonHousing.csv")
View(BostonHousing.df)

set.seed(1) # SAMPLE function will draw random usually
# but if u set seed , then U R TELLING R THAT U WANT TO RETURN SAME EXCAT RECORDS, 
# BUT IT WILL ..
numberOfRows <- nrow(BostonHousing.df)
train.index <- sample(numberOfRows, numberOfRows*0.6) # IMPORTANT TO SET THE PERCENTAGE(0.6)
#

train.df <- BostonHousing.df[train.index,]
validation.df <- BostonHousing.df[-train.index,]

housePrice1.lm <- lm(formula = MEDV ~ RM, data = train.df) #LM HERE WE ARE SUPPLYING ONLY
#TWO VARIABLES, FORMULA IS 'Y' WHICH IS MEDV IN THIS CASE, GIVE THE SAME COLUMN NAME
# HERE WE ARE USING THE TRAINING SET
housePrice2.lm <- lm(formula = MEDV ~ CRIM, data = train.df)
housePrice3.lm <- lm(formula = MEDV ~ RM + CRIM, data = train.df)

options(scipen = TRUE)
summary(housePrice1.lm) # THE RESIDUALS GIVES, SUMMARY STATS OF THE ERRORS THAT WE HAVE
#R SQUARED = 0.5612 LOOKS DIFFERENT FRO THAT OF EXCEL, COS THE TRAINING PARTITION
# IS DIFFERENT HERE
summary(housePrice2.lm)
summary(housePrice3.lm)


#NOW WE
#WANT TO KNOW HOW WELL IT PREDICTS FOR A NEW OBSERVATION
#RMS = WE SHOULD FOCUS ON THIS, 
housePrice1.pred <- predict(housePrice1.lm, validation.df)
accuracy(housePrice1.pred, validation.df$MEDV)# THIS COMPARES THE PREDICTED MODEL WITH ACTUAL
#rmse IS IMPORTANT IN THE OUTPUT, IT IS OFF BY AN AVG OF 7.3$.IS IT GOOD OR BAD?
#WE KNOW THAT LOWER ERROR IS BETTER BUT IS 7.3$ BETTER, THAT IS IS IT LOWER?
# SO CHECK FOR MIN AND MAX AND AVG VALUE OF MEDV, SO THE AVG IS 22THOUSAND$ BUT U HAVE AN 
# ERROR OF 7.3$
# SO THATS A CONSIDERABLE ERROR, UNIT WILL BE EXACT SAME AS DEPENDENT VARIABLE
# IN EXCEL IT IS 5.6842

housePrice2.pred <- predict(housePrice2.lm, validation.df)
accuracy(housePrice2.pred, validation.df$MEDV)

housePrice3.pred <- predict(housePrice3.lm, validation.df)
accuracy(housePrice3.pred, validation.df$MEDV)

housePrice.lm <- lm(formula = MEDV ~ ., data = train.df[-14])
summary(housePrice.lm)
housePriceAll.pred <- predict(housePrice.lm, validation.df)
accuracy(housePriceAll.pred, validation.df$MEDV)

# stepwise regression
# uses AIC by default to select variables
housePrice.lm.step <- step(housePrice.lm, direction = "forward")
summary(housePrice.lm.step)
housePrice.lm.step.pred <- predict(housePrice.lm.step, validation.df)
accuracy(housePrice.lm.step.pred, validation.df$MEDV)

all.residuals <- validation.df$MEDV - housePrice.lm.step.pred
all.residuals.df <- data.frame(all.residuals)
ggplot(all.residuals.df) + geom_histogram(aes(x = all.residuals), binwidth = 1)

