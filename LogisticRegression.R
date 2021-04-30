library(gains)
library(caret)
library(ROCR)

ridingmowers.df <- read.csv("RidingMowers.csv")
View(ridingmowers.df)
 
ridingmowers.df$Ownership <- factor(ridingmowers.df$Ownership,levels = c("Owner","Nonowner"),labels = c(1,0))
set.seed(1000)
numofrows <- nrow(ridingmowers.df)
View(numofrows)
trainds.index <- sample(24,24*0.6)
trainds.df <- ridingmowers.df[trainds.index,]
validds.df <- ridingmowers.df[-trainds.index,]
View(trainds.index)
View(trainds.df)

View(validds.df)

trainds.df <- trainds.df[,c(1,2,3)]

validds.df<-validds.df[c(1-10),c(1,2,3)]


logi.reg <- glm(Ownership~.,data = trainds.df,family = "binomial")
summary(logi.reg)
#general linear model
log.reg <- glm(Ownership ~ . ,data = trainds.df, family = "binomial")
options(scipen=999)
summary(log.reg)

log.reg.pred <- predict(log.reg, validds.df,type = "response")
View(log.reg.pred)

pred.df<- data.frame(actual = validds.df$Ownership,predicted = log.reg.pred)
View(pred.df)

pred <- prediction(pred.df$predicted,pred.df$actual)
perf <- performance(pred,"tpr","fpr")
plot(perf)
                    
help("confusionMatrix")
?confusionMatrix()
View(validds.df$Ownership)
confusionMatrix(table(predict(log.reg,newdata = validds.df,type="response")>=0.30,validds.df$Ownership == 1))

gain <- gains(as.numeric(validds.df$Ownership[1:10]),log.reg.pred[1:10],groups = 2)                
View(gain)
                
plot(c(0,gain$cume.pct.of.total*sum(as.numeric(validds.df$Ownership[1:10]))~c(0,gain$cume.obs),
       xlab="# cases", ylab="Cumulative", main="", type="l"))
lines(c(0,sum(valid.df$Personal.Loan))~c(0, dim(valid.df)[1]), lty=2)

                
exp(coef(log.reg))                
exp(cbind(OR = coef(log.reg), confint(log.reg)))

odds <- exp(28.14728-0.06299*60.0-1.24586*20)
print(odds)

#predict whether 60k and 20000 lot sze can be categorized as owner
df <- data.frame("Income"=60.0,"Lot_Size"=20.0)
predict.df<-predict(log.reg,newdata = df,type="response")
print(predict.df)               
