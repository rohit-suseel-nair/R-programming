getwd()

#if needed, use setwd() to set the working directory
#can also use menu option "Session - Set Working Directory
#setwd("/Users/markthouin/Dropbox/MIS 6356 BA with R/Fall 2019/Data Exploration")

## Boston housing data
housing.df <- read.csv("BostonHousing.csv")
View(housing.df)

#Basic summary statistics
summary(housing.df) 


# additional summary statistics using sapply to compute mean, standard dev., min, max, median, length, and missing values for all
# sapply applies a function to all values of X
help(sapply)
bh_stats_df <- data.frame(  mean=sapply(housing.df, mean), 
                         median=sapply(housing.df, median), 
                         sd=sapply(housing.df, sd), 
                         variance=sapply(housing.df, var),
                         min=sapply(housing.df, min), 
                         max=sapply(housing.df, max), 
                         count=sapply(housing.df, length),
                         miss.val=sapply(housing.df, function(x) 
                         sum(length(which(is.na(x))))))


## histogram of MEDV
hist(housing.df$MEDV, xlab = "MEDV")

# histogram with ggplot binwidth = 10 and 1
# + symbol is used when a function is expecting additional commands
library(ggplot2)
ggplot(housing.df) + geom_histogram(aes(x = MEDV), binwidth = 10)

ggplot(housing.df) + geom_histogram(aes(x = MEDV), binwidth = 1)


## boxplot of MEDV for different values of CHAS
boxplot(housing.df$MEDV ~ housing.df$CHAS, xlab = "CHAS", ylab = "MEDV")

# boxplot plot with ggplot
ggplot(housing.df) + geom_boxplot(aes(x = as.factor(CHAS), y = MEDV)) + xlab("CHAS")

## side-by-side boxplots
# use par() to split the plots into panels.
par(mfcol = c(2, 2))
boxplot(housing.df$NOX ~ housing.df$CAT..MEDV, xlab = "CAT.MEDV", ylab = "NOX")
boxplot(housing.df$LSTAT ~ housing.df$CAT..MEDV, xlab = "CAT.MEDV", ylab = "LSTAT")
boxplot(housing.df$PTRATIO ~ housing.df$CAT..MEDV, xlab = "CAT.MEDV", ylab = "PTRATIO")
boxplot(housing.df$INDUS ~ housing.df$CAT..MEDV, xlab = "CAT.MEDV", ylab = "INDUS")

# scatter plot with axes names
par(mfcol = c(1, 1))
plot(housing.df$MEDV ~ housing.df$LSTAT, xlab = "MDEV", ylab = "LSTAT")

# scatter plot with ggplot
library(ggplot2)
ggplot(housing.df) + geom_point(aes(x = LSTAT, y = MEDV), colour = "navy", alpha = 0.7)

# scatter plot with color
library(ggplot2)
ggplot(housing.df, aes(y = NOX, x = LSTAT, color= CAT..MEDV)) +
  geom_point(alpha = 0.5) 


## scatter plot matrix
# use plot() to generate a matrix of 4X4 panels with variable name on the diagonal, 
# and scatter plots in the remaining panels.
plot(housing.df[, c(1, 3, 12, 13)])

# scatter plot matrix with GGally 
library(GGally)
ggpairs(housing.df[, c(1, 3, 12, 13)])

# correlation maatrix with ggplot
library(ggplot2)
library(reshape) # to generate input for the plot
cor.mat <- round(cor(housing.df),2) # rounded correlation matrix
melted.cor.mat <- melt(cor.mat)
ggplot(melted.cor.mat, aes(x = X1, y = X2, fill = value)) + 
  geom_tile() + 
  geom_text(aes(x = X1, y = X2, label = value))


