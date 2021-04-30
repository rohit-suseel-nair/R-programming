library(tidyverse)
view(diamonds)

summary(diamonds)

diamonds_df <- diamonds

# additional summary statistics using sapply to compute mean, standard dev., min, max, median, length, and missing values for all
# variables

#get numeric variables only
#c() function combines values into a vector or list. Elements of a vector are all the same type.  Datatypes for lists may differ.
#[] a number inside square brackets of a dataframe refers to a column
#a vector of numbers inside square brackets after a dataframe refers to multiple columns. 
# a vector of column names can also be used.
df <- diamonds[,c(1,5:10)]
View(df)

df <- diamonds[c("carat", "depth", "table", "price", "x", "y", "z")]
View(df)


#lapply and sapply applies a function over a list of items
#sapply simplifies output

summary_stats <- data.frame(
                            mean=sapply(df, mean), 
                            median=sapply(df, median), 
                            sd=sapply(df, sd), 
                            variance=sapply(df, var),
                            min=sapply(df, min), 
                            max=sapply(df, max), 
                            count=sapply(df, length),
                            miss.val=sapply(df, function(x) sum(length(which(is.na(x)))))
                            )
print(summary_stats)
View(summary_stats)


#histogram of price
library(ggplot2)
ggplot(diamonds) + geom_histogram(aes(x = cut))

ggplot(diamonds) + geom_histogram(aes(x = price), binwidth = 500)

??factor
help(factor)
color_factor <- as.factor(diamonds$color)
print(color_factor)
str(color_factor)

#does the same thing
color_factor <- diamonds$color
print(color_factor)
str(color_factor)

help(diamonds)

# boxplot plot of color and price
ggplot(diamonds) + geom_boxplot(aes(x = as.factor(color), y = price)) + xlab("Color")

#does the same thing
ggplot(diamonds) + geom_boxplot(aes(x = carat, y = price)) + xlab("Carat")

# boxplot plot of cut and price
ggplot(diamonds) + geom_boxplot(aes(x = as.factor(cut), y = price)) + xlab("Cut")

# scatter plot of carat and price alpha sets opacity
library(ggplot2)
ggplot(diamonds) + geom_point(aes(x = carat, y = price), color = "navy", alpha = 0.2)

# scatter plot of cut and price
library(ggplot2)
ggplot(diamonds) + geom_point(aes(x = cut, y = price), color = "navy", alpha = 0.2)

# scatter plot of color and price
library(ggplot2)
ggplot(diamonds) + geom_point(aes(x = color, y = price), color = "navy", alpha = 0.2)

# scatterplot matrix
plot(diamonds[, c(2, 1, 3, 7)])

diamonds[1,]

library(GGally)
ggpairs(diamonds[c(1:10), c(2, 1, 3, 7)])


# correlation matrix for numerical variables only
library(ggplot2)
library(reshape) # to generate input for the plot
cor.mat <- round(cor(df),2) # rounded correlation matrix
melted.cor.mat <- melt(cor.mat)
ggplot(melted.cor.mat, aes(x = X1, y = X2, fill = value)) + 
  geom_tile() + 
  geom_text(aes(x = X1, y = X2, label = value))


#get the version and a list of all installed packages
version
.packages(all.available = TRUE)
installed.packages()   #detaild pacakge info

