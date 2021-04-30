library(readxl)
netflix.df <- read_excel("Netflix.xlsx")
View(netflix.df)
str(netflix.df)

#add customerID as a row label store row names and col names in a vector for later use
row.names <- netflix.df[,1]
col.names <- names(netflix.df)

#calculate correlation between items to get item based recommendations. remove first column as it is customerID
ibcf <- cor(netflix.df[2:10], method = c("pearson"), use = c("pairwise.complete.obs"))
print(ibcf)


#load recommendar lab library to calculate user based collaborative filter
library(recommenderlab)
m <- as.matrix(netflix.df[2:10])
str(m)
r <- as(m, "realRatingMatrix")
str(r)


#calculate simialrities between and among users for user based collaborative filter
user_sim <- similarity(r, method = "pearson", which = "user")
print(user_sim)


# user-rating based recommendation for user 4
# will compute the predicted ratings of 3 movies that are the closest from amongst the movies not watched by user 4
# the ratings will be predicted by taking the weighted average of movie ratings for similar users
# r[4,] stands for the 4th row/user, n = 3 stands for 3 neighbours, type = "topNList" stands for the closest n neighbours
UB.Rec <- Recommender(r, "UBCF")
pred <- predict(UB.Rec, r[4,], n = 3, type = "topNList")
as(pred, "matrix")




