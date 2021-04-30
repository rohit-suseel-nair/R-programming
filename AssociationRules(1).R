#load required libraries
library(Matrix)
library(arules)

#open input data file
all.books.df <- read.csv("CharlesBookClub.csv")
View(all.books.df)

#view structure of all.books.df
class(all.books.df)
str(all.books.df)

# create a binary incidence matrix
# remove variables that don't contain purchase information
count.books.df <- all.books.df[, c(8:18)]
View(count.books.df)

# change purchase columns to binary yes/no
incid.books.num <- ifelse(count.books.df > 0, 1, 0)
View(incid.books.num)
class(incid.books.num)
str(incid.books.num)

#  coerce the binary incidence matrix/array into a 'transactions' object
books.trans <- as(incid.books.num, "transactions")

#view the structure of the newly created transactions object.
#this is not necessary to execute the code but may help understand what is going on
class(books.trans)
str(books.trans)
books.ngCMatrix <- books.trans@data
class(books.ngCMatrix)
str(books.ngCMatrix)

# plot data and list
itemFrequencyPlot(books.trans)
benchmark_confidence <- itemFrequency(books.trans)
write(benchmark_confidence, file = "bechmark.csv", sep = ",")
View(benchmark_confidence)


# run apriori function
rules_2 <- apriori(books.trans, 
                 parameter = list(supp= 0.05, conf = 0.5, maxlen = 2, target = "rules"))

rules_3 <- apriori(books.trans, 
                 parameter = list(supp= 0.05, conf = 0.5, maxlen = 3, target = "rules"))

rules_10 <- apriori(books.trans, 
                 parameter = list(supp= 0.001, conf = 0.5, target = "rules"))

# inspect and write rules as an output file
inspect(sort(rules_2, by = "lift"))
write(rules_2, file = "book_club_rules_2.csv", sep = ",")


# inspect and write rules as an output file
inspect(sort(rules_3, by = "lift"))
write(rules_3, file = "book_club_rules_3.csv", sep = ",")


# inspect and write rules as an output file
inspect(sort(rules_10, by = "lift"))
write(rules_10, file = "book_club_rules_10.csv", sep = ",")

