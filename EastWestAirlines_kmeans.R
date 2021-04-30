#import EastWestAirlinesCluster
library(readxl)
EastWestAirlinescluster <- read_excel("EastWestAirlinesCluster.xlsx")
View(EastWestAirlinescluster)

EastWestAirlines.df <- data.frame(EastWestAirlinescluster)

# set row names to the ID# column
row.names(EastWestAirlines.df) <- EastWestAirlines.df[,1]
View(EastWestAirlines.df)

# remove the ID# column
EastWestAirlines.df <- EastWestAirlines.df[,-1]

# normalize EastWestAirlines.df variables so all variables have mean of 0 and sd of 1
EastWestAirlines.df.norm <- scale(EastWestAirlines.df, center = TRUE, scale = TRUE)
View(EastWestAirlines.df.norm)

#Create an elbow chart to select the best k
#Elbow chart calculates the weighted sum of squares for each cluster.
#Look for change in slope from steep to flat
library(factoextra)
fviz_nbclust(EastWestAirlines.df.norm, kmeans, method = "wss") + theme_minimal() + ggtitle("Elbow Chart")

#set k = 8 to create 8 clusters, from the elbow chart
kmu1 <- kmeans(EastWestAirlines.df.norm, 8,nstart =  100)

#create a plot of the clusters using fviz.
fviz_cluster(kmu1, data = EastWestAirlines.df.norm)

# show cluster membership
View(kmu1$cluster)
write.csv(kmu1$cluster, "clusters1.csv")

# get centroids, CLUSTER CENTERS FOR EACH VAIRABLE
View(kmu1$centers)
write.csv(kmu1$centers, "clustercenters1.csv")


# plot an empty scatter plot
plot(c(0), xaxt = 'n', ylab = "", type = "l", 
     ylim = c(min(kmu1$centers), max(kmu1$centers)), xlim = c(0, 11))

# label x-axes
axis(1, at = c(1:11), labels = names(EastWestAirlines.df))


# plot centroids
for (i in c(1:8))
  lines(kmu1$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 3, 5),
                                                        "black", "dark grey"))

# name clusters
text(x = 0.5, y = kmu1$centers[, 1], labels = paste("Cluster", c(1:8)))


#distance between centers
dist(kmu1$centers)

#sum of squares within clusters
kmu1$withinss

set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
data <- EastWestAirlines.df.norm
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=100,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#Validating clusters
km8 <- kmeans(EastWestAirlines.df.norm,8)
km1 <- kmeans(EastWestAirlines.df.norm,1)
ratio1 <- km8$tot.withinss/km1$tot.withinss
ratio1


#Hierarchical WITH 95% DATA

#import EastWestAirlinesCluster
library(readxl)
EastWestAirlinescluster <- read_excel("EastWestAirlinesCluster.xlsx")
View(EastWestAirlinescluster)

EastWestAirlines.df <- data.frame(EastWestAirlinescluster)

# set row names to the ID# column
row.names(EastWestAirlines.df) <- EastWestAirlines.df[,1]
View(EastWestAirlines.df)

# remove the ID# column
EastWestAirlines.df <- EastWestAirlines.df[,-1]

#remove 5% of data code#3

indices95 <-EastWestAirlines.df[sample(nrow(EastWestAirlines.df),3799),]
View(indices95)

# normalize EastWestAirlines.df variables so all variables have mean of 0 and sd of 1
EastWestAirlines.df.norm <- scale(indices95, center = TRUE, scale = TRUE)
View(EastWestAirlines.df.norm)


# calculate normalized distance based on all 11 variables  
dist.norm <- dist(EastWestAirlines.df.norm, method = "euclidean")
m <- as.matrix(dist.norm)
View(m)

#Hierarchical clustering using "single" distance measure
hcsingle <- hclust(dist.norm, method = "single")
#display dendogram of hc1
plot(hcsingle, hang = -1, ann = FALSE)
#
plot(hcsingle,cex = 0.8)
rect.hclust(hcsingle,k=8,border = "red")
abline(h = 8, col = 'red')


#assign observations to '8' clusters using splits depicted in map
memb_single <- cutree(hcsingle, k = 8)
View(memb_single)
write.csv(memb_single, "hcsingle95.csv")

#Hierarchical clustering using "average" distance measure
hcavg <- hclust(dist.norm, method = "average")

#Plot
plot(hcavg, hang = -1, ann = FALSE)

install.packages('package_name', dependencies = TRUE)
suppressPackageStartupMessages(library(dendextend))
average_dend_obj <- as.dendrogram(hcavg)
average_col_dend <- color_branches(average_dend_obj, h = 8)
plot(average_col_dend)


#assign observations to '8' clusters using splits depicted in map
memb_average <- cutree(hcavg, k = 8)
View(memb_average)
write.csv(memb_average, "hcaverage.csv")


#Cluster centroids for each variables for average linkage and K = 8 
membership <- as.matrix(memb_average)
membership <- data.frame(membership)
names(membership) <- c("cluster")

mydata1 <- data.frame(EastWestAirlines.df.norm, membership$cluster)
temp <- aggregate(mydata1, by = list(membership$cluster), FUN = mean)
temp2 <- t(temp)
View(temp2)


# label clusters and add company name
row.names(EastWestAirlines.df.norm) <- paste(memb_average, ": ", row.names(EastWestAirlines.df.norm), sep = "")
View(EastWestAirlines.df.norm)

# plot heatmap 
# rev() reverses the color mapping to large = dark
heatmap(as.matrix(EastWestAirlines.df.norm), Colv = NA, hclustfun = hclust, 
        col=rev(paste("gray",1:99,sep="")))
