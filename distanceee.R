###### Euclidean Distance ######

# First create a 2*2 matrix A, where each row represent an observation point
A <- matrix(c(1.7, 5, 4, 72), 2, 2, byrow = T) 

# Calculate euclidean distance
eDistance <- function(x,y) sqrt(sum((x-y)^2))

distanceE <- eDistance(A[1,], A[2,])

# Print answer
print(distanceE)

###### Generate a Fake Dataset ######

set.seed(5)
x <- matrix(rnorm(300), 150, 2) # The dataset contains 150 observations, each of which is described by a two-element vector
xmean <- matrix(rnorm(8), 4, 2) # Generate a deviation matrix
which <- sample(1:4, 150, replace = T) # Randomly sample of number 1-4 for 150 times with replacement
x <- x + xmean[which, ] # Add the deviation to dataset in four randomly formed groups
plot(x, col = which, pch = 19) # Plot the distribution of the observation points

###### K-Means Clustering ######

# Slightly complicated solution #
x.cluster <- cbind(x, which) # Add the randomly sampled 1-4 (a new column) to the dataset as the initial clusters (in this case, we specify 4 clusters)
while(TRUE){
  centroid <- c() # Create an empty vector to put the centroids
  for(g in 1:4){ # In this for loop, the position of each centroid is calculated - mean of observations assigned to each centroid
    centroid <- c(centroid, mean(x.cluster[x.cluster[, 3] == g, 1]), mean(x.cluster[x.cluster[, 3] == g, 2]))
  }
  centroid <- matrix(centroid, 4, 2, byrow = T) # Reform the vector into a matrix where each row is one centroid
  distance <- c()
  for(i in 1:nrow(x)){ # In this for loop, each observation is reassigned its closest centroid
    for(j in 1:4){ # First to calculate the Euclidean distance between each observation and each centroid
      dis <- eDistance(centroid[j,], x[i,])
      distance <- c(distance, dis)
    }
  }
  distance <- matrix(distance, 150, 4, byrow = T) # Create the matrix where each row is an observation and each column is its distance to the four centroids
  centroid.label <- apply(distance, 1, which.min) # Choose the centroid with the shortest distance
  if(all(centroid.label == x.cluster[, 3])){ # If the centroid is not changing any more for each observation, stop the iteration
    km.clusters <- centroid.label
    centroid.matrix <- centroid
    break
  }else{ # Otherwise, assign the new centroids to the dataset, and continue the iteration
    x.cluster[, 3] <- centroid.label
  }  
}
# 4 points
plot(x, col = x.cluster[,3], pch = 19) # Plot the clustered observation points 
points(centroid.matrix, pch = 19, col = 6, cex = 2) # Add centroids to the plot

##### Easy Solution for k-means ######
km.out <- kmeans(x, 4)
km.out
plot(x, col = km.out$cluster, pch = 19)
points(km.out$centers, pch = 19, col = 6, cex = 2)

# 3 clusters with kmeans function and plot the clusters
km.out3 <- kmeans(x, 3)
km.out3
plot(x, col = km.out3$cluster, pch = 19)
points(km.out3$centers, pch = 19, col = 6, cex = 2)

# 5 clusters with kmeans function and plot the clusters
km.out5 <- kmeans(x, 5)
km.out5
plot(x, col = km.out5$cluster, pch = 19)
points(km.out5$centers, pch = 19, col = 6, cex = 2)


# Plot original data and clustered data together #
par(mfcol = c(1, 2)) # create plotting structure of two plots in a row
plot(x, col = which, pch = 19) # plot observations with randomly assigned clusters
plot(x, col = centroid.label, pch = 19) # plot observations after clustering
dev.off() # quit the plotting structure settings

###### K-medoids clustering ######

library(cluster)
# generate a dissimilarity matrix for all the data points in data x, using the Euclidean distance.
dismat <- dist(x, method = "euclidean", diag = FALSE, upper = FALSE)
# run the pam() function with 3 clusters using the dismat generated above
med <- pam(dismat, 3)
# extract the clusters for all data points and the medoids from the outputs of med.
clusters <- med$clustering
medoids <- med$medoids
# plot the clustered data points, where color is decided by the clusters, and add the resulted medoids 
plot(x, col = clusters, pch = 19)
points(x[medoids,], pch=19, col=6, cex=2)

###### Hierarchical Clustering ###### 

# Euclidean distance function 1:
ed1 <- c()
for(i in 1:nrow(x)){
  for(j in 1:nrow(x)){
    if(i < j){
      ed <- eDistance(x[i,], x[j,])
      ed1 <- c(ed1, ed)
    }
  }
}

# Euclidean distance function 2:
ed2 <- dist(x, method = "euclidean") # the default method

# Plot Cluster Dendrogram #
# First take out a subset of the original observations. In this case, we randomly select 20 observations for the purpose of clear demonstration in the dendrogram
where <- sample(1:nrow(x), 20, replace = F)
x.part <- x[where, ]
# Then calculate the inter-observation distances
y <- dist(x.part) # use either method 1 or 2 to calculate the Euclidean distance
# Function for hierarchical clustering
hc.complete <- hclust(y, method = "complete")
# Plot the dendrogram
plot(hc.complete)

# Single with hclust function and plot the dendrogram
where <- sample(1:nrow(x), 20, replace = F)
x.part <- x[where, ]
# Then calculate the inter-observation distances
y <- dist(x.part) # use either method 1 or 2 to calculate the Euclidean distance
# Function for hierarchical clustering
hc.single <- hclust(y, method = "single")
# Plot the dendrogram
plot(hc.single)

where <- sample(1:nrow(x), 20, replace = F)
x.part <- x[where, ]
# Calculate the inter-observation distances
y <- dist(x.part)
# Function for hierarchical clustering
hc.average <- hclust(y, method = "average")
# Plot the dendrogram
plot(hc.average)

########################################### END ##############################################

