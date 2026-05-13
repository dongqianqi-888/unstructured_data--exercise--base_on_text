#-----------------------------------------------------------------------------
# k-means, hierarchical and HDBS model for health
dtmH <- DocumentTermMatrix(docsH) 

#Present text data numerically, weighted TF-IDF
dtmH.tfidf <- weightTfIdf(dtmH)
dtmH.tfidf <- removeSparseTerms(dtmH.tfidf, 0.999)
dtmH.matrix <- as.matrix(dtmH.tfidf) 

# Cosine distance matrix (useful for specific clustering algorithms) 
library(proxy)
distH.matrix <- dist(dtmH.matrix, method = "cosine")

K=10
#Perform clustering
library(dbscan)
clustering.kmeans <- kmeans(distH.matrix,K) 
clustering.hierarchical <- hclust(distH.matrix, method = "ward.D2") 
clustering.dbscan <- hdbscan(distH.matrix, minPts= 2)

library(cluster)
clusplot(as.matrix(distH.matrix),clustering.kmeans$cluster,color=T,shade=T,labels=2,lines=0)
plot(clustering.hierarchical)
rect.hclust(clustering.hierarchical,10)
plot(as.matrix(distH.matrix)[,c(1,2)],col=clustering.dbscan$cluster+1L)

#Combine results
master.cluster <- clustering.kmeans$cluster
slave.hierarchical <- cutree(clustering.hierarchical,k = K) 
slave.dbscan <- clustering.dbscan$cluster

#plotting results
library(colorspace)
points <- cmdscale(distH.matrix, k = 5) 
palette <- diverge_hcl(K) # Creating a colorpalette, need 
library(colorspace)
#layout(matrix(1:3,ncol=1))
par(mfrow = c(1, 3))
plot(points, main = 'K-Means clustering', col = as.factor(master.cluster), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
plot(points, main = 'Hierarchical clustering', col= as.factor(slave.hierarchical), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),  
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
plot(points, main = 'Density-based clustering', col = as.factor(slave.dbscan), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
table(master.cluster)
table(slave.hierarchical)
table(slave.dbscan)

#Elbow plot
#accumulator for cost results
cost_df <- data.frame()
#run kmeans for all clusters up to 100
for(i in 1:20){
  #Run kmeans for each level of i, allowing up to 100 iterations for convergence
  kmeans<- kmeans(x=dtmH.matrix, centers=i, iter.max=100)
  #Combine cluster number and cost together, write to df
  cost_df<- rbind(cost_df, cbind(i, kmeans$tot.withinss))
}
names(cost_df) <- c("cluster", "cost")
plot(cost_df$cluster, cost_df$cost)
lines(cost_df$cluster, cost_df$cost)


##########################################################
# k-means, hierarchical and HDBS model for financial issue
dtmF <- DocumentTermMatrix(docsF) 

#Present text data numerically, weighted TF-IDF
dtmF.tfidf <- weightTfIdf(dtmF)
dtmF.tfidf <- removeSparseTerms(dtmF.tfidf, 0.999)
dtmF.matrix <- as.matrix(dtmF.tfidf) 

# Cosine distance matrix (useful for specific clustering algorithms) 
library(proxy)
distF.matrix <- dist(dtmF.matrix, method = "cosine")

K=15
#Perform clustering
library(dbscan)
clustering.kmeans <- kmeans(distF.matrix,K) 
clustering.hierarchical <- hclust(distF.matrix, method = "ward.D2") 
clustering.dbscan <- hdbscan(distF.matrix, minPts= 2)

library(cluster)
clusplot(as.matrix(distF.matrix),clustering.kmeans$cluster,color=T,shade=T,labels=2,lines=0)
plot(clustering.hierarchical)
rect.hclust(clustering.hierarchical,15)
plot(as.matrix(distF.matrix)[,c(1,2)],col=clustering.dbscan$cluster+1L)

#Combine results
master.cluster <- clustering.kmeans$cluster
slave.hierarchical <- cutree(clustering.hierarchical,k = K) 
slave.dbscan <- clustering.dbscan$cluster

#plotting results
library(colorspace)
points <- cmdscale(distF.matrix, k = 5) 
palette <- diverge_hcl(K) # Creating a colorpalette, need 
library(colorspace)
#layout(matrix(1:3,ncol=1))
par(mfrow = c(1, 3))
plot(points, main = 'K-Means clustering', col = as.factor(master.cluster), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
plot(points, main = 'Hierarchical clustering', col= as.factor(slave.hierarchical), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),  
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
plot(points, main = 'Density-based clustering', col = as.factor(slave.dbscan), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
table(master.cluster)
table(slave.hierarchical)
table(slave.dbscan)

#Elbow plot
#accumulator for cost results
cost_df <- data.frame()
#run kmeans for all clusters up to 100
for(i in 1:20){
  #Run kmeans for each level of i, allowing up to 100 iterations for convergence
  kmeans<- kmeans(x=dtmF.matrix, centers=i, iter.max=100)
  #Combine cluster number and cost together, write to df
  cost_df<- rbind(cost_df, cbind(i, kmeans$tot.withinss))
}
names(cost_df) <- c("cluster", "cost")
plot(cost_df$cluster, cost_df$cost)
lines(cost_df$cluster, cost_df$cost)

