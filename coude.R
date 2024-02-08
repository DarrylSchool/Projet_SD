#y <- numeric(0)
#for(k in 2:16){
#  res <- kmeans(X1_scale, k)
#  y[k-2] <- res$tot.withinss / res$totss
#  k
#}
#y
#
#plot(2:15, y, type = "l", lwd = 2, pch = 16, xlab = "Number of Clusters (k)", ylab = "Proportion of Variance Explained (PVE)",
#     main = "Elbow Method for Optimal k")


wss_values <- numeric(length = 9)  # To store the within-cluster sum of squares values

for (k in 2:15) {
  kmeans_result <- kmeans(X1_scale, centers = k, nstart = 15)
  wss_values[k - 1] <- kmeans_result$tot.withinss
}
# print("2")
# Plot the elbow graph to determine the optimal number of clusters
fviz_nbclust(X1, kmeans, method = "wss") + 
  geom_vline(xintercept = which.min(wss_values), linetype = 2) + 
  labs(title = "Elbow Method to Determine Optimal k (Number of Clusters)",
       x = "Number of Clusters (k)",
       y = "Total Within-Cluster Sum of Squares (WSS)")