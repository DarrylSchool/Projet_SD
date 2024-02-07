y <- numeric(0)
for(k in 2:16){
  res <- kmeans(X1_scale, k)
  y[k-2] <- res$tot.withinss / res$totss
  k
}
y

plot(2:15, y, type = "l", lwd = 2, pch = 16, xlab = "Number of Clusters (k)", ylab = "Proportion of Variance Explained (PVE)",
     main = "Elbow Method for Optimal k")
