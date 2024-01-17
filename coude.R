y <- c(0,0,0,0)
for(k in 2:6){
  res <- kmeans(X2, k)
  y[k-2] <- res$tot.withinss / res$totss
  k
}
y

plot(c(2:5), y, type = "l")
