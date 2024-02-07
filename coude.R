y <- c(0,0,0,0)
for(k in 2:75){
  res <- kmeans(X1_scale, k)
  y[k-2] <- res$tot.withinss / res$totss
  k
}
y

plot(c(2:74), y, type = "l")
