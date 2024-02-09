# Chargement de la bibliothèque nécessaire
library(cluster)

# Initialisation d'un vecteur pour stocker les scores
scores <- numeric()
range_values <- 2:5

# Boucle sur les valeurs de range_values
for (i in range_values) {
  # Appliquer l'algorithme K-means avec le nombre de clusters i
  kmeans <- kmeans(data_drop_scale, centers = i, nstart = 10, algorithm = "Lloyd")
  
  # Calcul du score de silhouette
  score <- silhouette(kmeans$cluster, dist(data_drop_scale))$avg.width
  print(paste("\nNumber of clusters:", i))
  print(paste("\nSilhouette score =", score))
  
  # Stockage du score
  scores <- c(scores, score)
}

# Tracé des scores
barplot(scores, names.arg = range_values, col = "black", xlab = "Number of clusters", 
        ylab = "Silhouette score", main = "Silhouette score vs no.of clusters")