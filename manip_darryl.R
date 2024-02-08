
X1 <- X[, -which(colnames(X) == "Diagnosis")]
X1 <- X1[, -which(colnames(X1) == "ID")]
boxplot(X1)

plot(X1[,1:10])



X1 <- X[, -which(colnames(X) == "ID")]
boxplot(split(X1$radius1,X1$Diagnosis), main="raduis1")

install.packages("corrplot")
library(corrplot)
correlation_matrix <- cor(X1[,1:10])
corrplot(correlation_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)
diagnosis_counts <- table(donnees$Diagnosis)
pie(diagnosis_count, labels = paste(names(diagnosis_counts), "(", round(prop.table(diagnosis_counts) * 100, 1), "%)"), main = "Diagnosis Distribution")

# Suppression de plusieurs colonnes
data <- X1_scale[, -c(21:30)] #Supprime de la colonne 21 à la colonne 30

# Fonction Kmeans
kmeans_2<-kmeans(data, 2) #Kmeans avec 2 clusters

# Executer la fonction coude
source("/home/darryl/Documents/INFO5/SDD/Projet/Projet_SD/coude.R") 

# Generer le dendogram pour les centres du kmeans choisi
kmeans_3 <- kmeans(X1_scale, 3)
kmeans_3_centre_dist <- dist(kmeans_3$centers)
kmeans_3_dendo_centre_dist <- hclust(kmeans_3_centre_dist, method = "ward.D")

# interpretation des kmeans
X1 <- X[, -c(1:2)]
X1_scale <- scale (X1)
Dn <- dist(X1_scale)
l <- cmdscale(Dn, k = 2)
plot(l, type = "n")
text(l,X$Diagnosis, , col = kmeans_3$cluster)

# Cluster dendrogram pour tout le dataset
D <- dist(X1_scale)
resuhist <- hclust(D, method = "ward.D")
plot(resuhist)

# Fonction coude (directement dans la console)
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
