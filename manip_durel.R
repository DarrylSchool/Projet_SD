# Charger les données
dataset <- read.csv("wdbc.csv")
dataset


# Afficher les colonnes du dataset
col_names <- colnames(dataset)
print(col_names)

#Summary
summary(dataset)

# Distribution des données

library(ggplot2) 

ax <- ggplot(dataset, aes(Diagnosis)) + geom_bar()
counts <- table(dataset$Diagnosis)
ax

B <- counts["B"]
M <- counts["M"]

print(paste("Number of Benign: ", B))
print(paste("Number of Malignant: ", M))

# Retirer l'ID et le diagnostic
dataset1 <- dataset[, -which(colnames(dataset) == "Diagnosis")]
dataset1 <- dataset1[, -which(colnames(dataset1) == "ID")]

# Matrice de corrélation
install.packages("corrplot")
library(corrplot)
library(RColorBrewer)

correlation_matrix <- cor(as.matrix(dataset1))

# Visualize as a heat map with correlation coefficients
corrplot(correlation_matrix, method='color', type='upper', col=brewer.pal(n = 8, name = 'RdBu'),
         tl.col='black', tl.srt=45, addCoef.col = 'black', number.cex = 0.7)



# Drop data

drop_list1 <- c('perimeter1', 'radius1', 'compactness1', 'concave_points1', 'radius2', 'perimeter2', 'radius3', 'perimeter3', 'concavity3' ,'compactness3', 'compactness2', 'concave_points2', 'texture3')
data_drop <- dataset1[, !colnames(dataset1) %in% drop_list1] 
data_drop
data_drop_scale <- scale(data_drop)

# Bodatasetplot 

bodatasetplot(dataset1)

plot(dataset1[,1:10])


# Bodatasetplot 

dataset1 <- dataset[, -which(colnames(dataset) == "ID")]
bodatasetplot(split(dataset1$radius1,dataset1$Diagnosis), main="raduis1")


# Suppression de plusieurs colonnes
columns_to_remove <- c("Diagnosis", "Column2", "Column3")
dataset1 <- dataset1[, -which(colnames(dataset1) %in% columns_to_remove)]


# Histogramme
install.packages("ggplot2")
library(ggplot2)

zone <- ggplot(dataset1, aes(dataset = radius))
zone + geom_histogram(aes( fill = Diagnosis),alpha = 0.7) + 
  datasetlim(0, 100) + geom_density() + 
  ggtitle("Densité empirique de raduis1") + 
  datasetlab("radius1") + 
  ylab("densité de Kernel") + 
  guides(fill=guide_legend(
    title.position="left",label.position="right",label.hjust=0.5,direction="horizontal")
  ) + 
  theme(legend.position="bottom")  



## Kmeans
wss_values <- numeric(length = 9)  # To store the within-cluster sum of squares values

for (k in 2:10) {
  kmeans_result <- kmeans(dataset1_scale, centers = k, nstart = 10)
  wss_values[k - 1] <- kmeans_result$tot.withinss
}

#Plot the elbow graph to determine the optimal number of clusters
fviz_nbclust(dataset1, kmeans, method = "wss") +
  geom_vline(xintercept = which.min(wss_values), linetype = 2) +
  labs(title = "Elbow Method to Determine Optimal k (Number of Clusters)",
       x = "Number of Clusters (k)",
       y = "Total Within-Cluster Sum of Squares (WSS)")



# Kmeans drop 
library(factoextra)
library(ggplot2)
wss_values <- numeric(length = 9) 
for (k in 2:15) {
      kmeans_result <- kmeans(data_drop_scale, centers = k, nstart = 15)
     wss_values[k - 1] <- kmeans_result$tot.withinss
}

fviz_nbclust(data_drop, kmeans, method = "wss") + 
        geom_vline(xintercept = which.min(wss_values), linetype = 2) + 
        labs(title = "Elbow Method to Determine Optimal k (Number of Clusters)",
                      x = "Number of Clusters (k)",
                      y = "Total Within-Cluster Sum of Squares (WSS)")


# CAH 

kmean.class <- kmeans(data_drop_scale,4)
data.hclust<-hclust(dist(data_drop_scale),method="ward.D2")
plot(data.hclust, labels = FALSE)

# CAH des centres des cluster
data.hclust1<-hclust(dist(kmean.class$centers),method="ward.D2")
plot(data.hclust1)


## Kmeans avec k=2 
kmeanRes2.class <- kmeans(data_drop_scale,2)


#PAM

library(cluster)
DistIns<-daisy(data_drop)

## Calculate silhouette widths for different numbers of clusters (k)
si <- numeric(19)
for (nbc in 2:20) {
  resupam <- pam(DistIns, k = nbc)
  si[nbc - 1] <- resupam$silinfo$avg.width
}
barplot(si, names.arg = 2:20, col = "black", xlab = "Number of clusters", 
                ylab = "Silhouette score", main = "Silhouette score vs no.of clusters")




## Visualistion des clusters



## Apprentissage supervisé
#Construction de l'échantillon

indices_M <- which(S$Diagnosis == "M")
indices_B <- which(S$Diagnosis == "B")

# Calculer 80% du nombre total d'indices de la classe
sample_size_M <- round(0.8 * length(indices_M))
sample_size_B <- round(0.8 * length(indices_B))

# Tirer un échantillon aléatoire de 80% des indices de la classe "M"
sample_indices_M <- sample(indices_M, sample_size_M, replace = FALSE)
sample_indices_B <- sample(indices_B, sample_size_B, replace = FALSE)

sub <- c(sample_indices_M, sample_indices_B)


######Arbre de décision 
library(rpart)
fit <- rpart(S$Diagnosis~ ., data=S, subset=sub)
fit
plot(fit)
text(fit)

# Utiliser le modèle pour prédire les classes sur l'ensemble des données moins l'échantillon d'entraînement
predictions <- predict(fit, newdata = S[-sub, ], type = "class")

# Comparer les prédictions du modèle avec les véritables valeurs de classe et afficher la matrice de confusion
confusion_matrix <- table(predictions, S[-sub, "Diagnosis"])
print(confusion_matrix)

# Calculer le score du modèle (par exemple, l'exactitude)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))