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

# Boxplots
par(mfrow=c(2,5))
boxplot(split(dataset$radius1,dataset$Diagnosis), main="raduis1")
boxplot(split(dataset$texture1,dataset$Diagnosis), main="texture1")
boxplot(split(dataset$perimeter1,dataset$Diagnosis), main="perimeter1")
boxplot(split(dataset$area1,dataset$Diagnosis), main="area1")
boxplot(split(dataset$smoothness1,dataset$Diagnosis), main="smoothness1")
boxplot(split(dataset$compactness1,dataset$Diagnosis), main="compactness1")
boxplot(split(dataset$concavity1,dataset$Diagnosis), main="concavity1")
boxplot(split(dataset$concave_points1,dataset$Diagnosis), main="concave_points1")
boxplot(split(dataset$symmetry1,dataset$Diagnosis), main="symmetry1")
boxplot(split(dataset$fractal_dimension1,dataset$Diagnosis), main="fractal_dimension1")

boxplot(split(dataset$radius2,dataset$Diagnosis), main="radius2")
boxplot(split(dataset$texture2,dataset$Diagnosis), main="texture2")
boxplot(split(dataset$perimeter2,dataset$Diagnosis), main="perimeter2")
boxplot(split(dataset$area2,dataset$Diagnosis), main="area2")
boxplot(split(dataset$smoothness2,dataset$Diagnosis), main="smoothness2")
boxplot(split(dataset$compactness2,dataset$Diagnosis), main="compactness2")
boxplot(split(dataset$concavity2,dataset$Diagnosis), main="concavity2")
boxplot(split(dataset$concave_points2,dataset$Diagnosis), main="concave_points2")
boxplot(split(dataset$symmetry2,dataset$Diagnosis), main="symmetry2")
boxplot(split(dataset$fractal_dimension2,dataset$Diagnosis), main="fractal_dimension2")

boxplot(split(dataset$radius3,dataset$Diagnosis), main="radius3")
boxplot(split(dataset$texture3,dataset$Diagnosis), main="texture3")
boxplot(split(dataset$perimeter3,dataset$Diagnosis), main="perimeter3")
boxplot(split(dataset$area3,dataset$Diagnosis), main="area3")
boxplot(split(dataset$smoothness3,dataset$Diagnosis), main="smoothness3")
boxplot(split(dataset$compactness3,dataset$Diagnosis), main="compactness3")
boxplot(split(dataset$concavity3,dataset$Diagnosis), main="concavity3")
boxplot(split(dataset$concave_points3,dataset$Diagnosis), main="concave_points3")
boxplot(split(dataset$symmetry3,dataset$Diagnosis), main="symmetry3")
boxplot(split(dataset$fractal_dimension3,dataset$Diagnosis), main="fractal_dimension3")

par(mfrow=c(1,1))

# Drop data
drop_list1 <- c('perimeter1', 'radius1', 'compactness1', 'concave_points1', 'radius2', 'perimeter2', 'radius3', 'perimeter3', 'concavity3' ,'compactness3', 'compactness2', 'concave_points2', 'texture3')
data_drop <- dataset1[, !colnames(dataset1) %in% drop_list1] 
data_drop
data_drop_scale <- scale(data_drop)

# Matrice de corrélation
install.packages("corrplot")
library(corrplot)
library(RColorBrewer)

correlation_matrix <- cor(as.matrix(dataset1))

correlation_matrix_reduc <- cor(as.matrix(data_drop))

# Visualize as a heat map with correlation coefficients
corrplot(correlation_matrix, method='color', type='upper', col=brewer.pal(n = 8, name = 'RdBu'),
         tl.col='black', tl.srt=45, addCoef.col = 'black', number.cex = 0.7)

corrplot(correlation_matrix_reduc, method='color', type='upper', col=brewer.pal(n = 8, name = 'RdBu'),
         tl.col='black', tl.srt=45, addCoef.col = 'black', number.cex = 0.7)

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
Dn <- dist(data_drop_scale)
l <- cmdscale(Dn, k = 2)
plot(l, type = "n")
text(l,dataset$Diagnosis, , col = kmeanRes2.class$cluster)

## Apprentissage supervisé
#Construction de l'échantillon

indices_M <- which(dataset$Diagnosis == "M")
indices_B <- which(dataset$Diagnosis == "B")

# Calculer 80% du nombre total d'indices de la classe
sample_size_M <- round(0.8 * length(indices_M))
sample_size_B <- round(0.8 * length(indices_B))

# Tirer un échantillon aléatoire de 80% des indices de la classe "M"
sample_indices_M <- sample(indices_M, sample_size_M, replace = FALSE)
sample_indices_B <- sample(indices_B, sample_size_B, replace = FALSE)

sub <- c(sample_indices_M, sample_indices_B)


######Arbre de décision 
library(rpart)
fit <- rpart(dataset$Diagnosis~ ., data=dataset, subset=sub)
fit
plot(fit)
text(fit)

# Utiliser le modèle pour prédire les classes sur l'ensemble des données moins l'échantillon d'entraînement
predictions <- predict(fit, newdata = dataset[-sub, ], type = "class")

# Comparer les prédictions du modèle avec les véritables valeurs de classe et afficher la matrice de confusion
confusion_matrix <- table(predictions, dataset[-sub, "Diagnosis"])
print(confusion_matrix)

# Calculer le score du modèle (par exemple, l'exactitude)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))