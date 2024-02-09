# Construction de l'échantillon

indices_M <- which(S$Diagnosis == "M")
indices_B <- which(S$Diagnosis == "B")

# Calculer 80% du nombre total d'indices de la classe
sample_size_M <- round(0.8 * length(indices_M))
sample_size_B <- round(0.8 * length(indices_B))

# Tirer un échantillon aléatoire de 80% des indices de la classe "M"
sample_indices_M <- sample(indices_M, sample_size_M, replace = FALSE)
sample_indices_B <- sample(indices_B, sample_size_B, replace = FALSE)

sub <- c(sample_indices_M, sample_indices_B)

train_data <- S[sub, ]
test_data <- S[-sub, ]

create_tree <- function(data, indices) {
  # Sous-échantillon bootstrap
  boot_data <- data[indices, ]
  
  # Créer l'arbre de décision avec rpart
  tree <- rpart(Diagnosis ~ ., data = boot_data, method = "class")
  
  return(tree)
}

num_iterations <- 100 # Nombre d'itérations Bootstrap
trees <- list() # Pour stocker les arbres de décision générés

for (i in 1:num_iterations) {
  # Générer un échantillon bootstrap
  boot_indices <- sample(sub, length(sub), replace = TRUE)
  
  # Créer un arbre de décision sur l'échantillon bootstrap actuel
  tree <- create_tree(train_data, boot_indices)
  
  # Ajouter l'arbre à la liste
  trees[[i]] <- tree
}

# Nouvelles données
new_data <- test_data

# Fonction pour prédire avec un arbre
predict_tree <- function(tree, newdata) {
  # Utiliser la prédiction de l'arbre sur les nouvelles données
  predictions <- predict(tree, newdata = newdata, type = "class")
  return(predictions)
}

# Prédire avec chaque arbre dans la liste "trees"
predictions <- sapply(trees, function(tree) predict_tree(tree, new_data))

# Calculer les exactitudes pour chaque arbre
accuracies <- sapply(trees, function(tree) {
  pred <- predict_tree(tree, new_data)
  accuracy <- mean(pred == new_data$Diagnosis)
  return(accuracy)
})

# Trouver l'index de l'arbre avec la meilleure exactitude
best_tree_index <- which.max(accuracies)

# Sélectionner le meilleur arbre
best_tree <- trees[[best_tree_index]]

accuracies[[best_tree_index]]
# Visualiser le meilleur arbre
plot(best_tree)
text(best_tree)


# Calculer la moyenne des exactitudes
mean_accuracy <- mean(accuracies)
cat("Moyenne des exactitudes:", mean_accuracy, "\n")

# Calculer la variance des exactitudes
var_accuracy <- var(accuracies)
cat("Variance des exactitudes:", var_accuracy, "\n")
