# Charger les données
X <- read.csv("wdbc.csv")
X

#Summary
summary(X)

# Retirer l'ID et le diagnostic
X1 <- X[, -which(colnames(X) == "Diagnosis")]
X1 <- X1[, -which(colnames(X1) == "ID")]

# Matrice de corrélation
install.packages("corrplot")
library(corrplot)
correlation_matrix <- cor(X1[,1:10])
corrplot(correlation_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)

# Boxplot 

boxplot(X1)

plot(X1[,1:10])


# Boxplot 

X1 <- X[, -which(colnames(X) == "ID")]
boxplot(split(X1$radius1,X1$Diagnosis), main="raduis1")


# Suppression de plusieurs colonnes
columns_to_remove <- c("Diagnosis", "Column2", "Column3")
X1 <- X1[, -which(colnames(X1) %in% columns_to_remove)]


# Histogramme
install.packages("ggplot2")
library(ggplot2)

zone <- ggplot(X1, aes(x = radius))
zone + geom_histogram(aes( fill = Diagnosis),alpha = 0.7) + 
  xlim(0, 100) + geom_density() + 
  ggtitle("Densité empirique de raduis1") + 
  xlab("radius1") + 
  ylab("densité de Kernel") + 
  guides(fill=guide_legend(
    title.position="left",label.position="right",label.hjust=0.5,direction="horizontal")
  ) + 
  theme(legend.position="bottom")  
