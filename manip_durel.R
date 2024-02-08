# Charger les données
dataset <- read.csv("wdbc.csv")
dataset

#Summary
summary(dataset)

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
