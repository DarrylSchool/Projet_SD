
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