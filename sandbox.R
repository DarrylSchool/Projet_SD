sub <- c(sample(1:length(which(S$Diagnosis=="B")), round(0.8*length(which(S$Diagnosis=="B")),digits=0)),
         sample(1:(length(which(S$Diagnosis=="B"))+1):569,round(0.8*length(which(S$Diagnosis=="M")),digits=0)))


classes <- c('M', 'B')
for (classe in classes) {
  subset <- S[S$Diagnosis == classe, ]
  n <- round(nrow(subset) * 0.8)
  indices <- sample(1:nrow(subset), n, replace = FALSE)
  echantillon <- rbind(echantillon, subset[indices, ])
}

