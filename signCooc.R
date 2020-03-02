signCooc<-function(coocTerm, binDTM){
  
  # Matrix multiplication for cooccurrence counts
  coocCounts <- t(binDTM) %*% binDTM
  k <- nrow(binDTM)
  ki <- sum(binDTM[,coocTerm])
  kj <- colSums(binDTM)
  names(kj) <- colnames(binDTM)
  kij <- coocCounts[coocTerm, ]
  
  
  ########## MI: log(k*kij / (ki * kj) ########
  mutualInformationSig <- log(k * kij / (ki * kj))
  mutualInformationSig <- mutualInformationSig[order(mutualInformationSig, decreasing = TRUE)]
  
  ########## DICE: 2 X&Y / X + Y ##############
  dicesig <- 2 * kij / (ki + kj)
  dicesig <- dicesig[order(dicesig, decreasing=TRUE)]
  
  ########## Log Likelihood ###################
  logsig <- 2 * ((k * log(k)) - (ki * log(ki)) - (kj * log(kj)) + (kij * log(kij)) 
                 + (k - ki - kj + kij) * log(k - ki - kj + kij) 
                 + (ki - kij) * log(ki - kij) + (kj - kij) * log(kj - kij) 
                 - (k - ki) * log(k - ki) - (k - kj) * log(k - kj))
  logsig <- logsig[order(logsig, decreasing=T)]
  
  # Put all significance statistics in one Data-Frame
  resultOverView <- data.frame(
    names(sort(kij, decreasing=T)[1:10]), sort(kij, decreasing=T)[1:10],
    names(mutualInformationSig[1:10]), mutualInformationSig[1:10], 
    names(dicesig[1:10]), dicesig[1:10], 
    names(logsig[1:10]), logsig[1:10],
    row.names = NULL)
  colnames(resultOverView) <- c("Freq-terms", "Freq", "MI-terms", "MI", "Dice-Terms", "Dice", "LL-Terms", "LL")
  print(resultOverView)
}