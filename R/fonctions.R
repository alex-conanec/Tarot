presence <- function(scores){
  
  presence <- as.data.frame(matrix(FALSE, ncol=ncol(scores), nrow=nrow(scores)))
  colnames(presence) <- colnames(scores)
  presence[1,which(scores[1,] != 0)] <- TRUE
  
  for (i in 2:nrow(scores)){
    presence[i,which(scores[i,]!=scores[i-1,])] <- TRUE
  }
  presence
}


