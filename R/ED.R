Score_ED <- function(data){
  seq <- paste("ED_",9:34,sep="")
  for(j in 1:25){
    comp <- paste("Comp",j,sep="")
    data[,comp] <- rep(0,nrow(data))
    data[,comp][data[,seq[j]]==1] <- 3
    data[,comp][data[,seq[j]]==2] <- 2
    data[,comp][data[,seq[j]]==3] <- 1
    data[,comp][data[,seq[j]]==7] <- NA
    data[,comp][is.na(data[,seq[j]])] <- NA
  }
  
  data$Comp26 <- rep(0,nrow(data))
  data$Comp26[data[,seq[26]]==6] <- 3
  data$Comp26[data[,seq[26]]==5] <- 2
  data$Comp26[data[,seq[26]]==4] <- 1
  data$Comp26[data[,seq[26]]==7] <- NA
  data$Comp26[is.na(data[,seq[26]])] <- NA
  
  ED_SCORE <- data$Comp1 + data$Comp2 + data$Comp3 + data$Comp4 + data$Comp5 + data$Comp6 +
    data$Comp7 + data$Comp8 + data$Comp9 + data$Comp10 + data$Comp11 + data$Comp12 + data$Comp13 +
    data$Comp14 + data$Comp15 + data$Comp16 + data$Comp17 + data$Comp18 + data$Comp19 + 
    data$Comp20 + data$Comp21 + data$Comp22 + data$Comp23 + data$Comp24 + data$Comp25 + data$Comp26
  
  return(ED_SCORE)
}