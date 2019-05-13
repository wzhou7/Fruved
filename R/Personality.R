Score_Personality <- function(data){
  df <- data[,paste0("P_",1:10)]
  for(j in 1:ncol(df)){
    df[,j][df[,j]==8 | df[,j]==""] <- NA
    df[,j] <- as.numeric(as.character(df[,j]))
  }
  dg <- df
  for(j in c(2,4,6,8,10)){
    dg[,j] <- 8 - df[,j]
  }
  
  Extraversion <- (dg[,1] + dg[,6])/2
  Agreeableness <- (dg[,2] + dg[,7])/2
  Conscientiousness <- (dg[,3] + dg[,8])/2
  Emotional_Stability <- (dg[,4] + dg[,9])/2
  Openness_to_Experiences <- (dg[,5] + dg[,10])/2
  p.score <- cbind(Extraversion,Agreeableness,Conscientiousness,Emotional_Stability,Openness_to_Experiences)
  
  return(as.data.frame(p.score))
}