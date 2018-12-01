get_personality <- function(data){
  vars <- paste0("P",1:10)
  df <- data[,vars]
  for(j in 1:ncol(df)){
    df[,j][df[,j]==8] <- NA
  }
  dg <- df
  for(j in c(2,4,6,8,10)){
    dg[,j] <- 8 - df[,j]
  }
  data$Extraversion <- (dg[,1] + dg[,6])/2
  data$Agreeableness <- (dg[,2] + dg[,7])/2
  data$Conscientiousness <- (dg[,3] + dg[,8])/2
  data$Emotional_Stability <- (dg[,4] + dg[,9])/2
  data$Openness_to_Experiences <- (dg[,5] + dg[,10])/2
  
  out <- data[,c("Extraversion","Agreeableness","Conscientiousness","Emotional_Stability","Openness_to_Experiences")]
  return(out)
}