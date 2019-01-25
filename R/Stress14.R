StressItem <- function(x,rev){
  x[x==6] <- NA
  if(rev){
    x <- 5- x
  }
  else{
    x <- x - 1
  }
  return(x)
}

Score_Stress <- function(data){
  index <- paste0("STRESS_",c(5,3,2,1,6:15))
  df <- data[,index]
  Stress_Score <- StressItem(df[,1],FALSE) + StressItem(df[,2],FALSE) + 
    StressItem(df[,3],FALSE) + StressItem(df[,4],TRUE) + StressItem(df[,5],TRUE) + 
    StressItem(df[,6],TRUE) + StressItem(df[,7],TRUE) + StressItem(df[,8],FALSE) + 
    StressItem(df[,9],TRUE) + StressItem(df[,10],TRUE) + StressItem(df[,11],FALSE) +
    StressItem(df[,12],FALSE) + StressItem(df[,13],TRUE) + StressItem(df[,14],FALSE)
  return(Stress_Score)
}

