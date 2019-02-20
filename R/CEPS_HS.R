Score_CEPS_HS <- function(dta){
  vars <- paste0("CEPS",1:13)
  data <- dta[,vars]
  df <- as.data.frame(matrix(NA,nrow=nrow(dta),ncol=13))
  
  #Q1
  df[,1][data[,1]==1] <- 10
  df[,1][data[,1]==2] <- 7.5
  df[,1][data[,1]==3] <- 5
  df[,1][data[,1]==4] <- 2.5
  df[,1][data[,1]==5] <- 0
  
  #Q2
  df[,2][data[,2]==1] <- 6
  df[,2][data[,2]==2] <- 4.5
  df[,2][data[,2]==3] <- 3
  df[,2][data[,2]==4] <- 1.5
  df[,2][data[,2]==5] <- 0
  
  #Q3
  df[,3][data[,3]==1] <- 4
  df[,3][data[,3]==2] <- 3
  df[,3][data[,3]==3] <- 2
  df[,3][data[,3]==4] <- 1
  df[,3][data[,3]==5] <- 0
  
  #Q4
  df[,4][data[,4]==1] <- 6
  df[,4][data[,4]==2] <- 4.5
  df[,4][data[,4]==3] <- 3
  df[,4][data[,4]==4] <- 1.5
  df[,4][data[,4]==5] <- 0
  
  #Q5
  df[,5][data[,5]==1] <- 10
  df[,5][data[,5]==2] <- 7.5
  df[,5][data[,5]==3] <- 5
  df[,5][data[,5]==4] <- 2.5
  df[,5][data[,5]==5] <- 0
  
  #Q6
  df[,6][data[,6]==1] <- 10
  df[,6][data[,6]==2] <- 7.5
  df[,6][data[,6]==3] <- 5
  df[,6][data[,6]==4] <- 2.5
  df[,6][data[,6]==5] <- 0
  
  #Q7
  df[,7][data[,7]==1] <- 10
  df[,7][data[,7]==2] <- 7.5
  df[,7][data[,7]==3] <- 5
  df[,7][data[,7]==4] <- 2.5
  df[,7][data[,7]==5] <- 0
  
  #Q8
  df[,8][data[,8]==1] <- 10
  df[,8][data[,8]==2] <- 7.5
  df[,8][data[,8]==3] <- 5
  df[,8][data[,8]==4] <- 2.5
  df[,8][data[,8]==5] <- 0
  
  #Q9
  df[,9][data[,9]==1] <- 6
  df[,9][data[,9]==2] <- 4.5
  df[,9][data[,9]==3] <- 3
  df[,9][data[,9]==4] <- 1.5
  df[,9][data[,9]==5] <- 0
  
  #Q10
  df[,10][data[,10]==1] <- 6
  df[,10][data[,10]==2] <- 4.5
  df[,10][data[,10]==3] <- 3
  df[,10][data[,10]==4] <- 1.5
  df[,10][data[,10]==5] <- 0
  
  #Q11
  df[,11][data[,11]==1] <- 4
  df[,11][data[,11]==2] <- 3
  df[,11][data[,11]==3] <- 2
  df[,11][data[,11]==4] <- 1
  df[,11][data[,11]==5] <- 0
  
  #Q12
  df[,12][data[,12]==1] <- 4
  df[,12][data[,12]==2] <- 3
  df[,12][data[,12]==3] <- 2
  df[,12][data[,12]==4] <- 1
  df[,12][data[,12]==5] <- 0
  
  #Q13
  df[,13][data[,13]==1] <- 6
  df[,13][data[,13]==2] <- 4.5
  df[,13][data[,13]==3] <- 3
  df[,13][data[,13]==4] <- 1.5
  df[,13][data[,13]==5] <- 0
  
  out <- as.data.frame(matrix(NA,nrow=nrow(dta),ncol=7))
  names(out) <- c("Policy","Food","Water","Vending","PA","Stress","Total")
  out$Policy <- df[,1]
  out$Food <- df[,5] + df[,6] + df[,7] + df[,8]
  out$Water <- df[,3] + df[,11] + df[,12]
  out$Vending <- df[,9]
  out$PA <- df[,2] + df[,10] + df[,13]
  out$Stress <- df[,4]
  out$Total <- df[,1] + df[,2] + df[,3] + df[,4] + df[,5] + df[,6] + df[,7]+ 
    df[,8] + df[,9] + df[,10] + df[,11] + df[,12] + df[,13]
  
  names(out) <- paste0("CEPS_",names(out))
  return(out)
}