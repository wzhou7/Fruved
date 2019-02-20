Score_CEPS_College <- function(dta){
  vars <- paste0("CEPS",1:15)
  data <- dta[,vars]
  df <- as.data.frame(matrix(NA,nrow=nrow(dta),ncol=15))
  
  #Q1
  df[,1][data[,1]==1] <- 6
  df[,1][data[,1]==2] <- 4.5
  df[,1][data[,1]==3] <- 3
  df[,1][data[,1]==4] <- 1.5
  df[,1][data[,1]==5] <- 0
  
  #Q2
  df[,2][data[,2]==1] <- 10
  df[,2][data[,2]==2] <- 7.5
  df[,2][data[,2]==3] <- 5
  df[,2][data[,2]==4] <- 2.5
  df[,2][data[,2]==5] <- 0
  
  #Q3
  df[,3][data[,3]==1] <- 10
  df[,3][data[,3]==2] <- 7.5
  df[,3][data[,3]==3] <- 5
  df[,3][data[,3]==4] <- 2.5
  df[,3][data[,3]==5] <- 0
  
  #Q4
  df[,4][data[,4]==1] <- 6
  df[,4][data[,4]==2] <- 4.5
  df[,4][data[,4]==3] <- 3
  df[,4][data[,4]==4] <- 1.5
  df[,4][data[,4]==5] <- 0
  
  #Q5
  df[,5][data[,5]==1] <- 4
  df[,5][data[,5]==2] <- 3
  df[,5][data[,5]==3] <- 2
  df[,5][data[,5]==4] <- 1
  df[,5][data[,5]==5] <- 0
  
  #Q6
  df[,6][data[,6]==1] <- 6
  df[,6][data[,6]==2] <- 4.5
  df[,6][data[,6]==3] <- 3
  df[,6][data[,6]==4] <- 1.5
  df[,6][data[,6]==5] <- 0
  
  #Q7
  df[,7][data[,7]==1] <- 6
  df[,7][data[,7]==2] <- 4.5
  df[,7][data[,7]==3] <- 3
  df[,7][data[,7]==4] <- 1.5
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
  df[,11][data[,11]==1] <- 10
  df[,11][data[,11]==2] <- 7.5
  df[,11][data[,11]==3] <- 5
  df[,11][data[,11]==4] <- 2.5
  df[,11][data[,11]==5] <- 0
  
  #Q12
  df[,12][data[,12]==1] <- 6
  df[,12][data[,12]==2] <- 4.5
  df[,12][data[,12]==3] <- 3
  df[,12][data[,12]==4] <- 1.5
  df[,12][data[,12]==5] <- 0
  
  #Q13
  df[,13][data[,13]==1] <- 4
  df[,13][data[,13]==2] <- 3
  df[,13][data[,13]==3] <- 2
  df[,13][data[,13]==4] <- 1
  df[,13][data[,13]==5] <- 0
  
  #Q14
  df[,14][data[,14]==1] <- 4
  df[,14][data[,14]==2] <- 3
  df[,14][data[,14]==3] <- 2
  df[,14][data[,14]==4] <- 1
  df[,14][data[,14]==5] <- 0
  
  #Q15
  df[,15][data[,15]==1] <- 6
  df[,15][data[,15]==2] <- 4.5
  df[,15][data[,15]==3] <- 3
  df[,15][data[,15]==4] <- 1.5
  df[,15][data[,15]==5] <- 0
  
  out <- as.data.frame(matrix(NA,nrow=nrow(dta),ncol=8))
  names(out) <- c("Policy","Food","Water","Vending","PA","Stress","Sleep","Total")
  out$Policy <- df[,2] + df[,3]
  out$Food <- df[,8] + df[,11]
  out$Water <- df[,5] + df[,13] + df[,14]
  out$Vending <- df[,9] + df[,12]
  out$PA <- df[,1] + df[,4] + df[,10] + df[,15]
  out$Stress <- df[,7]
  out$Sleep <- df[,6]
  out$Total <- df[,1] + df[,2] + df[,3] + df[,4] + df[,5] + df[,6] + df[,7]+ 
    df[,8] + df[,9] + df[,10] + df[,11] + df[,12] + df[,13] + df[,14] + df[,15]
  
  names(out) <- paste0("CEPS_SCORE_",names(out))
  return(out)
}