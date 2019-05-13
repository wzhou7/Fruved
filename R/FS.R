Score_FS <- function(data){
  seq <- c("FS1","FS2","FS3","FS16","FS4","FS17","FS5","FS6","FS7","FS8","FS9")
  df <- data[,seq]
  
  seq1 <- paste("Comp",1:10,sep="")
  df[,seq1] <- rep(NA,nrow(df))
  
  ###Comp1-HH2: FS2
  df$Comp1[df$FS2==1 | df$FS2==2] <- 1
  df$Comp1[df$FS2==3 | df$FS2==4] <- 0
  
  ###Comp2-HH3: FS3
  df$Comp2[df$FS3==1 | df$FS3==2] <- 1
  df$Comp2[df$FS3==3 | df$FS3==4] <- 0
  
  ###Comp3-HH4: FS16
  df$Comp3[df$FS16==1 | df$FS16==2] <- 1
  df$Comp3[df$FS16==3 | df$FS16==4] <- 0
  
  ###Comp4-AD1: FS4
  df$Comp4[df$FS4==1] <- 1
  df$Comp4[df$FS4==7 | df$FS4==3] <- 0
  
  ###Comp5-AD1.1a: FS17
  df$Comp5[df$FS17==1 | df$FS17==2] <- 1
  df$Comp5[df$FS17==3 | df$FS17==4] <- 0
  df$Comp5[df$Comp4==0] <- 0
  
  ###Comp6-AD2: FS5
  df$Comp6[df$FS5==2] <- 1
  df$Comp6[df$FS5==3 | df$FS5==4] <- 0
  
  ###Comp7-AD3: FS6
  df$Comp7[df$FS6==1] <- 1
  df$Comp7[df$FS6==2 | df$FS6==3] <- 0
  
  ###Comp8-AD4: FS7
  df$Comp8[df$FS7==1] <- 1
  df$Comp8[df$FS7==2 | df$FS7==3] <- 0
  
  ###Comp9-AD5: FS8
  df$Comp9[df$FS8==1] <- 1
  df$Comp9[df$FS8==2 | df$FS8==3] <- 0
  
  ###Comp10-AD5a: FS9
  df$Comp10[df$FS9==1 | df$FS9==2] <- 1
  df$Comp10[df$FS9==3 | df$FS9==4] <- 0
  df$Comp10[df$Comp9==0] <- 0
  
  df$FS_SCORE1 <- df$Comp1 + df$Comp2 + df$Comp3
  df$FS_SCORE2 <- df$Comp4 + df$Comp5 + df$Comp6 + df$Comp7 + df$Comp8
  df$FS_SCORE3 <- df$Comp9 + df$Comp10
  df$FS_SCORE <- rep(NA,nrow(df))
  
  for(i in 1:nrow(df)){
    if(!is.na(df$FS_SCORE1[i])){
      if(df$FS_SCORE1[i]==0){df$FS_SCORE[i] <- 0}
      if(df$FS_SCORE1[i]>0 | (!is.na(df$FS1[i]) & (df$FS1[i]==3 | df$FS1[i]==4))){
        if(!is.na(df$FS_SCORE2[i])){
          if(df$FS_SCORE2[i]==0){df$FS_SCORE[i] <- df$FS_SCORE1[i]}
          if(df$FS_SCORE2[i]>0){
            if(!is.na(df$FS_SCORE3[i])){
              df$FS_SCORE[i] <- df$FS_SCORE1[i] + df$FS_SCORE2[i] + df$FS_SCORE3[i]
            }
          }
        }
      }
    }
  }
  
  return(df$FS_SCORE)
}