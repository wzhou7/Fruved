Score_IPAQ <- function(df){
    x1 <- x3 <- x5 <- rep(NA,nrow(df))  
    
    x1 <- df$IPAQ1 - 1
    x1[df$IPAQ1==9] <- NA
    
    x3 <- df$IPAQ3 - 1
    x3[df$IPAQ3==9] <- NA
    
    x5 <- df$IPAQ5 - 1
    x5[df$IPAQ5==9] <- NA
    
    x2 <- 10 * (df$IPAQ2 - 1)
    x2[df$IPAQ2==20] <- NA
    x2[df$IPAQ2==21] <- NA
    
    x4 <- 10 * (df$IPAQ4 - 1)
    x4[df$IPAQ4==20] <- NA
    x4[df$IPAQ4==21] <- NA
    
    x6 <- 10 * (df$IPAQ6 - 1)
    x6[df$IPAQ6==20] <- NA
    x6[df$IPAQ6==21] <- NA
    
    Walking <- Moderate <- Vigorous <- Category <- rep(NA,nrow(df))
    for(i in 1:nrow(df)){
      if(!is.na(x5[i]) & !is.na(x6[i])){Walking[i] <- 3.3*x5[i]*x6[i]}
      if(!is.na(x3[i]) & !is.na(x4[i])){Moderate[i] <- 4*x3[i]*x4[i]}
      if(!is.na(x1[i]) & !is.na(x2[i])){Vigorous[i] <- 8*x1[i]*x2[i]}
      if(!is.na(x5[i]) & x5[i]==0){Walking[i] <- 0}
      if(!is.na(x3[i]) & x3[i]==0){Moderate[i] <- 0}
      if(!is.na(x1[i]) & x1[i]==0){Vigorous[i] <- 0}
      
      cond1 <- !is.na(x1[i]) & x1[i]>=3 & Vigorous[i]>=1500
      cond2 <- sum(c(x1[i],x3[i],x5[i]),na.rm=TRUE)>=7 & 
        sum(c(Vigorous[i],Moderate[i],Walking[i]),na.rm=TRUE)>=3000
      cond3 <- !is.na(x1[i]) & x1[i]>=3 & !is.na(x2[i]) & x2[i]>=20
      cond4 <- sum(c(x3[i],x5[i]),na.rm=TRUE)>=5 & sum(c(x4[i],x6[i]),na.rm=TRUE)>=30
      cond5 <- sum(c(x1[i],x3[i],x5[i]),na.rm=TRUE)>=5 & 
        sum(c(Vigorous[i],Moderate[i],Walking[i]),na.rm=TRUE)>=600
      
      if(cond3 | cond4 | cond5){
        Category[i] <- "Moderate"
      }
      
      if(cond1 | cond2){
        Category[i] <- "High"
      }
      
      if(!is.na(Walking[i]) & !is.na(Moderate[i]) & !is.na(Vigorous[i]) & 
         !(cond1 | cond2 | cond3 | cond4 | cond5)){
        Category[i] <- "Low"
      }
        
    }
    
    Total <- Walking + Moderate + Vigorous
    
    out <- data.frame(WalkingDays = x5,
                      WalkingMins = x6,
                      WalkingMETS = Walking,
                      
                      ModerateDays = x3,
                      ModerateMins = x4,
                      ModerateMETS = Moderate,
                      
                      VigorousDays = x1,
                      VigorousMins = x2,
                      VigorousMETS = Vigorous,
                      
                      TotalMETS = Total,
                      Category = Category)
    
    
    return(out)
}
