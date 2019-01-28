# MET values and Formula for computation of Met-minutes
# Walking MET-minutes/week = 3.3 * walking minutes * walking days
# Moderate MET-minutes/week = 4.0 * moderate-intensity activity minutes * moderate days
# Vigorous MET-minutes/week = 8.0 * vigorous-intensity activity minutes * vigorous-intensity days
Score_IPAQ <- function(df){
    x1 <- x3 <- x5 <- rep(NA,nrow(df))  
  
    x1[df$IPAQ1==9] <- NA
    x1 <- df$IPAQ1 - 1
    
    x3[df$IPAQ3==9] <- NA
    x3 <- df$IPAQ3 - 1
    
    x5[df$IPAQ5==9] <- NA
    x5 <- df$IPAQ5 - 1
    
    x2 <- 10 * (df$IPAQ2 - 1)
    x2[df$IPAQ2==20] <- NA
    x2[df$IPAQ2==21] <- NA
    
    x4 <- 10 * (df$IPAQ4 - 1)
    x4[df$IPAQ4==20] <- NA
    x4[df$IPAQ4==21] <- NA
    
    x6 <- 10 * (df$IPAQ6 - 1)
    x6[df$IPAQ6==20] <- NA
    x6[df$IPAQ6==21] <- NA
    
    Walking <- Moderate <- Vigorous <- rep(NA,nrow(df))
    for(i in 1:nrow(df)){
        if(!is.na(x5[i]) & !is.na(x6[i])){Walking[i] <- 3.3*x5[i]*x6[i]}
        if(!is.na(x3[i]) & !is.na(x4[i])){Moderate[i] <- 4*x3[i]*x4[i]}
        if(!is.na(x1[i]) & !is.na(x2[i])){Vigorous[i] <- 8*x1[i]*x2[i]}
        if(!is.na(x5[i]) & x5[i]==0){Walking[i] <- 0}
        if(!is.na(x3[i]) & x3[i]==0){Moderate[i] <- 0}
        if(!is.na(x1[i]) & x1[i]==0){Vigorous[i] <- 0}
    }
    Total <- Walking + Moderate + Vigorous
    return(Total)
}
