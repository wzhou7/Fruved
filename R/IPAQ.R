# MET values and Formula for computation of Met-minutes
# Walking MET-minutes/week = 3.3 * walking minutes * walking days
# Moderate MET-minutes/week = 4.0 * moderate-intensity activity minutes * moderate days
# Vigorous MET-minutes/week = 8.0 * vigorous-intensity activity minutes * vigorous-intensity days
IPAQ_Continuous <- function(df){
    x1 <- x3 <- x5 <- rep(NA,nrow(df))  
  
    x1[df$IPAQ1==1] <- 0
    x1[df$IPAQ1==2] <- 1
    x1[df$IPAQ1==3] <- 2
    x1[df$IPAQ1==4] <- 3
    x1[df$IPAQ1==5] <- 4
    x1[df$IPAQ1==6] <- 5
    x1[df$IPAQ1==7] <- 6
    x1[df$IPAQ1==8] <- 7
    x1[df$IPAQ1==9] <- NA
    
    x3[df$IPAQ3==1] <- 0
    x3[df$IPAQ3==2] <- 1
    x3[df$IPAQ3==3] <- 2
    x3[df$IPAQ3==4] <- 3
    x3[df$IPAQ3==5] <- 4
    x3[df$IPAQ3==6] <- 5
    x3[df$IPAQ3==7] <- 6
    x3[df$IPAQ3==8] <- 7
    x3[df$IPAQ3==9] <- NA
    
    x5[df$IPAQ5==1] <- 0
    x5[df$IPAQ5==2] <- 1
    x5[df$IPAQ5==3] <- 2
    x5[df$IPAQ5==4] <- 3
    x5[df$IPAQ5==5] <- 4
    x5[df$IPAQ5==6] <- 5
    x5[df$IPAQ5==7] <- 6
    x5[df$IPAQ5==8] <- 7
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
    
    Walking <- Moderate <- Vigorous <- c(rep(99999999,nrow(df)))
    for(i in 1:nrow(df)){
        if(!is.na(x5[i]) & !is.na(x6[i])){Walking[i] <- 3.3*x5[i]*x6[i]}
        if(!is.na(x3[i]) & !is.na(x4[i])){Moderate[i] <- 4*x3[i]*x4[i]}
        if(!is.na(x1[i]) & !is.na(x2[i])){Vigorous[i] <- 8*x1[i]*x2[i]}
        if(!is.na(x5[i]) & x5[i]==0){Walking[i] <- 0}
        if(!is.na(x3[i]) & x3[i]==0){Moderate[i] <- 0}
        if(!is.na(x1[i]) & x1[i]==0){Vigorous[i] <- 0}
    }
    Total <- Walking + Moderate + Vigorous
    Total[Total>=99999999] <- NA
    return(Total)
}

# IPAQ_Discrete <- function(df){
#     m <- nrow(df)
#     n <- ncol(df)
#     x <- c(rep(1,m))
#     y <- c(rep(1,m))
#     z <- c()
#     Total <- IPAQ_Continuous(df)
#     for(i in 1:m){
#         if(df$IPAQ1[i]>=3 & Total[i]>=1500 & Total[i]<999999){x[i] <- 3}
#         if((df$IPAQ1[i] + df$IPAQ3[i] + df$IPAQ5[i])>=7 & Total[i]>=3000 & Total[i]<999999){x[i] <- 3}
#         if(Total[i]>999999){x[i] <- 9999}
#     }
#     
#     for(i in 1:m){
#         if(!is.na(df$IPAQ1[i]) & !is.na(df$IPAQ2[i])){
#             if(df$IPAQ1[i]>=3 & df$IPAQ2[i]>=20){y[i] <- 2}
#         }
#         if(!is.na(df$IPAQ3[i])){
#             if(df$IPAQ3[i]>=5){y[i] <- 2}
#         }
#         if(!is.na(df$IPAQ6[i])){
#             if(df$IPAQ6[i]>=30){y[i] <- 2}
#         }
#         if(!is.na(df$IPAQ1[i]) & !is.na(df$IPAQ3[i]) & !is.na(df$IPAQ5[i]) & Total[i]<999999){
#             if((df$IPAQ1[i] + df$IPAQ3[i] + df$IPAQ5[i])>=5 & Total[i]>=600){y[i] <- 2}
#         }
#         if(y[i]<2 & Total[i]>999999){y[i] <- 9999}
#     }
#     for(i in 1:m){
#         z[i] <- max(x[i],y[i])
#     }
#     return(z)
# }
# 
# ScoreIPAQ <- function(data){
#     IPAQquestions <- data[,c("IPAQ1","IPAQ2","IPAQ3","IPAQ4","IPAQ5","IPAQ6")]
#     
#     for(i in 1:nrow(IPAQquestions)){
#         if(!is.na(IPAQquestions$IPAQ1[i])){
#             if(IPAQquestions$IPAQ1[i]==9) {IPAQquestions$IPAQ1[i] <- NA}
#             else{IPAQquestions$IPAQ1[i] <- IPAQquestions$IPAQ1[i]-1}
#         }
#         
#         if(!is.na(IPAQquestions$IPAQ3[i])){
#             if(IPAQquestions$IPAQ3[i]==9) {IPAQquestions$IPAQ3[i] <- NA}
#             else{IPAQquestions$IPAQ3[i] <- IPAQquestions$IPAQ3[i]-1}
#         }
#         
#         if(!is.na(IPAQquestions$IPAQ5[i])){
#             if(IPAQquestions$IPAQ5[i]==9) {IPAQquestions$IPAQ5[i] <- NA}
#             else{IPAQquestions$IPAQ5[i] <- IPAQquestions$IPAQ5[i]-1}
#         }
#         
#         if(!is.na(IPAQquestions$IPAQ2[i])){
#             if(IPAQquestions$IPAQ2[i]==20 || IPAQquestions$IPAQ2[i]==21) {IPAQquestions$IPAQ2[i] <- NA}
#             else{IPAQquestions$IPAQ2[i] <- 10*(IPAQquestions$IPAQ2[i]-1)}
#         }
#         
#         if(!is.na(IPAQquestions$IPAQ4[i])){
#             if(IPAQquestions$IPAQ4[i]==20 || IPAQquestions$IPAQ4[i]==21) {IPAQquestions$IPAQ4[i] <- NA}
#             else{IPAQquestions$IPAQ4[i] <- 10*(IPAQquestions$IPAQ4[i]-1)}
#         }
#         
#         if(!is.na(IPAQquestions$IPAQ6[i])){
#             if(IPAQquestions$IPAQ6[i]==20 || IPAQquestions$IPAQ6[i]==21) {IPAQquestions$IPAQ6[i] <- NA}
#             else{IPAQquestions$IPAQ6[i] <- 10*(IPAQquestions$IPAQ6[i]-1)}
#         }
#     }
#     
#     IPAQ_Scores <- IPAQ_Continuous(IPAQquestions)
#     IPAQ_Categories <- IPAQ_Discrete(IPAQquestions)
#     
#     IPAQ_Scores[IPAQ_Scores==99999999] <- NA
#     IPAQ_Categories[IPAQ_Categories==9999] <- NA
#     
#     df <- data.frame(IPAQ_Scores=IPAQ_Scores, IPAQ_Categories=IPAQ_Categories)
#     return(df)
# }
