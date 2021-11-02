Score_sPSQI <- function(data){
    data$Comp1 <- rep(NA,nrow(data))
    data$Comp2 <- rep(NA,nrow(data))
    data$Comp3 <- rep(NA,nrow(data))
    data$Comp4 <- rep(NA,nrow(data))
    data$Comp5 <- rep(NA,nrow(data))
    data$Comp6 <- rep(NA,nrow(data))
    data$Comp7 <- rep(NA,nrow(data))
    
    ###Component 1###
    data$Comp1 <- data$Sleep9 - 1
    
    ###Component 2###
    Score2 <- rep(NA,nrow(data))
    Score2[data$Sleep2<=15] <- 0
    Score2[data$Sleep2>15 & data$Sleep2<=30] <- 1
    Score2[data$Sleep2>30 & data$Sleep2<=60] <- 2
    Score2[data$Sleep2>60] <- 3
    
    Score_Comp2 <- Score2 + data$Sleep5a - 1
    
    data$Comp2[Score_Comp2==0] <- 0
    data$Comp2[Score_Comp2==1 | Score_Comp2==2] <- 1
    data$Comp2[Score_Comp2==3 | Score_Comp2==4] <- 2
    data$Comp2[Score_Comp2==5 | Score_Comp2==6] <- 3
    
    ###Component 3###
    data$Comp3[data$Sleep4>=7] <- 0
    data$Comp3[data$Sleep4>=6 & data$Sleep4<7] <- 1
    data$Comp3[data$Sleep4>=5 & data$Sleep4<6] <- 2
    data$Comp3[data$Sleep4<5] <- 3
    
    ###Component 4###
    Hours_in_Bed<- as.numeric(difftime(strptime(data$Sleep3, "%I:%M %p" ),strptime(data$Sleep1, "%I:%M %p" ),units='hours'))
    Hours_in_Bed[!is.na(Hours_in_Bed) & Hours_in_Bed<0] <- 24 + Hours_in_Bed[!is.na(Hours_in_Bed) & Hours_in_Bed<0] 
    
    Score_Comp4 <- rep(NA,nrow(data))
    Score_Comp4 <- data$Sleep4/Hours_in_Bed*100
    data$Comp4[Score_Comp4 >= 85] <- 0
    data$Comp4[Score_Comp4 >= 75 & Score_Comp4 < 85] <- 1
    data$Comp4[Score_Comp4 >= 65 & Score_Comp4 < 75] <- 2
    data$Comp4[Score_Comp4 < 65] <- 3
    
    ###Component 5###
    data$Sleep5j[is.na(data$Sleep5j) | data$Sleep5j==""] <- 1
    Score_Comp5 <- data$Sleep5b + data$Sleep5c + data$Sleep5d + data$Sleep5e + data$Sleep5f + data$Sleep5g + data$Sleep5h + data$Sleep5i + data$Sleep5j- 9
    data$Comp5[Score_Comp5 == 0] <- 0
    data$Comp5[Score_Comp5 >= 1 & Score_Comp5 <= 9] <- 1
    data$Comp5[Score_Comp5 >= 10 & Score_Comp5 <= 18] <- 2
    data$Comp5[Score_Comp5 >= 19 & Score_Comp5 <= 27] <- 3
    
    ###Component 6###
    data$Comp6 <- data$Sleep6 - 1
    
    ###Component 7###
    Score_Comp7 <- data$Sleep7 - 1 + data$Sleep8 - 1
    data$Comp7[Score_Comp7==0] <- 0
    data$Comp7[Score_Comp7==1 | Score_Comp7==2] <- 1
    data$Comp7[Score_Comp7==3 | Score_Comp7==4] <- 2
    data$Comp7[Score_Comp7==5 | Score_Comp7==6] <- 3
    
    data$PSQI <- data$Comp1 + data$Comp2 + data$Comp3 + data$Comp4 + data$Comp5 + data$Comp6 + data$Comp7
    return(data$PSQI)
}
