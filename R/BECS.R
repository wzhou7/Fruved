Score_BECS <- function(data){
  subdata <- data[,grep("BECS",names(data),value=TRUE)]
  vars0 <- names(subdata)
  for(j in 1:ncol(subdata)){
    subdata[,j][subdata[,j]==""] <- NA
    subdata[,j][subdata[,j]=="DID_NOT_ASK"] <- NA
  }
  
  for(j in 1:40){
    var <- paste0("Item",j)
    subdata[,var] <- rep(NA,nrow(subdata))
  }
  for(j in 1:14){
    var1 <- paste0("Item",j)
    var2 <- paste0("BECS2_",j)
    subdata[,var1][subdata[,var2]==1] <- 1
    subdata[,var1][subdata[,var2]==2] <- 2
    subdata[,var1][subdata[,var2]==3] <- 3
    subdata[,var1][subdata[,var2]==4] <- 4
    subdata[,var1][subdata[,var2]==5] <- 5
  }
  for(j in 15:16){
    k <- j - 14
    var1 <- paste0("Item",j)
    var2 <- paste0("BECS3_",k)
    subdata[,var1][subdata[,var2]==1] <- 1
    subdata[,var1][subdata[,var2]==2] <- 2
    subdata[,var1][subdata[,var2]==3] <- 3
    subdata[,var1][subdata[,var2]==4] <- 4
    subdata[,var1][subdata[,var2]==5] <- 5
  }
  subdata$Item17[subdata$BECS5==1] <- 1
  subdata$Item17[subdata$BECS5==2] <- 2
  subdata$Item17[subdata$BECS5==3] <- 3
  subdata$Item17[subdata$BECS5==4] <- 4
  subdata$Item17[subdata$BECS5==5] <- 5
  for(j in 18:26){
    k <- j - 17
    var1 <- paste0("Item",j)
    var2 <- paste0("BECS4_",k)
    subdata[,var1][subdata[,var2]==1] <- 1
    subdata[,var1][subdata[,var2]==2] <- 2
    subdata[,var1][subdata[,var2]==3] <- 3
    subdata[,var1][subdata[,var2]==4] <- 4
    subdata[,var1][subdata[,var2]==5] <- 5
  }
  for(j in 27:34){
    k <- j - 26
    var1 <- paste0("Item",j)
    var2 <- paste0("BECS6_",k)
    subdata[,var1][subdata[,var2]==1] <- 1
    subdata[,var1][subdata[,var2]==2] <- 2
    subdata[,var1][subdata[,var2]==3] <- 3
    subdata[,var1][subdata[,var2]==4] <- 4
    subdata[,var1][subdata[,var2]==5] <- 5
  }
  for(j in 35:37){
    k <- j - 34
    var1 <- paste0("Item",j)
    var2 <- paste0("BECS8_",k)
    subdata[,var1][subdata[,var2]==1] <- 1
    subdata[,var1][subdata[,var2]==2] <- 2
    subdata[,var1][subdata[,var2]==3] <- 3
    subdata[,var1][subdata[,var2]==4] <- 4
    subdata[,var1][subdata[,var2]==5] <- 5
  }
  for(j in 38:40){
    k <- j - 37
    var1 <- paste0("Item",j)
    var2 <- paste0("BECS10_",k)
    subdata[,var1][subdata[,var2]==1] <- 1
    subdata[,var1][subdata[,var2]==2] <- 2
    subdata[,var1][subdata[,var2]==3] <- 3
    subdata[,var1][subdata[,var2]==4] <- 4
    subdata[,var1][subdata[,var2]==5] <- 5
  }
  
  data$Nutrition_Changeability <-  (subdata$Item27 + subdata$Item28 + subdata$Item29 + subdata$Item30 + 
                                      subdata$Item31 + subdata$Item32 + subdata$Item33 + subdata$Item34)/8
  data$Nutrition_Behavior <-  (subdata$Item1 + subdata$Item2 + subdata$Item3 + subdata$Item4 + subdata$Item8
                               + subdata$Item9 + subdata$Item10 + subdata$Item11 + subdata$Item12 + subdata$Item13)/10
  data$Environmental_Changeability <-  (subdata$Item18 + subdata$Item19 + subdata$Item20 + subdata$Item21
                                        + subdata$Item22 + subdata$Item24 + subdata$Item25 + subdata$Item26)/8
  data$Program_Importance_and_Change_ability <-  (subdata$Item15 + subdata$Item16 + subdata$Item23)/3
  data$Exercise_Behavior <-  (subdata$Item5 + subdata$Item6 + subdata$Item7)/3
  data$Sleep_Behavior_and_Importance <-  (subdata$Item14 + subdata$Item17)/2
  data$Weight_Loss <-  (subdata$Item35 + subdata$Item36 + subdata$Item37)/3
  data$Alcohol_Intake <-  (subdata$Item38 + subdata$Item39 + subdata$Item40)/3
  
  return(data[,c("Nutrition_Changeability","Nutrition_Behavior","Environmental_Changeability",
                 "Program_Importance_and_Change_ability","Exercise_Behavior",
                 "Sleep_Behavior_and_Importance","Weight_Loss","Alcohol_Intake")])
}