trim <- function (x) gsub("^\\s+|\\s+$", "", x)

Score_Race <- function(data){
  # HispanicLatino
  data$HispanicLatino[data$HispanicLatino==1] <- "Yes"
  data$HispanicLatino[data$HispanicLatino==2] <- "No"
  data$HispanicLatino[data$HispanicLatino==3] <- NA
  data$HispanicLatino[data$HispanicLatino==4] <- NA
  summary(as.factor(data$HispanicLatino))
  
  old2YN <- function(v){
    v <- as.character(v)
    cond_No <- is.na(v) | v==""
    cond_Yes <- (!is.na(v)) & (v!="")
    v[cond_No]  <- "No"
    v[cond_Yes] <- "Yes"
    return(v)
  }
  
  # Change text to Y/N or ""
  for(v in c("White","Black","Asian","Islander","Native","Other","HispanicLatino1")){
    data[,v] <- old2YN(data[,v])
  }
  
  data$RaceEth4 <- rep(NA,nrow(data))
  for(i in 1:nrow(data)){
    if(!is.na(data$HispanicLatino[i])){
      if(data$HispanicLatino[i]=="Yes"){
        data$RaceEth4[i] <- "Hispanic/Latino"
      }
      if(data$HispanicLatino[i]=="No"){
        cond1 <- (data$White[i]=="Yes" & data$Black[i]=="No" & data$HispanicLatino1[i]=="No" 
                  & data$Asian[i]=="No" & data$Islander[i]=="No" & data$Native[i]=="No" & data$Other[i]=="No")
        cond2 <- (data$White[i]=="No" & data$Black[i]=="Yes" & data$HispanicLatino1[i]=="No" 
                  & data$Asian[i]=="No" & data$Islander[i]=="No" & data$Native[i]=="No" & data$Other[i]=="No")
        cond3 <- (data$White[i]=="No" & data$Black[i]=="No" & data$HispanicLatino1[i]=="Yes" 
                  & data$Asian[i]=="No" & data$Islander[i]=="No" & data$Native[i]=="No" & data$Other[i]=="No")
        cond4 <- (data$White[i]=="No" & data$Black[i]=="No" & data$HispanicLatino1[i]=="No" 
                  & data$Asian[i]=="No" & data$Islander[i]=="No" & data$Native[i]=="No" & data$Other[i]=="No")
        if(cond1){
          data$RaceEth4[i] <- "Non-Hispanic White"
        }
        if(cond2){
          data$RaceEth4[i] <- "Non-Hispanic Black"
        }
        if(cond3){
          data$RaceEth4[i] <- "Hispanic/Latino"
        }
        if(!cond1 & !cond2 & !cond3 & !cond4){
          data$RaceEth4[i] <- "Other (including biracial)"
        }
      }
    }
  }
  
  cond <- !is.na(data[,"Race_ChooseNotToAnswer"]) & data[,"Race_ChooseNotToAnswer"]!=""
  data$RaceEth4[cond] <- NA
  
  return(data$RaceEth4)
}