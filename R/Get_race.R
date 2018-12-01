get_race <- function(data){
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  ##########################
  # Fix Race and Ethnicity #
  ##########################
  data$HispanicLatino <- as.character(data$DEM4LATINO)
  data$White <- as.character(data$DEM5RACE_6)
  data$Black <- as.character(data$DEM5RACE_3)
  data$Asian <- as.character(data$DEM5RACE_2)
  data$Islander <- as.character(data$DEM5RACE_5)
  data$Native <- as.character(data$DEM5RACE_1)
  data$Other <- as.character(data$DEM5RACE_7)
  data$Race_ChooseNotToAnswer <- as.character(data$DEM5RACE_8)
  data$Other_TEXT <- as.character(data$DEM5RACE_7_TEXT)
  data$HispanicLatino1 <- as.character(data$DEM5RACE_4)
  # HispanicLatino
  summary(as.factor(data$HispanicLatino))
  data$HispanicLatino <- as.character(data$HispanicLatino)
  data$HispanicLatino[data$HispanicLatino==1] <- "Yes"
  data$HispanicLatino[data$HispanicLatino==2] <- "No"
  data$HispanicLatino[data$HispanicLatino==3] <- NA
  data$HispanicLatino[data$HispanicLatino==4] <- NA
  summary(as.factor(data$HispanicLatino))
  
  # Race
  summary(as.factor(data[,"White"]))
  summary(as.factor(data[,"Black"]))
  summary(as.factor(data[,"Asian"]))
  summary(as.factor(data[,"Islander"]))
  summary(as.factor(data[,"Native"]))
  summary(as.factor(data[,"Other"]))
  summary(as.factor(data[,"Race_ChooseNotToAnswer"]))
  summary(as.factor(data[,"HispanicLatino1"]))
  
  old2YN <- function(v){
    v <- as.character(v)
    cond_No <- is.na(v) | v==""
    cond_Yes <- (!is.na(v)) & (v!="")
    v[cond_No]  <- "No"
    v[cond_Yes] <- "Yes"
    #v <- as.factor(v)
    return(v)
  }
  # summary(old2YN(data[,"White"]))
  
  # Change text to Y/N or ""
  for(v in c("White","Black","Asian","Islander","Native","Other","HispanicLatino1")){
    data[,v] <- old2YN(data[,v])
  }
  # summary(as.factor(data[,"White"]))
  # summary(as.factor(data[,"Black"]))
  # summary(as.factor(data[,"Asian"]))
  # summary(as.factor(data[,"Islander"]))
  # summary(as.factor(data[,"Native"]))
  # summary(as.factor(data[,"Other"]))
  
  summary(as.factor(data[,"Race_ChooseNotToAnswer"]))
  cond <- !is.na(data[,"Race_ChooseNotToAnswer"])
  #sum(data[cond,c("White","Black","Asian","Islander","Native","Other")]=="Yes")
  #data[cond,c("White","Black","Asian","Islander","Native","Other")] <- NA
  data$Race_ChooseNotToAnswer <- NULL
  
  # need to address these
  data$Other_TEXT <- trim(gsub("'","",data$Other_TEXT))
  #data[data$Other_TEXT!="",c("HispanicLatino","White","Black","Asian","Islander","Native","Other","Other_TEXT")]
  
  data[!is.na(data$Other_TEXT) & data$Other_TEXT=="Asian-American","Asian"] <- "Yes"
  data[!is.na(data$Other_TEXT) & data$Other_TEXT=="Asian-American","Other"] <- "No"
  
  data[!is.na(data$Other_TEXT) & data$Other_TEXT=="Hispanic","HispanicLatino"] <- "Yes"
  data[!is.na(data$Other_TEXT) & data$Other_TEXT=="Hispanic","Other"] <- "No"
  
  data[!is.na(data$Other_TEXT) & data$Other_TEXT=="Latino","HispanicLatino"] <- "Yes"
  data[!is.na(data$Other_TEXT) & data$Other_TEXT=="Latino","Other"] <- "No"
  
  data[!is.na(data$Other_TEXT) & data$Other_TEXT=="Latina","HispanicLatino"] <- "Yes"
  data[!is.na(data$Other_TEXT) & data$Other_TEXT=="Latina","Other"] <- "No"
  
  data[!is.na(data$Other_TEXT) & data$Other_TEXT=="White/American Indian or Alaska Native","White"]  <- "Yes"
  data[!is.na(data$Other_TEXT) & data$Other_TEXT=="White/American Indian or Alaska Native","Native"] <- "Yes"
  data[!is.na(data$Other_TEXT) & data$Other_TEXT=="White/American Indian or Alaska Native","Other"]  <- "No"
  
  #data[!is.na(data$Other_TEXT) & data$Other_TEXT=="Asian-American","Asian"] <- "Yes"
  #data[!is.na(data$Other_TEXT) & data$Other_TEXT=="Asian-American","Other"] <- "No"
  
  #subdata <- data[!is.na(data$HispanicLatino) & data$HispanicLatino=="Yes",
  #                c("HispanicLatino","White","Black","Asian","Islander","Native","Other","Other_TEXT")]
  
  summary(as.factor(data[,"White"]))
  summary(as.factor(data[,"Black"]))
  summary(as.factor(data[,"Asian"]))
  summary(as.factor(data[,"Islander"]))
  summary(as.factor(data[,"Native"]))
  summary(as.factor(data[,"HispanicLatino"]))
  summary(as.factor(data[,"Other"]))
  summary(as.factor(data[,"HispanicLatino1"]))
  
  
  # data$RaceEth = rep(NA,nrow(data))
  # data$RaceEth[data$White=="Yes" & data$Black=="No" & data$HispanicLatino=="No" &
  #                data$Asian=="No" & data$Islander=="No" & data$Native=="No" & data$Other=="No"] <- "White Only"
  # data$RaceEth[data$White=="No" & data$Black=="Yes" & data$HispanicLatino=="No" &
  #                data$Asian=="No" & data$Islander=="No" & data$Native=="No" & data$Other=="No"] <- "Black Only"
  # data$RaceEth[data$White=="No" & data$Black=="No" & data$HispanicLatino=="Yes" &
  #                data$Asian=="No" & data$Islander=="No" & data$Native=="No"] <- "Hispanic/Latino Only"
  # data$RaceEth[(((data$White=="Yes") + (data$Black=="Yes") + (data$HispanicLatino=="Yes"))>=2) |
  #                (data$Asian=="Yes" | data$Islander=="Yes" | data$Native=="Yes" | data$Other=="Yes")] <- "Other (including biracial)"
  # data$RaceEth <- factor(data$RaceEth,levels=c("White Only","Black Only","Hispanic/Latino Only","Other (including biracial)"))
  # data$RaceEth[data$Dem5race_7==1] <- NA
  # summary(data$RaceEth)
  
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
  
  data$HispanicLatino <- NULL
  data$White <- NULL
  data$Black <- NULL
  data$Asian <- NULL
  data$Islander <- NULL
  data$Native <- NULL
  data$Other <- NULL
  data$Other_TEXT <- NULL
  
  return(data$RaceEth4)
}