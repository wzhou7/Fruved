Score_zBMI <- function(data,age_month=FALSE){
    if(!age_month){
        data$DEM2AGE_YEAR_NORM <- 1919 + data$DEM2AGE_YEAR
        data$DEM2AGE_YEAR_NORM[data$DEM2AGE_YEAR==99] <- NA
        data$DEM2AGE_MONTH_NORM <- data$DEM2AGE_MONTH
        data$DEM2AGE_MONTH_NORM[data$DEM2AGE_MONTH==13] <- NA
        data$DEM2AGE_DATE_NROM <- data$DEM2AGE_DATE
        data$DEM2AGE_DATE_NROM[data$DEM2AGE_DATE==32] <- NA
        data$DOB <- paste(paste(data$DEM2AGE_MONTH_NORM,data$DEM2AGE_DATE_NROM,sep="/"),data$DEM2AGE_YEAR_NORM,sep="/")
        data$DOB <- as.Date(data$DOB,"%m/%d/%Y")
        data$ageinmonth <- rep(NA,nrow(data))
        for(i in 1:nrow(data)){
            EOD <- as.Date(strsplit(as.character(data$StartDate[i])," ")[[1]][1],"%m/%d/%y")
            if(!is.na(data$DOB[i]) & !is.na(EOD)){
                data$ageinmonth[i] <- age_calc(dob = data$DOB[i], enddate = EOD, units = "months")
            }
        }
    }
    data$ageinmonth_round <- floor(data$ageinmonth) + 0.5
    data$ageinmonth_round[data$ageinmonth <= 24.25] <- 24
    
    data$zBMI <- rep(NA,nrow(data))
    data$zBMI_category <- rep(NA,nrow(data))
    for(i in 1:nrow(data)){
        gender <- data$DEM12GENDER[i]
        X <- data$BMI[i]
        if((!is.na(gender) & gender %in% c(1,2)) & !is.na(data$ageinmonth_round[i]) & !is.na(X)){
            ref_BMI <- Standard_BMI[Standard_BMI$Sex==gender & Standard_BMI$Agemos==data$ageinmonth_round[i],]
            if(nrow(ref_BMI)>0){
                L <- ref_BMI$L
                M <- ref_BMI$M
                S <- ref_BMI$S
                P5 <- ref_BMI$P5
                P85 <- ref_BMI$P85
                P95 <- ref_BMI$P95
                if(L!=0){
                    data$zBMI[i] <- (((X/M)^L)-1)/(L*S)
                }
                if(L==0){
                    data$zBMI[i] <- log(X/M)/S
                }
                if(X<P5){data$zBMI_category[i] <- "Underweight"}
                if(X>=P5 & X<P85){data$zBMI_category[i] <- "Normal"}
                if(X>=P85 & X<P95){data$zBMI_category[i] <- "Overweight"}
                if(X>=P95){data$zBMI_category[i] <- "Obese"}
            }
        }
    }
    
    names(data)[names(data)=="zBMI_category"] <- "zBMI_Category"
    return(data[,c("zBMI","zBMI_Category")])
}