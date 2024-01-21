flag_plausible <- function(x, type="weight", min_plausible=2.1, max_plausible=650){
    y <- rep("", length(x))
    y[is.na(x)] <- paste("missing", type)
    y[(!is.na(x))&(x<min_plausible)] <- paste("implausible",type,"(too low)")
    y[(!is.na(x))&(x>max_plausible)] <- paste("implausible",type,"(too high)")
    return(y)
}

flag_outlier <- function(x, type, y){
    num_obs <- sum(!is.na(x))
    if(num_obs >= 30){
        xMedian <- median(x, na.rm=TRUE)
        xIQR <- IQR(x, na.rm=TRUE)
        
        extreme_low_cut <- xMedian - 3 * xIQR
        low_cut <- xMedian - 1.5 * xIQR
        y[(y=="")&(x<extreme_low_cut)] <- paste(type, "outlier (extrmely low)")
        y[(y=="")&(x<low_cut)] <- paste(type, "outlier (low)")
        
        extreme_high_cut <- xMedian + 3 * xIQR
        high_cut <- xMedian + 1.5 * xIQR
        y[(y=="")&(x>extreme_high_cut)] <- paste(type, "outlier (extrmely high)")
        y[(y=="")&(x>high_cut)] <- paste(type, "outlier (high)")
    }
    return(y)
}

ConvertWt <- function(data, wt = "WeightLB", wt_unit = "lb"){
    wt_unit <- tolower(wt_unit)
    if(wt_unit %in% c("lb","kg")){
        # convert implemented units to kg
        if(wt_unit=="lb"){
            data$WeightKG <- data[,wt] * 0.45359237 # pound to kilogram
        } else {
            data$WeightKG <- data[,wt]
        }
        # flag implausible values and outliers
        data$WeightFlag <- flag_plausible(data$WeightKG, "weight", min_plausible=2.1, max_plausible=650)
        data$WeightFlag <- flag_outlier(data$WeightKG, "weight", data$WeightFlag)
        return(data)
    } else {
        stop(paste("The unit",wt_unit,"is not implemented!"))
    }
}

ConvertHt <- function(data, ht = "HeightIN", ht_unit = "in"){
    ht_unit <- tolower(ht_unit)
    if(ht_unit %in% c("in","m")){
        # convert implemented units to meter
        if(ht_unit=="in"){
            data$HeightM <- data[,ht] * 0.0254 # inch to meter
        } else {
            data$HeightM <- data[,ht]
        }
        # flag implausible values and outliers
        data$HeightFlag <- flag_plausible(data$HeightM, "height", min_plausible=0.556, max_plausible=2.72)
        data$HeightFlag <- flag_outlier(data$HeightM, "height", data$HeightFlag)
        return(data)
    } else {
        stop(paste("The unit",ht_unit,"is not implemented!"))
    }
}

Score_BMI_Adults <- function(data, wt="WeightLB", ht = "HeightIN", wt_unit = "lb", ht_unit = "in"){
    
    # if these variables already exist, rename them
    varnames <- c("WeightKG","WeightFlag","HeightM","HeightFlag","BMI","BMIFlag","BMI_Category")
    for(v in varnames){
        v_old <- paste0(v,"_old")
        data[,v_old] <- data[,v]
    }
    
    # convert weight into KG
    data <- ConvertWt(data, wt, wt_unit)
    
    # convert height into M
    data <- ConvertHt(data, ht, ht_unit)
    
    # calculate BMI
    data$BMI <- data$WeightKG/data$HeightM/data$HeightM
    
    # flag implausible values and outliers
    data$BMIFlag <- flag_plausible(data$BMI, "BMI", min_plausible=6.7, max_plausible=251.1)
    data$BMIFlag <- flag_outlier(data$BMI, "BMI", data$BMIFlag)
    
    # categorize BMI
    data$BMI_Category <- NA
    data$BMI_Category[(!is.na(data$BMI))&(data$BMI<18.5)] <- "Underweight"
    data$BMI_Category[(!is.na(data$BMI))&(data$BMI>=18.5)&(data$BMI<25)] <- "Healthy Weight"
    data$BMI_Category[(!is.na(data$BMI))&(data$BMI>=25)&(data$BMI<30)] <- "Overweight"
    data$BMI_Category[(!is.na(data$BMI))&(data$BMI>=30)] <- "Obesity"
    
    return(data)
}

CDC_AgeMonth_fromDOB <- function(data, birth_date="DOB", data_date = "StartDate"){
    cdc_age_month <- difftime(data$DOB, data$EOD, unit="days")/30.35
    return(cdc_age_month)
}

Score_zBMI <- function(data, cdc_age_month = "cdc_age_month", gender = "Gender"){
    
    # missing age: BMI category will not be known
    data[,"AgeMonths"] <- data[,cdc_age_month]
    data_age0 <- subset(data, is.na(data[,"AgeMonths"]))
    data_age1 <- subset(data, !is.na(data[,"AgeMonths"]))
    data_age0$BMI_Category <- NA
    
    # missing gender: will try 
    data[,"Gender"] <- data[,gender]
    data_gender0 <- subset(data_age1, is.na(data[,"Gender"]))
    data_gender1 <- subset(data_age1, !is.na(data[,"Gender"]))
    
    # determine BMI category
    load("data/BMI_Percentiles.RData")
    data_gender1 <- merge(data_gender1, BMI_Percentiles, by=c("Gender", "AgeMonths"), by.x=TRUE, by.y=FALSE)
    data_gender1$BMI_Category <- ifelse(data_gender1$BMI<data_gender1$P5, "Underweight",
                                        ifelse(data_gender1$BMI<data_gender1$P85, "Healthy Weight",
                                               ifelse(data_gender1$BMI<data_gender1$P95, "Overweight","Obesity")))
    
    data_gender0_M <- merge(data_gender0, BMI_Percentiles["Gender"=="M",], by=c("AgeMonths"), by.x=TRUE, by.y=FALSE)
    data_gender0_F <- merge(data_gender0, BMI_Percentiles["Gender"=="F",], by=c("AgeMonths"), by.x=TRUE, by.y=FALSE)
    data_gender0_M$BMI_Category_M <- ifelse(data_gender0_M$BMI<data_gender1$P5, "Underweight",
                                            ifelse(data_gender0_M$BMI<data_gender1$P85, "Healthy Weight",
                                                   ifelse(data_gender0_M$BMI<data_gender1$P95, "Overweight","Obesity")))
    data_gender0_F$BMI_Category_F <- ifelse(data_gender0_F$BMI<data_gender1$P5, "Underweight",
                                            ifelse(data_gender0_F$BMI<data_gender1$P85, "Healthy Weight",
                                                   ifelse(data_gender0_F$BMI<data_gender1$P95, "Overweight","Obesity")))
    data_gender0 <- merge(data_gender0_M, data_gender0_F[,BMI_Category_F], by.x = 0, by.y = 0)
    data_gender0$BMI_Category <- ifelse(BMI_Category_M==BMI_Category_F, BMI_Category_M, NA)
    data_gender0$BMI_Category_F <- NULL
    data_gender0$BMI_Category_M <- NULL
    
    # join back
    data_gender <- rbind(data_gender1, data_gender0)
    data <- rbind(data_age0,data_gender)
    return(data)
}

Score_BMI <- function(data, wt = "WeightLB", ht = "HeightIN", 
                      wt_unit = "lb", ht_unit = "in", 
                      birth_date = NULL, data_date = NULL, gender = NULL){
    
    if(is.null(birth_date)){
        
        warning(paste("Age not available. All records assumed to be adults."))
        data <- Score_BMI_Adults(data, wt, ht, wt_unit, ht_unit)
        
    } else { # assume at least some kids; age must be available (if missing, BMI category cannot be computed)
        
        # compute age
        data$cdc_age_month <- CDC_AgeMonth_fromDOB(data, birth_date, data_date)
        
        # infants: BMI cannot be computed
        data_infant <- subset(data, data$cdc_age_month<24)
        num_infants <- NROW(data_infant)
        if(num_infants>0){
            warning(paste("BMI cannot be computed for", num_infants, "infants in the data."))
        }
        
        # adults & children, use adult calculator
        data_noninfant <- subset(data, data$cdc_age_month>=24)
        data_noninfant <- Score_BMI_Adults(data_noninfant, wt, ht, wt_unit, ht_unit)
        
        # separate adults and children
        data_adults <- subset(data_noninfant, data$cdc_age_month>240.5)
        data_children <- subset(data_noninfant, data$cdc_age_month<=240.5)
        
        # children BMI_Categories depends on age and gender group
        data_children$BMI_Categories <- NULL
        data_children <- Score_zBMI(data_children, cdc_age_month = "cdc_age_month", gender)
        data <- rbind(data_adults, data_children)
        
        data_infant$BMI <- NA
        data_infant$BMI_Category <- NA
        data <- rbind(data_infant, data)
        
    }
    return(data)
}
