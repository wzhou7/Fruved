flag_plausible <- function(x, type="weight", min_plausible=2, max_plausible=800){
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
        data$WeightFlag <- flag_plausible(data$WeightKG, "weight", min_plausible=2, max_plausible=800)
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
        data$HeightFlag <- flag_plausible(data$HeightM, "height", min_plausible=0.5, max_plausible=3.0)
        data$HeightFlag <- flag_outlier(data$HeightM, "height", data$HeightFlag)
        return(data)
    } else {
        stop(paste("The unit",ht_unit,"is not implemented!"))
    }
}

Score_BMI <- function(data, wt="WeightLB", ht = "HeightIN", wt_unit = "lb", ht_unit = "in"){
    
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
    data$BMIFlag <- flag_plausible(data$BMI, "BMI", min_plausible=5, max_plausible=200)
    data$BMIFlag <- flag_outlier(data$BMI, "BMI", data$BMIFlag)
    
    # categorize BMI
    data$BMI_Category <- NA
    data$BMI_Category[(!is.na(data$BMI))&(data$BMI<18.5)] <- "Underweight"
    data$BMI_Category[(!is.na(data$BMI))&(data$BMI>=18.5)&(data$BMI<25)] <- "Healthy Weight"
    data$BMI_Category[(!is.na(data$BMI))&(data$BMI>=25)&(data$BMI<30)] <- "Overweight"
    data$BMI_Category[(!is.na(data$BMI))&(data$BMI>=30)] <- "Obesity"
    
    return(data)
}
