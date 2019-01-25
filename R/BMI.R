Score_BMI <- function(data,ht="Height",wt="Weight"){
  names(data)[names(data)==ht] <- "Height"
  names(data)[names(data)==wt] <- "Weight"
  BMI <- data$Weight/((data$Height/100)^2)
  if(median(BMI,na.rm=TRUE) < 10 | median(BMI,na.rm=TRUE) > 65){
    warning("BMI range is abnormal. Make sure the unit of height is centimeter (cm) and the unit of weight is kilogram (kg)")
  }
  return(BMI)
}
