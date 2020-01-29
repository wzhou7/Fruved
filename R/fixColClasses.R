# This function will be used to convert a data frame imported 
# using default/common read functions in R without the 
# colClasses = "character" option. 
# The goal is to ensure that all variables are all of the 
# character string type, and missing values are all coded as 
# "" - an empty string rather than NA.

fixColClasses <- function(df,vars){
    for(v in vars){
        old_data <- df[,v]
        new_data <- as.character(old_data)
        new_data[is.na(new_data)] <- ""
        df[,v] <- new_data
    }
    return(df)
}
