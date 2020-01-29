read_TSV <- function(filepathname){
    
    # check file extension
    n <- nchar(filepathname)
    file_ext <- substr(filepathname,n-3,n)
    if(!(file_ext %in% c(".txt",".tsv"))){
        warning("Unexpected file extension!")
    }
    
    # read data (Note: colClasses = "character" option ensures that the 
    # data are all character type, and missing values are all coded as 
    # "" - empty strings)
    df <- read.delim(filepathname, sep="\t", colClasses = "character")
    
    return(df)
}
