# Scaling: 0 = Never; 1 = Almost Never; 2 = Sometimes; 3 = Fairly often; 4 = Very often 
# PSS-14 scores are obtained by reversing the scores on the seven positive items, 
# e.g., 0=4, 1=3, 2=2, etc., and then summing across all 14 items. 
# Items 4, 5, 6, 7, 9, 10, and 13 are the positively stated items.
StressItem <- function(x,rev){
    for(i in 1:length(x)){
        if(is.na(x[i])){x[i] <- 9999}
        else{
            if(rev){
                if(x[i]==6){x[i] <- 9999}
                else{
                    x[i] <- 5-x[i]
                } 
            }
            else{
                if(x[i]==6){x[i] <- 9999}
                else{
                    x[i] <- x[i]-1
                }
            }
        }
    }
    return(x)
}

StressScore <- function(df){
    StressScore <- 0
    
    StressScore <- StressScore + StressItem(df[,1],FALSE)
    StressScore <- StressScore + StressItem(df[,2],FALSE)
    StressScore <- StressScore + StressItem(df[,3],FALSE)
    StressScore <- StressScore + StressItem(df[,4],TRUE)
    StressScore <- StressScore + StressItem(df[,5],TRUE)
    StressScore <- StressScore + StressItem(df[,6],TRUE)
    StressScore <- StressScore + StressItem(df[,7],TRUE)
    StressScore <- StressScore + StressItem(df[,8],FALSE)
    StressScore <- StressScore + StressItem(df[,9],TRUE)
    StressScore <- StressScore + StressItem(df[,10],TRUE)
    StressScore <- StressScore + StressItem(df[,11],FALSE)
    StressScore <- StressScore + StressItem(df[,12],FALSE)
    StressScore <- StressScore + StressItem(df[,13],TRUE)
    StressScore <- StressScore + StressItem(df[,14],FALSE)
    
    return(StressScore)
}

# Cohen Perceived Stress Scale (CPSS)
# More Info: http://www.midss.org/content/perceived-stress-scale-pss
ScoreStress <- function(data){
    index <- c("Stress1","Stress2","Stress3","Stress4","Stress5","Stress6","Stress7",
               "Stress8","Stress9","Stress10","Stress11","Stress12","Stress13","Stress14")
    
    StressQuestions <- data[,index]
    Stress_Score <- StressScore(StressQuestions)
    Stress_Score[Stress_Score>9999] <- NA
    
    return(Stress_Score)
}

# References:
# Andreou, E., Alexopoulos, E. C., Lionis, C., Varvogli, L., Gnardellis, C., 
# Chrousos, G. P., & Darviri, C. (2011). Perceived Stress Scale: Reliability 
# and Validity Study in Greece. International Journal of Environmental Research 
# and Public Health, 8(8), 3287-3298. http://doi.org/10.3390/ijerph8083287
# URL: http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3166743/

