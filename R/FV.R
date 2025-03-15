freq <- function(x){
    y <- rep(NA,length(x))
    y[is.na(x)] <- 0
    y[x==1] <- 0
    y[x==2] <- 0.067
    y[x==3] <- 0.214
    y[x==4] <- 0.5
    y[x==5] <- 0.786
    y[x==6] <- 1
    y[x==7] <- 2
    y[x==8] <- 3
    y[x==9] <- 4
    y[x==10] <- 5
    y[x==11] <- 0
    return(y)
}

Pyramid <- function(x,FVfreq){
    for(s in 1:nrow(x)){
        for(t in 1:ncol(x)){
            if(is.na(x[s,t])){
                x[s,t] <- 6
            }
        }
    }
    y <- as.data.frame(matrix(NA,nrow=nrow(x),ncol=ncol(x)))

    y[,1][x[,1]==1] <- 0
    y[,1][x[,1]==2] <- 0.5
    y[,1][x[,1]==3] <- 1
    y[,1][x[,1]==4] <- 1.625
    y[,1][x[,1]==5] <- 2.5


    y[,2][x[,2]==1] <- 0
    y[,2][x[,2]==2] <- 0.25
    y[,2][x[,2]==3] <- 0.5
    y[,2][x[,2]==4] <- 1.0
    y[,2][x[,2]==5] <- 1.5

    y[,3][x[,3]==1] <- 0
    y[,3][x[,3]==2] <- 0.25
    y[,3][x[,3]==3] <- 0.5
    y[,3][x[,3]==4] <- 1.0
    y[,3][x[,3]==5] <- 1.5


    y[,4][x[,4]==1] <- 0
    y[,4][x[,4]==2] <- 0.2
    y[,4][x[,4]==3] <- 0.5
    y[,4][x[,4]==4] <- 0.75
    y[,4][x[,4]==5] <- 1.3


    y[,5][x[,5]==1] <- 0
    y[,5][x[,5]==2] <- 0.25
    y[,5][x[,5]==3] <- 0.75
    y[,5][x[,5]==4] <- 1.2
    y[,5][x[,5]==5] <- 2.0


    y[,6][x[,6]==1] <- 0
    y[,6][x[,6]==2] <- 0.25
    y[,6][x[,6]==3] <- 0.75
    y[,6][x[,6]==4] <- 1.25
    y[,6][x[,6]==5] <- 2.0


    y[,7][x[,7]==1] <- 0
    y[,7][x[,7]==2] <- 0.25
    y[,7][x[,7]==3] <- 0.75
    y[,7][x[,7]==4] <- 1.5
    y[,7][x[,7]==5] <- 2.25


    y[,8][x[,8]==1] <- 0
    y[,8][x[,8]==2] <- 0.25
    y[,8][x[,8]==3] <- 0.5
    y[,8][x[,8]==4] <- 1.0
    y[,8][x[,8]==5] <- 1.5


    y[,9][x[,9]==1] <- 0
    y[,9][x[,9]==2] <- 0.3
    y[,9][x[,9]==3] <- 1.0
    y[,9][x[,9]==4] <- 1.6
    y[,9][x[,9]==5] <- 2.25

    m <- nrow(x)
    for(i in 1:m){
        for(j in 1:9){
            if(x[i,j]==6  &  FVfreq[i,j]==0){y[i,j] <- 0}
        }
    }

    for(j in 1:9){
        y[,j] <- as.numeric(y[,j])
    }

    veg <- y[,-c(1,2)]
    veg_freq <- FVfreq[,-c(1,2)]
    most_freq_veg <- c()

    for(i in 1:m){
        most_freq_veg[i] <- sum(veg[i,]*veg_freq[i,],na.rm=TRUE)/sum(veg_freq[i,],na.rm=TRUE)
    }

    for(i in 1:m){
        for(j in 3:9){
            if(is.na(y[i,j])){
                y[i,j] <- most_freq_veg[i]
            }
        }
        if(is.na(y[i,1])  &  is.na(y[i,2])){
            y[i,1] <- y[i,2] <- most_freq_veg[i]
        }
        else{
            if(is.na(y[i,1])){y[i,1] <- y[i,2]}
            if(is.na(y[i,2])){y[i,2] <- y[i,1]}
        }
    }

    return(y)
}

#' Scoring FV
#'
#' This function calculates.
#' @param data The input data frame.
#' @return The intake values for fruits and vegetables, etc.
#' @examples
#' df_out <- FV_scores(df_in);
#' @export
FV_scores <- function(data){
    index1 <- c("NCIFV1","NCIFV3","NCIFV5","NCIFV7","NCIFV9","NCIFV11","NCIFV13","NCIFV15","NCIFV17")
    FreqQuestions <- data[,index1]
    FVfreq <- as.data.frame(matrix(NA,nrow=nrow(FreqQuestions),ncol=ncol(FreqQuestions)))
    for(j in 1:ncol(FreqQuestions)){
      FVfreq[,j] <- freq(FreqQuestions[,j])
    }

    index2 <- c("NCIFV2","NCIFV4","NCIFV6","NCIFV8","NCIFV10","NCIFV12","NCIFV14","NCIFV16","NCIFV18")
    MyPyramid <- data[,index2]
    Pyramid_Score <- Pyramid(MyPyramid,FVfreq)

    FV_Score <- rep(NA,nrow(data))
    for(i in 1:nrow(data)){
        FV_Score[i] <- sum(Pyramid_Score[i,]*FVfreq[i,])
        if(sum(!is.na(FreqQuestions[i,]))==0){
          FV_Score[i] <- NA
        }
    }

    return(FV_Score)
}


