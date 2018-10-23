# this is a change
freq <- function(x){
    if(is.na(x)) {y <- 0}
    if(!is.na(x)){
        if(x==1) {y <- 0}
        if(x==2) {y <- 0.067}
        if(x==3) {y <- 0.214}
        if(x==4) {y <- 0.5}
        if(x==5) {y <- 0.786}
        if(x==6) {y <- 1}
        if(x==7) {y <- 2}
        if(x==8) {y <- 3}
        if(x==9) {y <- 4}
        if(x==10) {y <- 5}
        if(x==11) {y <- 0}
    }
    return(y)
}

# this is another change
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
    #for(j in 1:7){
    #    veg[,j][is.na(veg[,j])] <- 0
    #}
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

ScoreFV <- function(data){
    
    index1 <- c("NCIfv1","NCIfv3","NCIfv5","NCIfv7","NCIfv9","NCIfv11","NCIfv13","NCIfv15","NCIfv17")
    FreqQuestions <- data[,index1]
    m <- nrow(data)
    n1 <- ncol(FreqQuestions)
    FVfreq <- as.data.frame(matrix(NA,nrow=m,ncol=n1))
    for(i in 1:m){
        for(j in 1:n1){
            FVfreq[i,j] <- freq(FreqQuestions[i,j])
        }
    }
    
    index2 <- c("NCIfv2","NCIfv4","NCIfv6","NCIfv8","NCIfv10","NCIfv12","NCIfv14","NCIfv16","NCIfv18")
    MyPyramid <- data[,index2]
    Pyramid_Score <- Pyramid(MyPyramid,FVfreq)
    
    FV_Score <- rep(NA,m)
    for(i in 1:m){
        FV_Score[i] <- sum(Pyramid_Score[i,]*FVfreq[i,])
    }
    
    return(FV_Score)
}


