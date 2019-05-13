freqency <- function(x){
  y <- rep(NA,length(x))
  y[x==1] <- 0
  y[x==2] <- 0.018
  y[x==3] <- 0.066
  y[x==4] <- 0.214
  y[x==5] <- 0.499
  y[x==6] <- 0.784
  y[x==7] <- 1
  y[x==8] <- 2
  return(y)
}

serving <- function(x,gender){
  if(is.na(gender) | gender=="" | !(gender %in% c(1,2))){y <- NA}
  else{
    if(gender==1){
      if(x=="Ncifat1"){y <- 74.666667}
      if(x=="Ncifat2"){y <- 366.666667}
      if(x=="Ncifat3"){y <- 92}
      if(x=="Ncifat4"){y <- 25}
      if(x=="Ncifat5" | x=="Ncifat11" | x=="Ncifat15"){y <- 9.54}
      if(x=="Ncifat6"){y <- 373.2}
      if(x=="Ncifat7"){y <- 131.75}
      if(x=="Ncifat8"){y <- 114}
      if(x=="Ncifat9"){y <- 33.36}
      if(x=="Ncifat10"){y <- 112.5}
      if(x=="Ncifat12"){y <- 13.75}
      if(x=="Ncifat13"){y <- 36.72}
      if(x=="Ncifat14"){y <- 213.625}
    }
    if(gender==2){
      if(x=="Ncifat1"){y <- 50}
      if(x=="Ncifat2"){y <- 245}
      if(x=="Ncifat3"){y <- 80}
      if(x=="Ncifat4"){y <- 26}
      if(x=="Ncifat5" | x=="Ncifat11" | x=="Ncifat15"){y <- 7}
      if(x=="Ncifat6"){y <- 249}
      if(x=="Ncifat7"){y <- 118}
      if(x=="Ncifat8"){y <- 57}
      if(x=="Ncifat9"){y <- 26.175}
      if(x=="Ncifat10"){y <- 79.5}
      if(x=="Ncifat12"){y <- 13.75}
      if(x=="Ncifat13"){y <- 30.63}
      if(x=="Ncifat14"){y <- 158}
    }
  }
  return(y)
}

Score_NCIFAT <- function(data){
  subdata <- data[,c(paste("Ncifat",1:17,sep=""),"gender")]
  freq <- matrix(NA,nrow=nrow(subdata),ncol=15)
  
  for(j in 1:15){
    freq[,j] <- freqency(subdata[,j])
  }

  
  serve <- matrix(NA,nrow=nrow(subdata),ncol=15)
  for(i in 1:nrow(subdata)){
    for(j in 1:15){
      serve[i,j] <- serving(colnames(subdata)[j],subdata[i,18])
    }
  }
  
  totfat <- freq[,5]*serve[,5] + freq[,11]*serve[,11] + freq[,15]*serve[,15]
  regfat <- c()
  for(i in 1:nrow(subdata)){
    if(is.na(subdata[i,16]) | subdata[i,16]==""){regfat[i] <- NA}
    else{
      if(subdata[i,16]==1 | subdata[i,16]==2){regfat[i] <- totfat[i]}
      if(subdata[i,16]==3){regfat[i] <- 0.75*totfat[i]}
      if(subdata[i,16]==4){regfat[i] <- 0.5*totfat[i]}
      if(subdata[i,16]==5){regfat[i] <- 0.25*totfat[i]}
      if(subdata[i,16]==6){regfat[i] <- 0}
    }
  }
  
  Fat <- matrix(NA,nrow=nrow(subdata),ncol=13)
  Fat[,1] <- freq[,1]*serve[,1]
  Fat[,2] <- freq[,2]*serve[,2]
  Fat[,3] <- freq[,3]*serve[,3]
  Fat[,4] <- freq[,4]*serve[,4]
  Fat[,5] <- freq[,6]*serve[,6]
  Fat[,6] <- freq[,7]*serve[,7]
  Fat[,7] <- freq[,8]*serve[,8]
  Fat[,8] <- freq[,9]*serve[,9]
  Fat[,9] <- freq[,10]*serve[,10]
  Fat[,10] <- freq[,12]*serve[,12]
  Fat[,11] <- freq[,13]*serve[,13]
  Fat[,12] <- freq[,14]*serve[,14]
  Fat[,13] <- regfat
  
  predict_pcf <- rep(NA,nrow(subdata))
  for(i in 1:nrow(subdata)){
    indicator <- 0
    for(j in 1:13){
      if(is.na(Fat[i,j])){indicator <- indicator+1}
    }
    if(is.na(subdata[i,18]) | !(subdata[i,18] %in% c(1,2))){indicator <- indicator+1}
    
    if(indicator==0){
      if(subdata[i,18]==1){
        predict_pcf[i] <- 30.795765 - (0.022086*Fat[i,1]) - (0.009666*Fat[i,2]) +
          (0.026997*Fat[i,3]) + (0.109569*Fat[i,4]) - (0.004946*Fat[i,5])
        - (0.009346*Fat[i,6]) +
          (0.040118*Fat[i,7]) + (0.069945*Fat[i,8]) + (0.024262*Fat[i,9]) +
          (0.145026*Fat[i,10]) + (0.114649*Fat[i,11]) - (0.017017*Fat[i,12]) +
          (0.167937*Fat[i,13])
      }
      if(subdata[i,18]==2){
        predict_pcf[i] <- 29.86587 - (0.045171*Fat[i,1]) - (0.010393*Fat[i,2]) +
          (0.036787*Fat[i,3]) + (0.198808*Fat[i,4]) - (0.010141*Fat[i,5])
        - (0.012103*Fat[i,6]) +
          (0.106686*Fat[i,7]) + (0.103239*Fat[i,8]) + (0.040374*Fat[i,9]) +
          (0.287044*Fat[i,10]) + (0.182758*Fat[i,11]) - (0.014224*Fat[i,12]) +
          (0.326702*Fat[i,13])
      }
    }
  }
  
  return(predict_pcf)
}