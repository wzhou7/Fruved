get_priorities <- function(data){
  map <- read.csv("/Users/Wangcheng/University of Tennessee/Zhou, Wenjun - WY/3_College Wave 3/Current_Version/PRIORITIES_Survey_Names.csv")
  map[,1] <- as.character(map[,1])
  map[,2] <- trim(as.character(map[,2]))
  
  df <- data[,paste0("PRIORITIES",1:31)]
  dg <- as.data.frame(matrix(NA,nrow=2,ncol=ncol(df)))
  for(j in 1:ncol(df)){
    df[,j][df[,j]==6 | df[,j]==7] <- NA
    dg[1,j] <- sum(!is.na(df[,j]))
    dg[2,j] <- mean(df[,j],na.rm=TRUE)
  }
  if(dg[1,31]<0.5*dg[1,1]){
    dh <- dg[,-31]
  }
  if(dg[1,31]>=0.5*dg[1,1]){
    dh <- dg[,-31]
  }
  all_index <- order(dh[2,],decreasing = TRUE)
  index <- all_index[1:5]
  scores <- rep(NA,16)
  for(i in 1:5){
    if(dh[1,index[i]]>0){
      scores[3*(i-1)+1] <- map[,2][map[,1]==paste0("PRIORTIES_",index[i])]
      scores[3*(i-1)+2] <- dh[1,index[i]]
      scores[3*(i-1)+3] <- dh[2,index[i]]
    }
  }
  if(!is.na(dh[2,all_index[5]]) & !is.na(dh[2,all_index[6]])){
    if(dh[2, all_index[5]] == dh[2, all_index[6]]){
      scores[16] <- map[,2][map[,1]==paste0("PRIORTIES_",all_index[6])]
      for(j in 7:length(all_index)){
        if(!is.na(dh[2,all_index[j]]) & (dh[2,all_index[j]]==dh[2,all_index[5]])){
          scores[16] <- paste(scores[16],
                              map[,2][map[,1]==paste0("PRIORTIES_",all_index[j])],sep=" AND ")
        }
      }
    }
  }
  return(scores)
}