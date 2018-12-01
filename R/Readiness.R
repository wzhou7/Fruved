Score_Readiness <- function(data){
  vars <- paste0("READINESS",1:10)
  df <- as.data.frame(matrix(NA,nrow=nrow(data),ncol=10))
  for(j in 1:10){
    var <- vars[j]
    df[,j][!is.na(data[,var]) & data[,var]==1] <-  10
    df[,j][!is.na(data[,var]) & data[,var]==2] <-  7
    df[,j][!is.na(data[,var]) & data[,var]==3] <-  4
    df[,j][!is.na(data[,var]) & data[,var]==4] <-  1
    df[,j][!is.na(data[,var]) & data[,var]==5] <-  0
  }
  out <- as.data.frame(matrix(NA,nrow=nrow(data),ncol=6))
  names(out) <- c("Total","AR","SR","IP","LI","PC")
  out$AR <- df[,1] + df[,5]
  out$PC <- df[,2] + df[,7]
  out$SR <- df[,3] + df[,9]
  out$IP <- df[,6] + df[,10]
  out$LI <- df[,4] + df[,8]
  out$Total <- out$AR + out$PC + out$SR + out$IP + out$LI
  names(out) <- paste0("Readiness_",names(out))
  return(out)
}
