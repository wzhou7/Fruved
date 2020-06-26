# df = data frame
# row_vars = a vector of variables to list on the rows
# col_var = a factor variable to use to make columns
make_table <- function(df, row_vars_catg, row_vars_cont, col_var, compare_levels){
    df <- subset(df, df[,col_var] %in% compare_levels)
    
    # make header
    header <- matrix("",nrow=2,ncol=6)
    header[1,2] <- compare_levels[1]
    header[1,4] <- compare_levels[2]
    header[2,2] <- paste0("(n=",sum(df[,col_var]==compare_levels[1]),")")
    header[2,4] <- paste0("(n=",sum(df[,col_var]==compare_levels[2]),")")
    
    # for each categorical variable, make the table
    header <- rbind(header,c("Categorical Variable","n","%","n","%","p"))
    for(v in row_vars_catg){
        t <- as.matrix(table(df[,v],df[,col_var]))
        pct1 <- t[,1]/sum(t[,1])*100
        pct2 <- t[,2]/sum(t[,2])*100
        p <- chisq.test(t)$p.value
        out <- cbind(rownames(t),t[,1],pct1,t[,2],pct2,"")
        out <- rbind(c(v,rep("",4),p),out)
        header <- rbind(header,out)
    }
    
    # for each continuous variable, make the table row
    header <- rbind(header,c("Continuous Variable","m","sd","m","sd","p"))
    for(v in row_vars_cont){
        x <- df[df[,col_var]==compare_levels[1],v]
        y <- df[df[,col_var]==compare_levels[2],v]
        header <- rbind(header,c(v,
                                 mean(x,na.rm=T),
                                 mean(y,na.rm=T),
                                 sd(x,na.rm=T),
                                 sd(y,na.rm=T),
                                 wilcox.test(x,y)$p.value))
    }
    return(header)
}


