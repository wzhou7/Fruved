\name{make_table}
\alias{make_table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a table for reporting
%%  ~~function to do ... ~~
}
\description{
Creates a two-column comparison table and runs basic testing for differences.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
make_table(df, row_vars_catg, row_vars_cont, col_var, compare_levels)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{ data frame
%%     ~~Describe \code{df} here~~
}
  \item{row_vars_catg}{a vector of cattegorical variables to list on the rows
%%     ~~Describe \code{row_vars_catg} here~~
}
  \item{row_vars_cont}{a vector of continuous variables to list on the rows
%%     ~~Describe \code{row_vars_cont} here~~
}
  \item{col_var}{the grouping variable
%%     ~~Describe \code{col_var} here~~
}
  \item{compare_levels}{the two levels to compare 
%%     ~~Describe \code{compare_levels} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (df, row_vars_catg, row_vars_cont, col_var, compare_levels) 
{
    df <- subset(df, df[, col_var] \%in\% compare_levels)
    header <- matrix("", nrow = 2, ncol = 6)
    header[1, 2] <- compare_levels[1]
    header[1, 4] <- compare_levels[2]
    header[2, 2] <- paste0("(n=", sum(df[, col_var] == compare_levels[1]), 
        ")")
    header[2, 4] <- paste0("(n=", sum(df[, col_var] == compare_levels[2]), 
        ")")
    header <- rbind(header, c("Categorical Variable", "n", "\%", 
        "n", "\%", "p"))
    for (v in row_vars_catg) {
        t <- as.matrix(table(df[, v], df[, col_var]))
        pct1 <- t[, 1]/sum(t[, 1]) * 100
        pct2 <- t[, 2]/sum(t[, 2]) * 100
        p <- chisq.test(t)$p.value
        out <- cbind(rownames(t), t[, 1], pct1, t[, 2], pct2, 
            "")
        out <- rbind(c(v, rep("", 4), p), out)
        header <- rbind(header, out)
    }
    header <- rbind(header, c("Continuous Variable", "m", "sd", 
        "m", "sd", "p"))
    for (v in row_vars_cont) {
        x <- df[df[, col_var] == compare_levels[1], v]
        y <- df[df[, col_var] == compare_levels[2], v]
        header <- rbind(header, c(v, mean(x, na.rm = T), mean(y, 
            na.rm = T), sd(x, na.rm = T), sd(y, na.rm = T), wilcox.test(x, 
            y)$p.value))
    }
    return(header)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
