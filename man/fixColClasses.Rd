\name{fixColClasses}
\alias{fixColClasses}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fix Column Classes
}
\description{
This function will be used to convert a data frame imported 
using default/common read functions in R without the 
colClasses = "character" option. 
The goal is to ensure that all variables are all of the 
character string type, and missing values are all coded as 
"" - an empty string rather than NA.
}
\usage{
fixColClasses(data,vars)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{data}{input data}
\item{vars}{the variable names to convert to characters}
}
\format{
}
\details{
}
\value{
}
\references{
}
\author{
Wangcheng Yan, Wenjun Zhou
}
\note{
}
\section{Warning }{
}
\seealso{
}
\examples{
}
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
