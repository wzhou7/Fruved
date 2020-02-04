\name{BMI}
\alias{BMI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scoring of BMI
}
\description{
The function returns the BMI. 

Make sure the unit of height is centimeter (cm) and the unit of weight is kilogram (kg).
}
\usage{
Score_BMI(data,ht="Height",wt="Weight")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{data}{input data}
\item{ht}{the variable name of height in the data. The default is ``Height''}
\item{wt}{the variable name of weight in the data. The default is ``Weight''}
}
\format{
}
\details{
}
\value{
}
\references{
https://en.wikipedia.org/wiki/Body_mass_index
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
library(Fruved)
data("STRESS_IPAQ_BMI")
Score_BMI(STRESS_IPAQ_BMI)
}
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
