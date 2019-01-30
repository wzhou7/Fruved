\name{zBMI}
\alias{zBMI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scoring of zBMI and zBMI category
}
\description{
The function returns the age (measured in months), zBMI, and zBMI category given the data.
}
\usage{
Score_zBMI(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
}
\format{
The data needs the following variables (The coded values are showed after the question text):
  \describe{
    \item{\code{DEM2AGE_YEAR}}{What year were you born in? 
    
    (1) 1920;
    (2) 1921;
    ... 
    (90) 2009;
    (99) Choose not to answer}
    \item{\code{DEM2AGE_MONTH}}{What month were you born in? 
    
    (1) Jan; (2) Feb; ... (12) Dec; (13) Choose not to answer}
    \item{\code{DEM2AGE_Date}}{What day were you born on? 
    
    (1) 1; (2) 2; ... (31) 31; (32) Choose not to answer}
    \item{\code{DEM12GENDER}}{What is your gender?
    
    (1) Male; (2) Female; (3) Other; (4) Choose not to answer}
    \item{\code{BMI}}{Computed from height and weight}
    \item{\code{StartDate}}{System generated time of filling the survey, must have the format "mm/dd/yy hh:mm"}
  }
}
\details{
}
\value{
\item{ageinmonth}{Computed age measured in months}
\item{zBMI}{A continuous value}
\item{zBMI_Category}{A categorical value in one of the four levels: Underweight, Normal, Overweight, Obese}
}
\references{
https://www.cdc.gov/growthcharts/percentile_data_files.htm
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
