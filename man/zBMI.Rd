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
Score_zBMI(data,age_month=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{data}{The input data}
\item{age_month}{An indicator whether age measured in months is given or not. If age_month=FALSE, then the date of birth (DEM2AGE_YEAR, DEM2AGE_MONTH, DEM2AGE_Date) and survey date (StartDate) are required. If age_month=TRUE, convert the age so that it is measured in the unit of month.}
}
\format{
The data needs the following variables (The coded values are showed after the question text):
  \describe{
    \item{\code{ageinmonth (Needed if age_month=FALSE)}}{The age measured in the unit of month}
    
    \item{\code{DEM2AGE_YEAR (Needed if age_month=TRUE)}}{What year were you born in? 
    
    (1) 1920;
    (2) 1921;
    ... 
    (90) 2009;
    (99) Choose not to answer}
    \item{\code{DEM2AGE_MONTH (Needed if age_month=TRUE)}}{What month were you born in? 
    
    (1) Jan; (2) Feb; ... (12) Dec; (13) Choose not to answer}
    \item{\code{DEM2AGE_Date (Needed if age_month=TRUE)}}{What day were you born on? 
    
    (1) 1; (2) 2; ... (31) 31; (32) Choose not to answer}
    
    \item{\code{StartDate (Needed if age_month=TRUE)}}{System generated time of filling the survey, must have the format "mm/dd/yy hh:mm"}
    
    \item{\code{DEM12GENDER}}{What is your gender?
    
    (1) Male; (2) Female; (3) Other; (4) Choose not to answer}
    \item{\code{BMI}}{Computed from height and weight}
  }
}
\details{
}
\value{
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
