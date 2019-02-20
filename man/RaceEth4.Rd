\name{RaceEth4}
\alias{RaceEth4}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scoring of RaceEth4
}
\description{
The function return the RaceEth4 given the data.

If “Yes" to DEM4LATINO, then RaceEth4 = Hispanic/Latino;

If “No" to DEM4LATINO, then,

\itemize{
  \item if only “White” to DEM5RACE, then RaceEth4 = Non-Hispanic White

  \item if only “Black or African American” to DEM5RACE, then RaceEth4 = Non-Hispanic Black

  \item if only “Hispanic or Latino” to DEM5RACE, then RaceEth4 = Hispanic/Latino

  \item if any other choice or combinations of choices to DEM5RACE,  then RaceEth4 = Other (including biracial)
}

}
\usage{
Score_Race(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
}
\format{
The data needs to name the survey variables as follows,

\describe{
  \item{\code{HispanicLatino}}{Are you Hispanic or Latino? - (1) Yes (2) No (3) I don't know/Not sure (4) Choose not to answer}
}

Which one or more of the following would you say is your race? (select all that apply): 1 = Yes; Missing = No
\describe{
  \item{\code{White}}{White}
  \item{\code{Black}}{Black or African American}
  \item{\code{Asian}}{Asian}
  \item{\code{Islander}}{Native Hawaiian or Other Pacific Islander}
  \item{\code{Native}}{American Indian or Alaska Native}
  \item{\code{Other}}{Other}
  \item{\code{HispanicLatino1}}{Hispanic or Latino}
  \item{\code{Race_ChooseNotToAnswer}}{Choose not to answer}
}
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
