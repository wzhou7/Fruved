\name{Readiness}
\alias{Readiness}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scoring of ``10 Question Readiness'' survey for HS and College students
}
\description{
The function return the total readiness score with its components, including the administration's readiness (AR), the power of wellness efforts to create change (PC), the students' readiness (SR), the individual's perceived power to create change (IP), and the level of interest in working on wellness (LI). The total score ranges from 0 to 100.

For each survey question, 10 points if strongly agree, 7 points if agree, 4 points if neither agree nor disagree, 1 point if disagree, 0 point if strongly disagree or choose not to answer.
}
\usage{
Score_Readiness(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
}
\format{
The data needs to name the survey variables as follows,
  \describe{
    \item{\code{READINESS1}}{My school’s administration highly values promoting health and wellness.}
    \item{\code{READINESS2}}{Wellness programs are a good investment because they improve student health.}
    \item{\code{READINESS3}}{The students on campus are strongly interested in improving their health and wellness.}
    \item{\code{READINESS4}}{I am interested in working with other individuals and groups to help make the ``healthy choice the easy choice'' on campus.}
    \item{\code{READINESS5}}{It seems like my school's administration is interested in making changes to make things better when changes are needed.}
    \item{\code{READINESS6}}{I have the ability to work with other individuals and groups to help improve the health promotion resources on campus.}
    \item{\code{READINESS7}}{Wellness programs are a good investment because they improve student academic success.}
    \item{\code{READINESS8}}{I am willing to dedicate a percentage of my time to working with other individuals and groups to help make the ``healthy choice the easy choic'' on campus.}
    \item{\code{READINESS9}}{If my campus develops more health and wellness programming and resources, students will use it.}
    \item{\code{READINESS10}}{I have the power to influence how resources on campus are allocated to help promote health and wellness.}
  }
  
Each question has the following choices:
(1) Strongly agree; (2) Agree; (3) Neither agree nor disagree; (4) Disagree; (5) Strongly disagree; (6) Choose not to answer.

AR: Question 1 and 5

PC: Question 2 and 7

SR: Question 3 and 9

IP: Question 6 and 10

LI: Question 4 and 8
}
\details{
}
\value{
\item{TOTAL}{The total readiness score}
\item{AR}{Administration's readiness}
\item{PC}{Power of wellness efforts to create change}
\item{SR}{Students' readiness}
\item{IP}{Individual's perceived power to create change}
\item{LI}{Level of interest in working on wellness}
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
