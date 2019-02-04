\name{PSQI}
\alias{PSQI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scoring of the Pittsburgh Sleep Quality Index (PSQI)
}
\description{
The function return the PSQI score. The smaller the score, the better the sleep quality.
}
\usage{
Score_PSQI(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
}
\format{
The data needs to name the survey variables as follows:
  \describe{
    \item{\code{Sleep1}}{When have you usually gone to bed?}
    \item{\code{Sleep2}}{How long (in minutes) has it taken you to fall asleep each night?}
    \item{\code{Sleep3}}{When have you usually gotten up in the morning?}
    \item{\code{Sleep4}}{How many hours of actual sleep do you get at night? (This may be different than the number of hours you spend in bed)}
  }
  During the past month, how often have you had trouble sleeping because you... The coded values are as following,
  
  (1) Not during the past month; (2) Less than once a week; (3) Once or twice a week; (4) Three or more times a week
  \describe{
    \item{\code{Sleep5a}}{Cannot get to sleep within 30 minutes}
    \item{\code{Sleep5b}}{Wake up in the middle of the night or early morning}
    \item{\code{Sleep5c}}{Have to get up to use the bathroom}
    \item{\code{Sleep5d}}{Cannot breathe comfortably}
    \item{\code{Sleep5e}}{Cough or snore loudly}
    \item{\code{Sleep5f}}{Feel too cold}
    \item{\code{Sleep5g}}{Feel too hot}
    \item{\code{Sleep5h}}{Have bad dreams}
    \item{\code{Sleep5i}}{Have pain}
    \item{\code{Sleep5j}}{Other reason(s), please describe, including how often you 
have had trouble sleeping because of this reason(s):}
    \item{\code{Sleep6}}{During the past month, how often have you taken medicine 
(prescribed or “over the counter”) to help you sleep?

(1) Not during the past month; (2) Less than once a week; (3) Once or twice a week; (4) Three or more times a week}
    \item{\code{Sleep7}}{During the past month, how often have you had trouble staying 
awake while driving, eating meals, or engaging in social activity?

(1) Not during the past month; (2) Less than once a week; (3) Once or twice a week; (4) Three or more times a week}
    \item{\code{Sleep8}}{During the past month, how much of a problem has it been for 
you to keep up enthusiasm to get things done?

(1) Not during the past month; (2) Less than once a week; (3) Once or twice a week; (4) Three or more times a week}
    \item{\code{Sleep9}}{During the past month, how would you rate your sleep  
quality overall?

(1) Very good; (2) Fairly good; (3) Fairly bad; (4) Very bad}
  }
}
\details{
}
\value{
}
\references{
Buysse,D.J., Reynolds,C.F., Monk,T.H., Berman,S.R., & Kupfer,D.J. (1989). The Pittsburgh Sleep Quality Index (PSQI): A new instrument for psychiatric research and practice. Psychiatry Research, 28(2), 193-213. Available at: https://www.sleep.pitt.edu/research/instruments.html
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
