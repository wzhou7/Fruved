\name{CEPS_College}
\alias{CEPS_College}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scoring of CEPS survey for college students
}
\description{
The function returns the CEPS scores for college students. If your survey subjects are high school students, refer to CEPS_HS.
}
\usage{
Score_CEPS_College(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
}
\format{
All the questions have six choices: (1) Strongly Agree; (2) Agree; (3) Neither Agree nor Disagree; (4) Disagree; (5) Strongly Disagree; (6) Choose not to answer

The question name and text are as follows,
\describe{
  \item{\code{CEPS1}}{There are plenty of exercise classes offered at the rec center on campus}
  \item{\code{CEPS2}}{There are policies (e.g. no cars on campus) that promote physical activity)}
  \item{\code{CEPS3}}{There are policies on campus (e.g. limits on sizes of sodas, minimum healthy items in vending machines) that promote healthy eating}
  \item{\code{CEPS4}}{The university’s exercise facilities and equipment are in good condition}
  \item{\code{CEPS5}}{The water in the water fountains on campus taste good}
  \item{\code{CEPS6}}{The campus living environment allows for quiet and restful sleep}
  \item{\code{CEPS7}}{There are programs on campus that promote stress management}
  \item{\code{CEPS8}}{There are healthy foods available on campus}
  \item{\code{CEPS9}}{There are lots of healthy choices in vending machines on campus}
  \item{\code{CEPS10}}{There are sports (intramural or club) available to play on campus}
  \item{\code{CEPS11}}{There are healthy foods available where I usually eat in dining halls on campus}
  \item{\code{CEPS12}}{There are signs telling me which foods are healthy in vending machines on campus}
  \item{\code{CEPS13}}{The water/drinking fountains on campus look clean}
  \item{\code{CEPS14}}{Most buildings on campus have water/drinking fountains}
  \item{\code{CEPS15}}{There are plenty of opportunities on campus to be moderately or vigorously active}
}
}
\details{
}
\value{
\item{CEPS_Policy}{CEPS2 + CEPS3, total 20 points}
\item{CEPS_Food}{CEPS8 + CEPS11, total 20 points}
\item{CEPS_Water}{CEPS5 + CEPS13 + CEPS14, total 12 points}
\item{CEPS_Vending}{CEPS9 + CEPS12, total 12 points}
\item{CEPS_PA}{CEPS1 + CEPS4 + CEPS10 + CEPS15, total 24 points}
\item{CEPS_Stress}{CEPS7, total 6 points}
\item{CEPS_Sleep}{CEPS6, total 6 points}
\item{CEPS_Total}{Sum of the above components, total 100 points}
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
