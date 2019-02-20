\name{CEPS_HS}
\alias{CEPS_HS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scoring of CEPS survey for high school students
}
\description{
The function returns the CEPS scores for high school students. If your survey subjects are college students, refer to CEPS_College.
}
\usage{
Score_CEPS_HS(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
}
\format{
All the questions have six choices: (1) Strongly Agree; (2) Agree; (3) Neither Agree nor Disagree; (4) Disagree; (5) Strongly Disagree; (6) Choose not to answer

The question name and text are as follows,
\describe{
  \item{\code{CEPS1}}{There are policies at school (e.g. limits on availability of sodas, healthy items in vending machines, healthy items labeled as healthy) that promote healthy eating}
  \item{\code{CEPS2}}{The school’s exercise facilities and equipment are in good condition}
  \item{\code{CEPS3}}{The water in the water fountains at school taste good}
  \item{\code{CEPS4}}{There are programs at school that promote stress management}
  \item{\code{CEPS5}}{There are enough different kinds of foods available at school in the cafeteria}
  \item{\code{CEPS6}}{The foods available at school in the cafeteria usually taste good}
  \item{\code{CEPS7}}{There is enough time to get and eat lunch at school on most days}
  \item{\code{CEPS8}}{There are enough healthy food options available at school in the cafeteria}
  \item{\code{CEPS9}}{There are lots of healthy choices in vending machines at school}
  \item{\code{CEPS10}}{There are enough sports available to play after school}
  \item{\code{CEPS11}}{The water/drinking fountains on campus look clean}
  \item{\code{CEPS12}}{There are enough water/drinking fountains available}
  \item{\code{CEPS13}}{There are enough opportunities to be physically active during school}
}
}
\details{
}
\value{
\item{CEPS_Policy}{CEPS1, total 10 points}
\item{CEPS_Food}{CEPS5 + CEPS6 + CEPS7 + CEPS8, total 40 points}
\item{CEPS_Water}{CEPS3 + CEPS11 + CEPS12, total 12 points}
\item{CEPS_Vending}{CEPS9, total 6 points}
\item{CEPS_PA}{CEPS2 + CEPS10 + CEPS13, total 18 points}
\item{CEPS_Stress}{CEPS4, total 6 points}
\item{CEPS_Total}{Sum of the above components, total 80 points}
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
