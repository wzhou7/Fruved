\name{Personality}
\alias{Personality}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scoring of the ``Personality'' survey
}
\description{
1. Reverse scored P2, P4, P6, P8, P10 in Qualtrics scoring. 

2. Created variables for each scale in survey flow.

P_extraverison: average of P1 and Reverse P6; 
P_agreeableness: average of reverse P2 and P7; 
P_conscientiousness: average of P3 and reverse P8; 
P_emotionalstability: average of reverse P4 and P9; 
P_openness: average of P5 and reverse P10.
}
\usage{
Score_Personality(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
}
\format{
  Here are a number of personality traits that may or may not apply to you. Please select an option to indicate the extent to which you agree or disagree with that statement. You should rate the extent to which the pair of traits applies to you, even if one characteristic applies more strongly than the other.

1) Strongly disagree; 
2) Disagree moderately; 
3) Disagree a little
4) Neither agree or disagree
5) Agree a little
6) Agree moderately
7) Agree strongly
0) Choose not to answer
  \describe{
    \item{\code{P_1}}{Extraverted, enthusiastic.}
    \item{\code{P_2}}{Critical, quarrelsome.}
    \item{\code{P_3}}{Dependable, self-disciplined}
    \item{\code{P_4}}{Anxious, easily upset}
    \item{\code{P_5}}{Open to new experiences, complex}
    \item{\code{P_6}}{Reserved, quiet}
    \item{\code{P_7}}{Sympathetic, warm}
    \item{\code{P_8}}{Disorganized, careless}
    \item{\code{P_9}}{Calm, emotionally stable}
    \item{\code{P_10}}{Conventional, uncreative}
  }
}
\details{

}
\value{
%  ~Describe the value returned
%  If it is a LIST, use
\item{P_extraversion}{average of P1 and Reverse P6}
\item{P_agreeableness}{average of reverse P2 and P7}
\item{P_conscientiousness}{average of P3 and reverse P8}
\item{P_emotionalstability}{average of reverse P4 and P9}
\item{P_openess}{average of P5 and reverse P10}
}
\references{
https://gosling.psy.utexas.edu/scales-weve-developed/ten-item-personality-measure-tipi/
}
\author{
Wangcheng Yan, Wenjun Zhou
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
library(Fruved)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
