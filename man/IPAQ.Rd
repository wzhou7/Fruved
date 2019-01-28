\name{IPAQ}
\alias{IPAQ}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Scoring of the International Physical Activity Questionaire
}
\description{
The function returns the stress score given the data. 

MET values and Formula for computation of Met-minutes

Walking MET-minutes/week = 3.3 * walking minutes * walking days

Moderate MET-minutes/week = 4.0 * moderate-intensity activity minutes * moderate days

Vigorous MET-minutes/week = 8.0 * vigorous-intensity activity minutes * vigorous-intensity days
}
\usage{
Score_IPAQ(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
}
\format{
  The data needs to name the survey variables as follows (the ordering does not matter): 
  \describe{
    \item{\code{IPAQ1}}{During the last 7 days, on how many days did you do vigorous physical activities like heavy lifting, digging, aerobics, or fast bicycling?}
    \item{\code{IPAQ2}}{How much time did you usually spend doing vigorous physical activities on one of those days?}
    \item{\code{IPAQ3}}{During the last 7 days, on how many days did you do moderate physical activities like carrying light loads, bicycling at a regular pace, or doubles tennis?  Do not include walking.}
    \item{\code{IPAQ4}}{How much time did you usually spend doing moderate physical activities on one of those days?}
    \item{\code{IPAQ5}}{During the last 7 days, on how many days did you walk for at least 10 minutes at a time?}
    \item{\code{IPAQ6}}{How much time did you usually spend walking on one of those days?}
  }
}
\details{

%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Wangcheng Yan, Wenjun Zhou
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
