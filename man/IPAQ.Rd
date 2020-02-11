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

Categorical Score

Three levels (categories) of physical activity are proposed:

Category 1: Low

This is the lowest level of physical activity. Those individuals who not meet criteria for categories 2 or 3 are considered low/inactive.

Category 2: Moderate

Any one of the following 3 criteria:

1) 3 or more days of vigorous activity of at least 20 minutes per day OR

2) 5 or more days of moderate-intensity activity or walking of at least 30 minutes
per day OR

3) 5 or more days of any combination of walking, moderate-intensity or vigorous
intensity activities achieving a minimum of at least 600 MET-min/week.

Category 3: High

Any one of the following 2 criteria:

1) Vigorous-intensity activity on at least 3 days and accumulating at least 1500 MET-minutes/
week OR

2) 7 or more days of any combination of walking, moderate-intensity or vigorous
intensity activities achieving a minimum of at least 3000 MET-minutes/week 
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
library(Fruved)
data("STRESS_IPAQ_BMI")
Score_IPAQ(STRESS_IPAQ_BMI)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
