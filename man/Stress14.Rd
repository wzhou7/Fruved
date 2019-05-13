\name{Stress14}
\alias{Stress14}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scoring of the ``Stress14'' survey
}
\description{
The function returns the stress score given the data. 

Scaling: 0 = Never; 1 = Almost Never; 2 = Sometimes; 3 = Fairly often; 4 = Very often

PSS-14 scores are obtained by reversing the scores on the seven positive items, e.g., 0=4, 1=3, 2=2, etc., and then summing across all 14 items.

Items STRESS_1, STRESS_6, STRESS_7, STRESS_8, STRESS_10, STRESS_11, and STRESS_14 are the positively stated items.
}
\usage{
Score_Stress(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
}
\format{
  The data needs to name the survey variables as follows (the ordering does not matter):
  \describe{
    \item{\code{STRESS_1}}{...been upset because of something that happened unexpectedly?}
    \item{\code{STRESS_2}}{...felt that you were unable to control the important things in your life?}
    \item{\code{STRESS_3}}{...felt nervous and stressed?}
    \item{\code{STRESS_4}}{...dealt successfully with irritating life hassles? }
    \item{\code{STRESS_5}}{...felt that you were effectively coping with important changes that were occurring in your life?}
    \item{\code{STRESS_6}}{...felt confident about your ability to handle your personal problems?}
    \item{\code{STRESS_7}}{...felt that things were going your way?}
    \item{\code{STRESS_8}}{...found that you could not cope with all the things that you had to do?}
    \item{\code{STRESS_9}}{...been able to control irritations in your life?}
    \item{\code{STRESS_10}}{...felt that you were on top of things?}
    \item{\code{STRESS_11}}{...been angered because of things that happen that were outside of your control?}
    \item{\code{STRESS_12}}{...found yourself thinking about things that you have to accomplish?}
    \item{\code{STRESS_13}}{...been able to control the way you spend your time?}
    \item{\code{STRESS_14}}{...felt difficulties were piling up so high that you could not overcome them?}
  }
}
\details{

}
\value{
%  ~Describe the value returned
%  If it is a LIST, use
%  \item{comp1 }{Description of 'comp1'}
%  \item{comp2 }{Description of 'comp2'}
}
\references{
Andreou, E., Alexopoulos, E. C., Lionis, C., Varvogli, L., Gnardellis, C., Chrousos, G. P., & Darviri, C. (2011). Perceived Stress Scale: Reliability and Validity Study in Greece. International Journal of Environmental Research and Public Health, 8(8), 3287-3298. Available at: http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3166743/
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
data("STRESS_IPAQ_BMI")
Score_Stress(STRESS_IPAQ_BMI)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
