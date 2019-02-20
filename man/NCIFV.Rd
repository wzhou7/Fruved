\name{NCIFV}
\alias{NCIFV}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Scoring of NCI Fruit and Vegetables
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Score_FV(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
}
\format{
  The data needs to name the survey variables as follows (the ordering does not matter): Over the last month, how often did you
  \describe{
    \item{\code{NCIFV1}}{...drink 100\% juice such as orange, apple, grape, or grapefruit juice?}
    \item{\code{NCIFV2}}{Each time you drank 100\% juice, how much did you usually drink?}
    \item{\code{NCIFV3}}{...eat fruit? Count any kind of fruit - fresh, canned, and frozen.}
    \item{\code{NCIFV4}}{Each time you ate fruit, how much did you usually eat? }
    \item{\code{NCIFV5}}{...eat lettuce salad (with or without other vegetables)?}
    \item{\code{NCIFV6}}{Each time you ate lettuce salad, how much did you usually eat? }
    \item{\code{NCIFV7}}{...eat French fries or fried potatoes?}
    \item{\code{NCIFV8}}{Each time you ate French fries or fried potatoes, how much did you usually eat?}
    \item{\code{NCIFV9}}{...eat other white potatoes?}
    \item{\code{NCIFV10}}{Each time you ate these potatoes, how much did you usually eat?}
    \item{\code{NCIFV11}}{...eat cooked dried beans?}
    \item{\code{NCIFV12}}{Each time you ate these beans, how much did you usually eat?}
    \item{\code{NCIFV13}}{...eat other vegetables? }
    \item{\code{NCIFV14}}{Each of these times that you ate other vegetables, how much did you usually eat?}
    \item{\code{NCIFV15}}{...eat tomato sauce?}
    \item{\code{NCIFV16}}{Each time you ate tomato sauce, how much did you usually eat?}
    \item{\code{NCIFV17}}{...eat vegetable soups?}
    \item{\code{NCIFV18}}{Each time you ate vegetable soup, how much did you usually eat?}
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
https://epi.grants.cancer.gov/diet/screeners/fruitveg/scoring/allday.html
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
