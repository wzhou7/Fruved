\name{NCIFAT}
\alias{NCIFAT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scoring of the ``NCIFAT'' survey
}
\description{
The function returns the NCIFAT score given the data. 

1. Multiplied median serving sizes (g) by the frequency range for each gender. We used ages 18-27 because this will be most of the sample size. Entered these values as separate scores in Qualtrics scoring. 

2. Used the variables FAT_Fmargarine and FAT_Mmargarine to determine the proportion of the margarine and butter added to foods. In the survey flow, created statements:
If “Female” OR “Transmale/Transman” is Selected: Totfat = FAT_Fmargarine * NCIfat16; 
If “Male” OR “Transfemale/Transwoman” is Selected: Totfat = FAT_Mmargarine * NCIfat16.

3. Individual percentage energy from fat by gender: Created gender specific regression equation below in the survey flow using the coeffients in the table below to create estpcalfat. 

}
\usage{
Score_NCIFAT(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
}
\format{
  Think about your eating habits over the past 12 months.  About how often did you eat or drink each of the following foods?  Remember breakfast, lunch, dinner, snacks, and eating out.  Blacken in only one bubble for each food. 
  
  All the following questions have the choices:
  1)	Never;
  2)	Less than once per month;
  3)	1-3 times per month;
  4)	1-2 times per week;
  5)	3-4 times per week;
  6)	5-6 times per week;
  7)	1 time per day
2 or more times per day
  \describe{
    \item{\code{Ncifat1}}{...Cold cereal}
    \item{\code{Ncifat2}}{...Skim milk, on cereal or to drink}
    \item{\code{Ncifat3}}{...Eggs, fried or scrambled in margarine, butter, or oil}
    \item{\code{Ncifat4}}{...Sausage or bacon, regular-fat}
    \item{\code{Ncifat5}}{...Margarine or butter on bread, rolls, pancakes}
    \item{\code{Ncifat6}}{...Orange juice or grapefruit juice}
    \item{\code{Ncifat7}}{...Fruit (not juices)}
    \item{\code{Ncifat8}}{...Beef or pork hot dogs, regular-fat}
    \item{\code{Ncifat9}}{...Cheese or cheese spread, regular-fat}
    \item{\code{Ncifat10}}{...French fries, home fries, or hash brown potatoes}
    \item{\code{Ncifat11}}{...Margarine or butter on vegetables, including potatoes}
    \item{\code{Ncifat12}}{...Mayonnaise, regular-fat}
    \item{\code{Ncifat13}}{...Salad dressings, regular-fat}
    \item{\code{Ncifat14}}{...Rice}
    \item{\code{Ncifat15}}{...Margarine, butter, or oil on rice or pasta}
  }
  
  \describe{
    \item{\code{Ncifat16}}{Over the past 12 months, when you prepared foods with margarine or ate margarine, how often did you use a reduced-fat margarine?
    
    1) Didn’t use margarine
    2) Almost never
    3) About ¼ of the time 
    4) About ½ of the time 
    5) About ¾ of the time 
    6) Almost always or always}
    \item{\code{Ncifat17}}{Overall, when you think about the foods you ate over the past 12 months, would you say your diet was high, medium, or low in fat?
    
    1)	High;
    2)	Medium;
    3)  Low}
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
https://epi.grants.cancer.gov/diet/screeners/fat/scoring.html#scoring
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
