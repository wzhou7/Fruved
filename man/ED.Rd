\name{Eeating Disorder}
\alias{Eeating Disorder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scoring of the ``Eeating Disorder'' survey
}
\description{
For question 1-25, Always=3, Usually=2, Often=1, Sometimes/Rarely/Never=0;

For question 26, Always/Usually/Often-0, Sometimes=1, Rarely=2, Never=3;

The total is the ED_SCORE.
}
\usage{
Score_ED(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
}
\format{
  Please check a response for each of the following statements:
  
  1)	Always;
  2)	Usually;
  3)	Often;
  4)	Sometimes;
  5)	Rarely;
  6)	Never;
  7)  Choose not to answer
  
  \describe{
    \item{\code{ED_9}}{I am terrified about being overweight}
    \item{\code{ED_10}}{I avoid eating when I am hungry}
    \item{\code{ED_11}}{I find myself preoccupied with food}
    \item{\code{ED_12}}{I have gone on eating binges where I feel that I may not be able to stop}
    \item{\code{ED_13}}{I cut my food into small pieces}
    \item{\code{ED_14}}{I am aware of the calorie content of foods that I eat}
    \item{\code{ED_15}}{I particularly avoid food with a high carbohydrate content (i.e. bread, rice, potatoes, etc.)}
    \item{\code{ED_16}}{I feel that others would prefer if I ate more}
    \item{\code{ED_17}}{I vomit after I have eaten}
    \item{\code{ED_18}}{I feel extremely guilty after eating}
    \item{\code{ED_19}}{I am preoccupied with a desire to be thinner}
    \item{\code{ED_20}}{I think about burning up calories when I exercise}
    \item{\code{ED_21}}{Other people think that I am too thin}
    \item{\code{ED_22}}{I am preoccupied with the thought of having fat on my body}
    \item{\code{ED_23}}{I take longer than others to eat my meals}
    \item{\code{ED_24}}{I avoid foods with sugar in them}
    \item{\code{ED_25}}{I eat diet foods}
    \item{\code{ED_26}}{I feel that food controls my life}
    \item{\code{ED_27}}{I display self-control around food}
    \item{\code{ED_28}}{I feel that others pressure me to eat}
    \item{\code{ED_29}}{I give too much time and thought to food}
    \item{\code{ED_30}}{I feel uncomfortable after eating sweets}
    \item{\code{ED_31}}{I engage in dieting behavior}
    \item{\code{ED_32}}{I like my stomach to be empty}
    \item{\code{ED_33}}{I have the impulse to vomit after meals}
    \item{\code{ED_34}}{I enjoy trying new rich foods}
  }
}
\details{

}
\value{
%  ~Describe the value returned
%  If it is a LIST, use
\item{ED_SCORE}{Total of 26 components}
%  \item{comp2 }{Description of 'comp2'}
}
\references{
http://www.nyeatingdisorders.org/pdf/EAT-26IntpretScoring-Test-3-20-10.pdf
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
%data("STRESS_IPAQ_BMI")
%Score_Stress(STRESS_IPAQ_BMI)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
