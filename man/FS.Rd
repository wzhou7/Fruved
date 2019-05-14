\name{Food Security}
\alias{Food Security}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scoring of the ``Food Security'' survey
}
\description{
1. Coded the following variables as instructions provided (FS2, FS3, FS16, FS4, FS17, FS5, FS6, FS7, FS8, FS9). Responses of “yes,” “often,” “sometimes,” “almost every month,” and “some months but not every month” are coded as affirmative (1). And others are coded as 0.

2. Created the following variables as scores in the Qualtrics scoring: 
Created FS_Step1 variable: FS2 + FS3 + FS16;
Created FS_Step2 variable: FS4 + FS17 + FS5 + FS6 + FS7;
Created FS_Step3 variable: FS8 + FS9.

3. In survey flow, embedded all three variables with the sum of the three scores. 

4. Created the following statements in survey flow, per the instructions. 

Total Test Score: count affirmative items. If there is no affirmative item in Step1, then no need to go through Step 2 and Step 3. If there is/are affirmative item(s) in Step 1 but no affirmative item in Step 2, then no need to go through Step 3. If there are affirmative items in both Step 1 and Step 2, then sum all the items in three steps.

If FS_Step1 is = 0 OR FS_Step2 = 0, then FoodSecurity_SCORE = FS_Step1

If FS_Step1 is ≠ 0 OR FS_Step2 ≠ 0, then FoodSecurity_SCORE = FS_Step1 + FS_Step2 + FS_Step3

}
\usage{
Score_FS(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
}
\format{
  \describe{
    
    \item{\code{FS2}}{"I worried whether my food would run out before I got money to buy more." Was that often true, sometimes true, or never true for you in the last 12 months? -
    
    1)	Often true;
2)	Sometimes true; 
3)	Never true;
4)  Choose not to answer}
    
    \item{\code{FS3}}{"The food that I bought just didn't last, and (I/We) didn't have the money to get more." Was that often true, sometimes true, or never true for you in the last 12 months?

1)	Often true;
2)	Sometimes true; 
3)	Never true;
4)  Choose not to answer}
    
    \item{\code{FS16}}{“I couldn’t afford to eat balanced/healthy meals.” Was that often, sometimes, or never true for you in the last 12 months? 
    
    1)	Often true;
2)	Sometimes true; 
3)	Never true;
4)  Choose not to answer
    }
    
    \item{\code{FS4}}{cut the size of your meals or skip meals because there wasn't enough money for food? 
    
    1=Yes;
7=No;
3= Choose not to answer}
    
    \item{\code{FS17}}{Display If FS4, “Yes” is selected

You answered yes to “Did you ever cut the size of your meals or skip meals because there wasn’t enough money for food?”
How often did this happen –almost every month, some months bit not every month, or in only 1 or 2 months?

1=Almost every month;
2=Some months but not every month;
3=Only 1 or 2 months;
4=Choose not to answer}
    
    \item{\code{FS5}}{eat less than you felt you should because there wasn't enough money for food?
    
    2=Yes;
3=No;
4= Choose not to answer
}
    
    \item{\code{FS6}}{hungry but didn't eat because there wasn't enough money for food?
    
    1=Yes;
2=No;
3= Choose not to answer}
    
    \item{\code{FS7}}{lose weight because there wasn't enough money for food?
    
    1=Yes;
2=No;
3= Choose not to answer}
    
    \item{\code{FS8}}{not eat for a whole day because there wasn't enough money for food?
    
    1=Yes;
2=No;
3= Choose not to answer}
    
    \item{\code{FS9}}{(DISPLAYED IF ANSWER TO FS8 WAS YES)
You answered yes to, “In the last 12 months, did you ever not eat for a whole day because there wasn’t enough money for food?”
How often did this happen – almost every month, some months but not every month, or in only 1 or 2 months? 

1)	Almost every month;
2)	Some months but not every month;
3)	Only 1 or 2 months;
4)	Choose not to answer
}
  }
}
\details{

}
\value{
%  ~Describe the value returned
%  If it is a LIST, use
\item{FS_SCORE}{Total of three subtotals}
%  \item{comp2 }{Description of 'comp2'}
}
\references{
https://www.ers.usda.gov/media/8279/ad2012.pdf
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
