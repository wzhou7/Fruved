\name{BECS}
\alias{BECS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scoring of the ``BECS'' survey
}
\description{
My current behavior is that (Response options: Never=1, Rarely=2, Sometimes=3, Often=4, Always=5): 
1.	I eat healthy foods at my dining hall/apartment/home;
2. 	I eat healthy foods at local restaurants;
3.	I eat healthy foods from local food/grocery stores;
4.	I eat healthy foods on campus or at work;
5.	I exercise in an area or place that is easily accessible;
6.	I exercise at least 30-60 minutes most days of the week;
7. 	I participate in an exercise program or class;
8.	I eat healthy foods;
9.	I prepare healthy meals;
10.	I eat 2-3 cups or more of vegetables daily;
11.	I eat 1-2 cups or more of fruits daily;
12. 	I eat whole grain foods;
13.	I eat healthy snacks;
14. 	I get 7-9 hours of sleep at night.

It’s important for me to have (Response options: Not at all important=1, Unimportant=2, Neutral=3l, Important=4, Very Important=5):
15.	exercise program or classes available; 
16.	health education programs or classes available


It is important to me to get (Response options: Not at all important=1, Unimportant=2, Neutral=3l, Important=4, Very Important=5): 
17. 7-9 hours of sleep at night.

I would like to see (Response options: Strongly Disagree=1, Disagree=2, Neutral=3, Agree=4, Strongly Agree=5):
18. 	more healthy foods available at my dining hall/apartment/home;
19.	more healthy foods available at restaurants;
20. 	more healthy foods available in local food/grocery stores;
21.	more healthy foods available on campus or at work;
22.	more exercise areas or places that are easily accessible;
23.	more exercise programs or classes available;
24.	my eating environment become more pleasant (e.g. cleaner, more comfortable surroundings);
25.	my local area become easier and safer for walking (e.g. maintained surfaces and secure areas);
26.	my local area become easier and safer for biking (e.g. paths, lanes and limited obstacles).


I am willing to make changes so that I can (Response options: Strongly Disagree=1, Disagree=2, Neutral=3, Agree=4, Strongly Agree=5): 
27.	eat healthy foods more often;
28.	enjoy eating a variety of healthy foods;
29.	prepare healthy meals more often;
30.	eat slower to determine whether I am hungry or full;
31.	eat vegetables more often;
32.	eat fruits more often;
33.	eat whole grain foods more often;
34.	eat healthy snacks more often.


Skip Questions 38-40 if you have never tried or wanted to lose weight. (Response options: Strongly Disagree=1, Disagree=2, Neutral=3, Agree=4, Strongly Agree=5):
35.	I am currently trying to lose weight;
36. 	It is important to me that when trying to lose weight I do not use fad or strict dieting;
37.	When losing weight, I am willing to lose without fad or strict dieting.

Skip Questions 41-43 if you rarely or never drink alcohol. (Response options: Strongly Disagree=1, Disagree=2, Neutral=3, Agree=4, Strongly Agree=5):
38. 	I limit alcohol to avoid gaining weight;	
39.	It is important to me that I limit alcohol to avoid gaining weight;
40.	I am willing to limit my alcohol intake to avoid gaining weight.


Scoring Instructions

Each of the BECS scales are scored by calculating the average of the summed item raw scores. 

Nutrition Changeability = (Item 27 + Item 28 + Item 29 + Item 30 + Item 31 + Item 32 + Item 33 + Item 34) / 8

Nutrition Behavior = (Item 1 + Item 2 + Item 3 + Item 4 + Item 8 + Item 9 + Item 10 + Item 11 + Item 12 + Item 13) / 10

Environmental Changeability = (Item 18 + Item 19 + Item 20 + Item 21 + Item 22 + Item 24 + Item 25 + Item 26) / 8

Program Importance and Changeability = (Item 15 + Item 16 + Item 23) / 3

Exercise Behavior= (Item 5 + Item 6 + Item 7) / 3

Sleep Behavior and Importance = (Item 14 + Item 17) / 2

Weight Loss = (Item 35 + Item 36 + Item 37) / 3

Alcohol Intake = (Item 38 + Item 39 + Item 40) / 3

}
\usage{
Score_BECS(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
}
\format{
  My current behavior is that: 
  
  Never (1);
  Rarely (2);
  Sometimes (3);
  Often (4);
  Always (5);
  Choose not to answer (0).
  
  \describe{
    \item{\code{BECS2_1}}{...I eat healthy food}
    \item{\code{BECS2_2}}{...I eat healthy foods at my dining hall/apartment/home}
    \item{\code{BECS2_3}}{...I eat healthy foods at local restaurants}
    \item{\code{BECS2_4}}{...I eat healthy foods from local food/grocery stores}
    \item{\code{BECS2_5}}{...I eat healthy foods on campus or at work}
    \item{\code{BECS2_6}}{...I exercise in an area(s) or place(s) that is easily accessible}
    \item{\code{BECS2_7}}{...I exercise at least 30-60 minutes most days of the week}
    \item{\code{BECS2_8}}{...I participate in an exercise program(s) or class(es)}
    \item{\code{BECS2_9}}{...I prepare healthy meals}
    \item{\code{BECS2_10}}{...I eat 2-3 cups or more of vegetables daily}
    \item{\code{BECS2_11}}{...I eat 1-2 cups or more of fruits daily}
    \item{\code{BECS2_12}}{...I eat whole grain foods}
    \item{\code{BECS2_13}}{...I eat healthy snacks}
    \item{\code{BECS2_14}}{...I get 7-9 hours of sleep at night}
  }
  
  It is important to me to have: 
  
  Not at all important (1);
Unimportant (2);
Neutral (3);
Important (4);
Very important (5);
Choose not to answer (0)

  \describe{
  \item{\code{BECS3_1}}{Exercise programs or classes available}
  \item{\code{BECS3_2}}{Health education programs or classes available}
  }
  
  I would like to see: 
  
  Strongly disagree (1);
Disagree (2);
Neutral (3);
Agree (4);
Strongly Agree (5);
Choose not to answer (0)

  \describe{
  \item{\code{BECS4_1}}{More healthy foods available at my dining hall/apartment/home}
  \item{\code{BECS4_2}}{More healthy foods available at local restaurants}
  \item{\code{BECS4_3}}{More healthy foods available in local food/grocery stores}
  \item{\code{BECS4_4}}{More healthy foods available on campus or at work}
  \item{\code{BECS4_5}}{More exercise areas or places that are easily accessible}
  \item{\code{BECS4_6}}{More exercise programs or classes available}
  \item{\code{BECS4_7}}{My eating environment become more pleasant (e.g. cleaner, more comfortable surroundings)}
  \item{\code{BECS4_8}}{My local area become easier and safer for walking (e.g. maintained surfaces and secure areas)}
  \item{\code{BECS4_9}}{My local area become easier and safer for biking (e.g. paths, lanes, and limited obstacles)}
  }
  
  I am willing to make changes so that I can:
  
  Strongly disagree (1);
Disagree (2);
Neutral (3);
Agree (4);
Strongly Agree (5);
Choose not to answer (0)

  \describe{
  \item{\code{BECS6_1}}{Eat healthy foods more often}
  \item{\code{BECS6_2}}{Enjoy eating a variety of healthy foods}
  \item{\code{BECS6_3}}{Prepare healthy meals more often}
  \item{\code{BECS6_4}}{Eat slower to determine whether I am hungry or full}
  \item{\code{BECS6_5}}{Eat vegetables more often}
  \item{\code{BECS6_6}}{Eat fruit more often}
  \item{\code{BECS6_7}}{Eat whole grain foods more often}
  \item{\code{BECS6_8}}{Eat healthy snacks more often}
  }
  
  The following questions - Strongly disagree (1);
Disagree (2);
Neutral (3);
Agree (4);
Strongly Agree (5);
Choose not to answer (0)

  \describe{
  \item{\code{BECS8_1}}{I am currently trying to lose weight}
  \item{\code{BECS8_2}}{It is IMPORTANT to me that when trying to lose weight I do not use fad or strict dieting}
  \item{\code{BECS8_3}}{When losing weight, I am WILLING to lose without fad or strict dieting}
  }
  
  The following questions - Strongly disagree (1);
Disagree (2);
Neutral (3);
Agree (4);
Strongly Agree (5);
Choose not to answer (0)
  
  \describe{
  \item{\code{BECS10_1}}{I limit alcohol to avoid gaining weight}
  \item{\code{BECS10_2}}{It is IMPORTANT to me that I limit alcohol to avoid gaining weight}
  \item{\code{BECS10_3}}{I am WILLING to limit my alcohol intake to avoid gaining weight}
  }
}
\details{

}
\value{
%  ~Describe the value returned
%  If it is a LIST, use
\item{Nutrition_Changeability}{}
\item{Nutrition_Behavior}{}
\item{Environmental_Changeability}{}
\item{Program_Importance_and_Change_ability}{}
\item{Exercise_Behavior}{}
\item{Sleep_Behavior_and_Importance}{}
\item{Weight_Loss}{}
\item{Alcohol_Intake}{}
}
\references{
Walsh J, Hebert A, Byrd-Bredbenner C, Carey G, Colby S, Brown-Esters O, Greene G, Hoerr S, Horacek T, Kattelmann K, Kidd T, Koenings M, Phillips B, Shelnutt K, White A. The development and preliminary validation of the behavior, environment, and changeability survey (BECS). J. Nutr. Educ. Behav. 2012: 44 (6); 490-499.
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
