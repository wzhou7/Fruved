\name{Priorities}
\alias{Priorities}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Ranking Priorities Survey Choices
}
\description{
The function returns top 5 important choices, i.e., health promotion policies, areas of focus or changes. 
If there are multiple choices ranked 5, then store one in the fifth rank and all other ties in the sixth rank (served as optional).
}
\usage{
Score_Priorities(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
}
\format{
The data needs to name the survey variables as follows: 

How important do you think the following health promotion policies, areas of focus or changes, would be for your University to adopt? 
  \describe{
    \item{\code{PRIORTIES_1}}{Have more healthy food options available in campus dining halls.}
    \item{\code{PRIORTIES_2}}{Have more healthy food options available in campus restaurants.}
    \item{\code{PRIORTIES_3}}{Have more healthy food options available in campus vending machines.}
    \item{\code{PRIORTIES_4}}{Have more healthy food options available in campus convenience stores.}
    \item{\code{PRIORTIES_5}}{Have more healthy food options available through catering on campus.}
    \item{\code{PRIORTIES_6}}{Implement a program to use local and sustainable foods in campus dining halls.}
    \item{\code{PRIORTIES_7}}{Have programs and resources available that promote healthier dietary behavior (e.g. smaller beverage glasses, access to Registered Dietitian Nutritionists (RDNs), cooking classes, etc.).}
    \item{\code{PRIORTIES_8}}{All vending machines on campus must have working student ID card readers.}
    \item{\code{PRIORTIES_9}}{Healthy food options must be available for purchase using meal plans 24-hours a day within a ½ mile radius of any point on campus.}
    \item{\code{PRIORTIES_10}}{Healthy food options available on campus must be of equal or lesser price to comparable less healthy food options.}
    \item{\code{PRIORTIES_11}}{All on-campus residents must have 24-hour access to kitchen equipment (e.g. stove, oven).}
    \item{\code{PRIORTIES_12}}{Meal plans must be available for use at farmer’s markets.}
    \item{\code{PRIORTIES_13}}{All unhealthy food advertising (posters, student newspapers, buses, etc.) should be banned on the school campus.}
    \item{\code{PRIORTIES_14}}{There must be enough physical education courses available so that 50\% of all students can register for physical education credits in any given semester.}
    \item{\code{PRIORTIES_15}}{There must be resources available for students with limited mobility to be physically active.}
    \item{\code{PRIORTIES_16}}{A wellness course must be required for first-year and transfer students.}
    \item{\code{PRIORTIES_17}}{Healthier food options must be clearly labeled in all campus dining halls.}
    \item{\code{PRIORTIES_18}}{Healthier food options must be clearly labeled in all campus vending machines.}
    \item{\code{PRIORTIES_19}}{Healthier food options must be clearly labeled in all campus restaurants.}
    \item{\code{PRIORTIES_20}}{The college/university must offer an incentive program to encourage students’ healthy behaviors.}
    \item{\code{PRIORTIES_21}}{Free counseling services and programs to promote stress management must be available throughout the year for all students.}
    \item{\code{PRIORTIES_22}}{Free water must be available in all campus dining venues, recreational and educational facilities, and residence halls.}
    \item{\code{PRIORTIES_23}}{Quality recreational facilities, programs, physical activity classes, and resources must be easily accessible to all students.}
    \item{\code{PRIORTIES_24}}{Quality recreational facilities, resources, and classes must be accessible to all students 24-hours a day.}
    \item{\code{PRIORTIES_25}}{Campus-wide wellness programs must be available for all individuals.}
    \item{\code{PRIORTIES_26}}{The campus must be safely accessible by walking and biking.}
    \item{\code{PRIORTIES_27}}{Biking on campus must be promoted through programs, policies, and resources.}
    \item{\code{PRIORTIES_28}}{A program/policy must be implemented which assists students who may not be able to afford food and provides food assistance options on campus.}
    \item{\code{PRIORTIES_29}}{Use of public or campus provided transportation must be implemented and/or promoted.}
    \item{\code{PRIORTIES_30}}{A program/policy that supports and accommodates breastfeeding for mothers on campus must be implemented.}
    \item{\code{PRIORTIES_31}}{Other priority that might have not been covered}
  }
  
All the above questions have the following answer options:
(1) Not important (2) (3) (4) (5) Very important (6) Not applicable (7) Choose not to answer
}
\details{
}
\value{
A list of 16 elements: 5 groups of choice text, choice number of answers, and choice score plus choice text ranked 5 with ties (connected using AND).
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
