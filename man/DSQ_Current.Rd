\name{DSQ}
\alias{DSQ}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scoring of the 26-item Dietary Screener Questionnaire (DSQ) - Current Method
}
\description{
The function returns the computed diet variables for the Dietary Screener Questionnaire in NHANES 2009-10.
}
\usage{
Score_DSQ_Current(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
}
\format{
The data needs to name the survey variables as follows,

  \describe{
  \item{\code{Age}}{age of a unit}
  \item{\code{Gender}}{gender of a unit: 1=male; 2=female}
  }

During the past month, how often did you drink ...
  \describe{
    \item{\code{Dsqmilk1}}{...any milk (either to drink or on cereal)? Include regular milks, chocolate or other flavored milks, lactose-free milk, buttermilk. Please do not include soy milk or small amounts of milk in coffee or tea.
    
    (1)	Never (will skip the next question);
    (2)	1 time last month;
    (3)	2-3 times last month;
    (4)	1 time per week;
    (5)	2 times per week;
    (6)	3-4 times per week;
    (7)	5-6 times per week;
    (8)	1 time per day;
    (9)	2-3 times per day;
    (10) 4-5 times per day;
    (11) 6 or more times per day}
    
    \item{\code{Dsqmilk2}}{If answer above is not ``Never''... During the past month, what kind of milk did you usually drink?
    
    (1)	Whole or regular milk;
    (2)	2\% fat or reduced-fat milk;
    (3)	1\%, 0.5\%, or low-fat milk;
    (4)	Fat-free, skim, or nonfat milk;
    (5)	Soy milk;
    (6)	Other}
    
    \item{\code{Dsqmilk2_TEXT}}{If ``Other'' is chosen above... Specify  kind of milk _________}
    
    \item{\code{Dsqsoda}}{...regular soda or pop that contains sugar? Do not include diet soda. 
    
    The coded values are the same as Dsqmilk1}
    
    \item{\code{Dsqjuice}}{...100\% pure fruit juices such as orange, mango, apple, grape and pineapple juices? Do not include fruit-flavored drinks with added sugar or fruit juice you made at home and added sugar to. 
    
    The coded values are the same as Dsqmilk1}
    
    \item{\code{Dsqcoffee}}{...coffee or tea that had sugar or honey added to it? Include coffee and tea you sweetened yourself and presweetened tea and coffee drinks such as Arizona Iced Tea and Frappuccino. Do not include artificially sweetened coffee or diet tea.
    
    The coded values are the same as Dsqmilk1}
    
    \item{\code{Dsqdrink}}{...sweetened fruit drinks, sports or energy drinks, such as Kool-Aid, lemonade, Hi-C, cranberry drink, Gatorade, Red Bull, or Vitamin Water? Include fruit juices you made at home and added sugar to. Do not include diet drinks or artificially sweetened drinks.

    The coded values are the same as Dsqmilk1}
  }
  
During the past month, how often did you eat...
  \describe{
    \item{\code{Dsqfruit}}{...fruit? Include fresh, frozen, or canned fruit. Do not include juices. 
    
      (1)	Never; 
      (2)	1 time last month;
      (3)	2-3 times last month;
      (4)	1 time per week;
      (5)	2 times per week;
      (6)	3-4 times per week;
      (7)	5-6 times per week;
      (8)	1 time per day;
      (9)	2 or more times per day}
      
    \item{\code{Dsqsalad}}{...a green leafy or lettuce salad, with or without other vegetables?
    
    The coded values are the same as Dsqfruit}
    
    \item{\code{Dsqfried}}{...any kind of fried potatoes, including French fries, home fries, or hash brown potatoes?
    
    The coded values are the same as Dsqfruit}
    
    \item{\code{Dsqpotato}}{...any other kind of potatoes, such as baked, boiled, mashed potatoes, sweet potatoes, or potato salad?
    
    The coded values are the same as Dsqfruit}
    
    \item{\code{Dsqbean}}{...refried beans, baked beans, beans in soup, pork and beans or any other type of cooked dried beans? Do not include green beans.

    The coded values are the same as Dsqfruit}
    
    \item{\code{Dsqgrain}}{...brown rice or other cooked whole grains, such as bulgur, cracked wheat, or millet? Do not include white rice.
    
    The coded values are the same as Dsqfruit}
    
    \item{\code{Dsqveg}}{During the past month, not including what you just told me about (green salads, potatoes, cooked dried beans), how often did you eat other vegetables?
    
    The coded values are the same as Dsqfruit}
    
    \item{\code{Dsqsalsa}}{During the past month, how often did you have Mexican-type salsa made with tomato?
    
    The coded values are the same as Dsqfruit}
    
    \item{\code{Dsqpizza}}{...pizza? Include frozen pizza, fast food pizza, and homemade pizza.
    
    The coded values are the same as Dsqfruit}
    
    \item{\code{Dsqsauce}}{During the past month, how often did you have tomato sauces such as with spaghetti or noodles or mixed into foods such as lasagna? Do not include tomato sauce on pizza.
    
    The coded values are the same as Dsqfruit}
    
    \item{\code{Dsqcheese}}{...any kind of cheese? Include cheese as a snack, cheese on burgers, sandwiches, and cheese in foods such as lasagna, quesadillas, or casseroles. Do not count cheese on pizza.
    
    The coded values are the same as Dsqfruit}
    
    \item{\code{Dsqmeat}}{...red meat, such as beef, pork, ham, or sausage? Do not include chicken, turkey or seafood.
    
    The coded values are the same as Dsqfruit}
    
    \item{\code{Dsqproc}}{...any processed meat, such as bacon, lunch meats, or hot dogs? Include processed meats you had in sandwiches, soups, pizza, casseroles, and other mixtures.
    
    The coded values are the same as Dsqfruit}
    
    \item{\code{Dsqbread}}{...whole grain bread including toast, rolls and in sandwiches? Whole grain breads include whole wheat, rye, oatmeal and pumpernickel. Do not include white bread.
    
    The coded values are the same as Dsqfruit}
    
    \item{\code{Dsqcandy}}{...chocolate or any other types of candy? Do not include sugar-free candy.
    
    The coded values are the same as Dsqfruit}
    
    \item{\code{Dsqrolls}}{...doughnuts, sweet rolls, Danish, muffins, pan dulce, or pop-tarts? Do not include sugar-free items.
    
    The coded values are the same as Dsqfruit}
    
    \item{\code{Dsqcake}}{...cookies, cake, pie or brownies? Do not include sugar-free kinds.
    
    The coded values are the same as Dsqfruit}
    
    \item{\code{Dsqice}}{...ice cream or other frozen desserts? Do not include sugar-free kinds.
    
    The coded values are the same as Dsqfruit}
    
    \item{\code{Dsqcorn}}{...popcorn?
    
    The coded values are the same as Dsqfruit}
    
    \item{\code{Dsqcereal}}{...hot or cold cereals?
    
    The coded values are the same as Dsqfruit}
    
    \item{\code{Dsqcertyp1}}{If answer above is not ``Never''... During the past month, what kind of cereal did you usually eat?

    Drop down menu with 350 cereal options.}
    
    \item{\code{Dsqcertyp2}}{Was there another cereal you usually ate?
    
    Drop down menu with 350 cereal options.}
  
  }
}
\details{
}
\value{
\item{DSQfvl}{Predicted intake of fruits and vegetables including legumes and French fries (cup equivalents) per day}
\item{DSQfvlnf}{Predicted intake of fruits and vegetables including legumes and excluding French fries (cup equivalents) per day}
\item{DSQfrt}{Predicted intake of fruits (cup equivalents) per day}
\item{DSQvlall}{Predicted intake of vegetables including legumes and French fries (cup equivalents) per day}
\item{DSQvlnf}{Predicted intake of vegetables including legumes and excluding French fries (cup equivalents) per day}
\item{DSQdairy}{Predicted intake of dairy (cup equivalents) per day}
\item{DSQsug}{Predicted intake of total added sugars (tsp equivalents) per day}
\item{DSQssb}{Predicted intake of added sugars from sugar-sweetened beverages (tsp equivalents) per day}
\item{DSQwhgr}{Predicted intake of whole grains (ounce equivalents) per day}
\item{DSQfib}{Predicted intake of fiber (gm) per day}
\item{DSQcalc}{Predicted intake of calcium (mg) per day}

\item{DSQfvl_low}{Predicted probability of eating less than 1.7 cup equivalents of fruits and vegetables including legumes and French fries}
\item{DSQfvlnf_low}{Predicted probability of eating less than 1.7 cup equivalents fruits and vegetables including legumes and excluding French fries}
\item{DSQfrt_low}{Predicted probability of eating less than 0.5 cup equivalents of fruits}
\item{DSQvlall_low}{Predicted probability of eating less than 1.0 cup equivalents of vegetables including legumes and French fries}
\item{DSQvlnf_low}{Predicted probability of eating less than 1.0 cup equivalents vegetables including legumes and excluding French fries}
\item{DSQdairy_low}{Predicted probability of eating less than 1.2 cup equivalents of dairy}
\item{DSQsug_low}{Predicted probability of eating less than 11 tsp equivalents of total added sugars}
\item{DSQssb_low}{Predicted probability of eating less than 3 tsp equivalents added sugars from sugar-sweetened beverages}
\item{DSQwhgr_low}{Predicted probability of eating less than 0.3 ounce equivalents of whole grains}
\item{DSQfib_low}{Predicted probability of eating less than 12 grams of fiber}
\item{DSQcalc_low}{Predicted probability of eating less than 800 milligrams of calcium}

\item{DSQfvl_high}{Predicted probability of eating equal or more than 3.2 cup equivalents of fruits and vegetables including legumes and French fries}
\item{DSQfvlnf_high}{Predicted probability of eating equal or more than 3.2 cup equivalents fruits and vegetables including legumes and excluding French fries}
\item{DSQfrt_high}{Predicted probability of eating equal or more than 1.4 cup equivalents of fruits}
\item{DSQvlall_high}{Predicted probability of eating equal or more than 1.8 cup equivalents of vegetables including legumes and French fries}
\item{DSQvlnf_high}{Predicted probability of eating equal or more than 1.8 cup equivalents vegetables including legumes and excluding French fries}
\item{DSQdairy_high}{Predicted probability of eating equal or more than 2.4 cup equivalents of dairy}
\item{DSQsug_high}{Predicted probability of eating equal or more than 23 tsp equivalents of total added sugars}
\item{DSQssb_high}{Predicted probability of eating equal or more than 11 tsp equivalents added sugars from sugar-sweetened beverages}
\item{DSQwhgr_high}{Predicted probability of eating equal or more than 1.0 ounce equivalents of whole grains}
\item{DSQfib_high}{Predicted probability of eating equal or more than 19 grams of fiber}
\item{DSQcalc_high}{Predicted probability of eating equal or more than 1100 milligrams of calcium}
}
\references{
https://epi.grants.cancer.gov/nhanes/dietscreen/scoring/current/
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
