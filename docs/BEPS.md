# BECS (40 Items)

Behavior, Environment, and Changeability Survey (BECS)

Citation:

> Walsh J, Hebert A, Byrd-Bredbenner C, Carey G, Colby S, Brown-Esters O, Greene G, Hoerr S, Horacek T, Kattelmann K, Kidd T, Koenings M, Phillips B, Shelnutt K, White A. **The development and preliminary validation of the behavior, environment, and changeability survey (BECS)**. *J. Nutr. Educ. Behav.* 2012: 44 (6); 490-499.

# Input & Output

## Survey Questions and Encoding

This survey is about your lifestyle, health, and current surroundings.	

What are HEALTHY foods? A selection of healthy foods are depicted below. There are plenty of ways to eat healthy foods throughout the day by including a variety of nutrient-rich foods. When considering the amount of food eaten, a cup is about the size of a baseball.   Over the past 24 hours, from which food groups have you eaten?
(select all that apply)	

Each is coded as Y/N

* `BECS1_1` Grains
* `BECS1_2` Vegetables
* `BECS1_3` Fruit
* `BECS1_4` Milk, Yogurt, and Cheese
* `BECS1_5` Meat and Beans
* `BECS1_6` I have not eaten in 24 hours
* `BECS1_7` Choose not to answer

The following use the same:
* Never (1)
* Rarely (2)
* Sometimes (3)
* Often (4)
* Always (5)
* Choose not to answer (0)

My current behavior is that
* `BECS2_1`	...-I eat healthy food.`	
* `BECS2_2`	...-I eat healthy foods at my dining hall/apartment/home`	Same as previous.
* `BECS2_3`	...-I eat healthy foods at local restaurants.`	Same as previous.
* `BECS2_4`	...-I eat healthy foods from local food/grocery stores.`	Same as previous.
* `BECS2_5`	...-I eat healthy foods on campus or at work.`	Same as previous.
* `BECS2_6`	...-I exercise in an area(s) or place(s) that is easily accessible.`	Same as previous.
* `BECS2_7`	...-I exercise at least 30-60 minutes most days of the week`	Same as previous.
* `BECS2_8`	...-I participate in an exercise program(s) or class(es).`	Same as previous.
* `BECS2_9`	...-I prepare healthy meals`	Same as previous.
* `BECS2_10`	...-I eat 2-3 cups or more of vegetables daily`	Same as previous.
* `BECS2_11`	...-I eat 1-2 cups or more of fruits daily`	Same as previous.
* `BECS2_12`	...-I eat whole grain foods.`	Same as previous.
* `BECS2_13`	...-I eat healthy snacks.`	Same as previous.
* `BECS2_14`	...-I get 7-9 hours of sleep at night.`	Same as previous.
	

It is important to me to have
* `BECS3_1`	...-Exercise programs or classes available.
  - Not at all important (1)
  - Unimportant (2)
  - Neutral (3)
  - Important (4)
  - Very important (5)
  - Choose not to answer (6)
* `BECS3_2`	...-Health education programs or classes available.`	Same as previous.

I would like to see
* `BECS4_1`	...-More healthy foods available at my dining hall/apartment/home.
  - Strongly disagree (1)
  - Disagree (2)
  - Neutral (3)
  - Agree (4)
  - Strongly Agree (5)
  - Choose not to answer (6)
* `BECS4_2`	...-More healthy foods available at local restaurants.`	Same as previous.
* `BECS4_3`	...-More healthy foods available in local food/grocery stores.`	Same as previous.
* `BECS4_4`	...-More healthy foods available on campus or at work.`	Same as previous.
* `BECS4_5`	...-More exercise areas or places that are easily accessible.`	Same as previous.
* `BECS4_6`	...-More exercise programs or classes available.`	Same as previous.
* `BECS4_7`	...-My eating environment become more pleasant (e.g. cleaner, more comfortable surroundings).`	Same as previous.
* `BECS4_8`	...-My local area become easier and safer for walking (e.g. maintained surfaces and secure areas).`	Same as previous.
* `BECS4_9`	...-My local area become easier and safer for biking (e.g. paths, lanes, and limited obstacles).`	Same as previous.

* `BECS5`	It is important to me to get 7-9 hours of sleep at night
  - Not at all important (1)
  - Unimportant (2)
  - Neutral (3)
  - Important (4)
  - Very Important (5)
  - Choose not to answer (6)

I am willing to make changes so that I can
* `BECS6_1`	...-Eat healthy foods more often.
  - Strongly disagree (1)
  - Disagree (2)
  - Neutral (3)
  - Agree (4)
  - Strongly Agree (5)
  - Choose not to answer (6)
* `BECS6_2`	...-Enjoy eating a variety of healthy foods.`	Same as previous.
* `BECS6_3`	...-Prepare healthy meals more often.`	Same as previous.
* `BECS6_4`	...-Eat slower to determine whether I am hungry or full.`	Same as previous.
* `BECS6_5`	...-Eat vegetables more often.`	Same as previous.
* `BECS6_6`	...-Eat fruit more often.`	Same as previous.
* `BECS6_7`	...-Eat whole grain foods more often.`	Same as previous.
* `BECS6_8`	...-Eat healthy snacks more often.`	Same as previous.

%Variable Name `	Question Text`	Label Values
%* `BECS7`	Have you ever tried or wanted to lose weight?
%
%If Yes Is Selected, Then display the following three questions.`	Yes (1)
%No (2)
%Choose not to answer
%* `BECS8_1`	I am currently trying to lose weight`	Strongly disagree (1)
%Disagree (2)
%Neutral (3)
%Agree (4)
%Strongly Agree (5)
%Choose not to answer (6)
%* `BECS8_2`	It is IMPORTANT to me that when trying to lose weight I do not use fad or strict dieting.`	Same as previous.
%* `BECS8_3`	When losing weight, I am WILLING to lose without fad or strict dieting.`	Same as previous.
%* `BECS9`	Do you rarely or never drink alcohol?
%
%If Yes Is Selected, Then Skip To End of Block`	Yes (1)
%No (2)
%Choose not to answer
%* `BECS10_1`	I limit alcohol to avoid gaining weight.`	Strongly disagree (1)
%Disagree (2)
%Neutral (3)
%Agree (4)
%Strongly Agree (5)
%Choose not to answer (6)
%* `BECS10_2`	It is IMPORTANT to me that I limit alcohol to avoid gaining weight.`	Same as previous.
%* `BECS10_3`	I am WILLING to limit my alcohol intake to avoid gaining weight.	Same as previous.
%
%
%
%
%\subsection{Scoring Instructions}
%
%Walsh J, Hebert A, Byrd-Bredbenner C, Carey G, Colby S, Brown-Esters O, Greene G, Hoerr S, Horacek T, Kattelmann K, Kidd T, Koenings M, Phillips B, Shelnutt K, White A. The development and preliminary validation of the behavior, environment, and changeability survey (BECS). J. Nutr. Educ. Behav. 2012: 44 (6); 490-499.
%
%%Table 2: Behavior, Environment, and Changeability Survey (BECS) Component Structure
%%BECS Scale	Dataset 1 (n=565)	Dataset 2 (n=561)	P valuea
%%	m±sd	α	m±sd	α	
%%Nutrition Changeabilityb	4.1±.62	.93	4.1±.63	.93	.104
%%Nutrition Behaviorc	3.4±.65	.90	3.4±.63	.89	.218
%%Environment Changeabilityb	4.0±.65	.91	4.0±.64	.90	.490
%%Program Importance and Changeabilityd	3.5±.93	.84	3.5±.90	.82	.830
%%Exercise Behaviore	3.1±1.0	.82	3.0±1.0	.83	.307
%%Sleep Behavior and Importancef	3.8±.83	.66	3.8±.83	.67	.228
%%BECS = Behavior, Environment, Changeability Survey
%%α  = Cronbach’s Internal Consistency alpha.
%%a Determined by Independent t-tests.
%%b Range 1=Strongly Disagree – 5=Strongly Agree
%%c Range 1=Never – 5=Always
%%d Range 1=Very Unimportant – 5=Very Important and 1=Strongly Disagree – 5=Strongly Agree
%%eRange 1= Never – 5=Always
%%fRange 1=Never – 5=Always and 1=Very Unimportant – 5=Very Important
%
%Final Behavior, Environment, Changeability Survey (BECS) Instrument and Scoring
%
%My current behavior is that… (Response options: Never=1, Rarely=2, Sometimes=3, Often=4, Always=5)
%1.	I eat healthy foods at my dining hall/apartment/home
%2. 	I eat healthy foods at local restaurants.
%3.	I eat healthy foods from local food/grocery stores
%4.	I eat healthy foods on campus or at work
%5.	I exercise in an area or place that is easily accessible
%6.	I exercise at least 30-60 minutes most days of the week
%7. 	I participate in an exercise program or class.
%8.	I eat healthy foods
%9.	I prepare healthy meals
%10.	I eat 2-3 cups or more of vegetables daily
%11.	I eat 1-2 cups or more of fruits daily
%12. 	I eat whole grain foods
%13.	I eat healthy snacks
%14. 	I get 7-9 hours of sleep at night.
%
%It’s important for me to have … (Response options: Not at all important=1, Unimportant=2, Neutral=3l, Important=4, Very Important=5)
%15.	exercise program or classes available
%16.	health education programs or classes available
%
%
%It is important to me to get… (Response options: Not at all important=1, Unimportant=2, Neutral=3l, Important=4, Very Important=5)
%17. 7-9 hours of sleep at night.
%
%I would like to see… (Response options: Strongly Disagree=1, Disagree=2, Neutral=3, Agree=4, Strongly Agree=5)
%
%18. 	more healthy foods available at my dining hall/apartment/home
%19.	more healthy foods available at restaurants
%20. 	more healthy foods available in local food/grocery stores
%21.	more healthy foods available on campus or at work
%22.	more exercise areas or places that are easily accessible
%23.	more exercise programs or classes available
%24.	my eating environment become more pleasant (e.g. cleaner, more comfortable surroundings)
%25.	my local area become easier and safer for walking (e.g. maintained surfaces and secure areas)
%26.	my local area become easier and safer for biking (e.g. paths, lanes and limited obstacles)
%
%
%I am willing to make changes so that I can… (Response options: Strongly Disagree=1, Disagree=2, Neutral=3, Agree=4, Strongly Agree=5)
%
%27.	eat healthy foods more often
%28.	enjoy eating a variety of healthy foods
%29.	prepare healthy meals more often
%30.	eat slower to determine whether I am hungry or full
%31.	eat vegetables more often
%32.	eat fruits more often
%33.	eat whole grain foods more often
%34.	eat healthy snacks more often
%
%
%Skip Questions 38-40 if you have never tried or wanted to lose weight. (Response options: Strongly Disagree=1, Disagree=2, Neutral=3, Agree=4, Strongly Agree=5)
%
%35.	I am currently trying to lose weight.
%36. 	It is important to me that when trying to lose weight I do not use fad or strict dieting.
%37.	When losing weight, I am willing to lose without fad or strict dieting.
%
%Skip Questions 41-43 if you rarely or never drink alcohol. (Response options: Strongly Disagree=1, Disagree=2, Neutral=3, Agree=4, Strongly Agree=5)
%
%38. 	I limit alcohol to avoid gaining weight.	
%39.	It is important to me that I limit alcohol to avoid gaining weight.
%40.	I am willing to limit my alcohol intake to avoid gaining weight.


# Output

Each of the BECS scales are scored by calculating the average of the summed item raw scores.

Nutrition Changeability = (Item 27 + Item 28 + Item 29 + Item 30 + Item 31 + Item 32 + Item 33 + Item 34) / 8

Nutrition Behavior = (Item 1 + Item 2 + Item 3 + Item 4 + Item 8 + Item 9 + Item 10 + Item 11 + Item 12 + Item 13) / 10
 
Environmental Changeability = (Item 18 + Item 19 + Item 20 + Item 21 + Item 22 + Item 24 + Item 25 + Item 26) / 8

Program Importance and Changeability = (Item 15 + Item 16 + Item 23) / 3

Exercise Behavior= (Item 5 + Item 6 + Item 7) / 3

Sleep Behavior and Importance = (Item 14 + Item 17) / 2

Weight Loss = (Item 35 + Item 36 + Item 37) / 3

Alcohol Intake = (Item 38 + Item 39 + Item 40) / 3
