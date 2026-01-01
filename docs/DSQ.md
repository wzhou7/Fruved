# What Is DSQ?

Dietary Screener Questionnaires (DSQ) is a 26-item survey instrument developed by NIH's [National Cancer Instritute](https://epi.grants.cancer.gov/nhanes/dietscreen/) (NCI). 

* Besides **gender** and **age**, it asks about the frequency of food group intake in the past month.
Then, using an algorithm developed by NCI, several food group intake amounts can be estimated.

* While [its official website]((https://epi.grants.cancer.gov/nhanes/dietscreen/)) provides [several versions of the questionaire](https://epi.grants.cancer.gov/nhanes/dietscreen/questionnaires.html), 
we focus on the [Self-Administered Questionnaire: Paper](https://epi.grants.cancer.gov/nhanes/dietscreen/questionnaires.html#paper) version.
We attempt to follow the questionaire's [codebook](https://epi.grants.cancer.gov/nhanes/dietscreen/dsq_codebook_teleform.docx) as much as possible.

* The official SAS programs are available on the [NIH NCI website](https://epi.grants.cancer.gov/nhanes/dietscreen/programs.html). Here we provide a translated R version.
Similar to the official scoring methods, we implement both an [earlier method (PDF)](https://epi.grants.cancer.gov/nhanes/dietscreen/scoring/earlier/dietary-screener-questionnaire-earlier-methods-scoring-procedure-04082020.pdf) and a [current method](https://epi.grants.cancer.gov/nhanes/dietscreen/scoring/current/).

## Usage

The basic usage is 

```
df_out <- DSQ_Score(df_in)
```

where `df_in` is the input data frame and `df_out` is the output data frame.

# How to Score Your Data in R?

## Step 1. Prepare the Data

In this step, we standardize the data formatting (variable naming and value encoding) to make it compatible with the official scoring code. 

First, each record should be assigned a unique identifier `SEQN`.

To score DSQ, two demographics variables are required:

* `DSQ_xx1` : Age in years. Must be an integer and the acceptable range is 1-99.
* `DSQ_xx2` : Are you male or female? `'A'` = Male; `'B'` = Female. Other values are not possible.

The following cereal variables are collected:
* `DSQ_010` : During the past month, how often did you eat hot or cold cereals? Choices are:
  - `'A'` = Never 
  - `'B'` = 1 time last month 
  - `'C'` = 2-3 times last month 
  - `'D'` = 1 time per week 
  - `'E'` = 2 times per week 
  - `'F'` = 3-4 times per week 
  - `'G'` = 5-6 times per week 
  - `'H'` = 1 time per day 
  - `'I'` = 2 or more times per day.

* `DSQ_020` : During the past month, what kind of cereal did you usually eat? Choices are food codes from a cereal database (see the appendix in the [codebook](https://epi.grants.cancer.gov/nhanes/dietscreen/dsq_codebook_teleform.docx)).

* `DSQ_xx3` : If there was another kind of cereal that you usually ate during the past month, what kind was it? Choices are the same as `DSQ_020`.

The following drinks are asked of their frequency during the past month. Choices are from `'A'` to `'K'`, as in `DSQ_030`.
* `DSQ_030` : have any milk (either to drink or on cereal)? Choices are:
  - `'A'` = Never 
  - `'B'` = 1 time last month 
  - `'C'` = 2-3 times last month 
  - `'D'` = 1 time per week 
  - `'E'` = 2 times per week 
  - `'F'` = 3-4 times per week 
  - `'G'` = 5-6 times per week 
  - `'H'` = 1 time per day 
  - `'I'` = 2-3 times per day 
  - `'J'` = 4-5 times per day 
  - `'K'` = 6 or more times per day
* `DSQ_040` : drink regular soda or pop that contains sugar? 
* `DSQ_050` : drink 100% pure fruit juices such as orange, mango, apple, grape and pineapple juices? 
* `DSQ_060` : drink coffee or tea that had sugar or honey added to it? 
* `DSQ_070` : drink sweetened fruit drinks, sports or energy drinks, such as Kool-Aid, lemonade, Hi-C, cranberry drink, Gatorade, Red Bull or Vitamin Water? 

The following foods are asked of their frequency during the past month. Choices are from `'A'` to `'I'`, the same as in `DSQ_010`.
* `DSQ_080` : eat fruit? 
* `DSQ_090` : eat green leafy or lettuce salad, with or without other vegetables? 
* `DSQ_100` : eat any kind of fried potatoes, including French fries, home fries, or hash brown potatoes? 
* `DSQ_110` : eat any other kind of potatoes, such as baked, boiled, mashed potatoes, sweet potatoes, or potato salad? 
* `DSQ_120` : eat refried beans, baked beans, beans in soup, pork and beans or any other type of cooked dried beans? 
* `DSQ_210` : eat brown rice or other cooked whole grains, such as bulgur, cracked wheat, or millet? Do not include white rice. 
* `DSQ_130` : not including what you just told me about (green salads, potatoes, cooked dried beans), how often did you eat other vegetables? 
* `DSQ_150` : have Mexican-type salsa made with tomato? 
* `DSQ_140` : eat pizza? Include frozen pizza, fast food pizza, and homemade pizza. 
* `DSQ_160` : have tomato sauces such as with spagetti or noodles or mixed into foods such as lasagna? 
* `DSQ_190` : eat any kind of cheese? Include cheese as a snack, cheese on burgers, sandwiches, and cheese in foods such as lasagna, quesadillas, or casseroles. 
* `DSQ_180` : eat any processed meat, such as bacon, lunch meats, or hot dogs? 
* `DSQ_200` : eat whole grain bread including toast, rolls and in sandwiches? 
* `DSQ_220` : eat chocolate or any other types of candy? 
* `DSQ_230` : eat doughnuts, sweet rolls, Danish, muffins, pan dulce, or pop-tarts? 
* `DSQ_240` : eat cookies, cake, pie or brownies? 
* `DSQ_250` : eat ice cream or other frozen desserts? 
* `DSQ_260` : eat popcorn? 

The following food intake variables are collected but not used for scoring:
* `DSQ_xx4` : what kind of milk did you usually drink
* `DSQ_xx4os` : what kind of milk did you usually drink? Other Specify
* `DSQ_170` : eat red meat, such as beef, pork, ham, or sausage? 

We provide the following utility function in R to rename your variable names. 
Please note that you still need to ensure that the [data encoding](DSQ_input.md) is correct for each variable.

```
data <- DSQ_Variables(data, UNIQUEID="ID",
                      DSQ_xx1="Age",
                      DSQ_xx2="Gender",
                      DSQ_010="Dsqcereal",
                      DSQ_020="Dsqcertyp1",
                      DSQ_xx3="Dsqcertyp2",
                      DSQ_030="Dsqmilk1",
                      DSQ_040="Dsqsoda",
                      DSQ_050="Dsqjuice",
                      DSQ_060="Dsqcoffee",
                      DSQ_070="Dsqdrink",
                      DSQ_080="Dsqfruit",
                      DSQ_090="Dsqsalad",
                      DSQ_100="Dsqfried",
                      DSQ_110="Dsqpotato",
                      DSQ_120="Dsqbean",
                      DSQ_130="Dsqveg",
                      DSQ_140="Dsqpizza",
                      DSQ_150="Dsqsalsa",
                      DSQ_160="Dsqsauce",
                      DSQ_190="Dsqcheese",
                      DSQ_180="Dsqproc",
                      DSQ_200="Dsqbread",
                      DSQ_210="Dsqgrain",
                      DSQ_220="Dsqcandy",
                      DSQ_230="Dsqrolls",
                      DSQ_240="Dsqcake",
                      DSQ_250="Dsqice",
                      DSQ_260="Dsqcorn")
```

Once you save the output data frame `data` in a standard data file (such as CSV), you can import it in SAS, and run the [official SAS scoring script](https://epi.grants.cancer.gov/nhanes/dietscreen/scoring/current/self.paper.zip).

## Step 2. Run the Scoring Code

The [scoring algorithms](https://epi.grants.cancer.gov/nhanes/dietscreen/scoring/) are well documented on its official website. 
There is an earlier method and a current method. The current method is recommended. 
Both methods have been implemented in [SAS](https://epi.grants.cancer.gov/nhanes/dietscreen/programs.html). 
This package implements the [current method](https://epi.grants.cancer.gov/nhanes/dietscreen/scoring/current/) in R. 

Once your data (encapsulated in data frame named `data`) is cleaned and formatted according to the official codebook, you can run this:

```
data_scored <- DSQ_Scores(data)
```

By default, the output data frame `data_scored` will only include the [computed variables](https://epi.grants.cancer.gov/nhanes/dietscreen/scoring/current/variables.html).


# Example

# Input and Output


## DSQ Output

See [NCI website](https://epi.grants.cancer.gov/nhanes/dietscreen/scoring/current/variables.html).
