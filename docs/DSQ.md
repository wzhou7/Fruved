# What Is DSQ?

Dietary Screener Questionnaires (DSQ) is a 26-item survey instrument developed by NIH's [National Cancer Instritute](https://epi.grants.cancer.gov/nhanes/dietscreen/) (NCI). 

* Besides **gender** and **age**, it asks about the frequency of food group intake in the past month.
Then, using an [algorithm developed by NCI](https://epi.grants.cancer.gov/nhanes/dietscreen/scoring/), several food group intake amounts can be estimated.

* While [its official website]((https://epi.grants.cancer.gov/nhanes/dietscreen/)) provides [several versions of the questionaire](https://epi.grants.cancer.gov/nhanes/dietscreen/questionnaires.html), 
we focus on the [Self-Administered Questionnaire: Paper](https://epi.grants.cancer.gov/nhanes/dietscreen/questionnaires.html#paper) version.
We attempt to follow the questionaire's [codebook](https://epi.grants.cancer.gov/nhanes/dietscreen/dsq_codebook_teleform.docx) as much as possible.
For minor discrepancies between the codebook and the official scoring code, we follow the requirement of the official scoring code. 

* The official SAS programs are available on the [NIH NCI website](https://epi.grants.cancer.gov/nhanes/dietscreen/programs.html). Here we provide a translated R version.
Similar to the official scoring methods, here we focus on implementing the [current method](https://epi.grants.cancer.gov/nhanes/dietscreen/scoring/current/).

# How to Score DSQ?

## Step 1. Prepare the Data

In this step, we standardize the data formatting (variable naming and value encoding) to make it compatible with the official scoring code. 
We provide the following utility function in R to rename your variable names. 
Please note that you still need to ensure that the [data encoding](DSQ_Variables.md) is correct for each variable.

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

Once you save the output data frame `data` in a standard data file (such as CSV), you can import it in SAS, and run the official SAS scoring script.

## Step 2. Run the Scoring Code in R

Once your data (a data frame named `data`) is cleaned and formatted, you can run this:

```
data_scored <- DSQ_Scores(data)
```

Explanation of the DSQ output can be found at the [NCI website](https://epi.grants.cancer.gov/nhanes/dietscreen/scoring/current/variables.html).
