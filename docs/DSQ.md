# DSQ

Official information about the Dietary Screener Questionnaires (DSQ) can be found on the [NIH NCI website](https://epi.grants.cancer.gov/nhanes/dietscreen/).

We implemented the [Earlier Methods](https://epi.grants.cancer.gov/nhanes/dietscreen/scoring/earlier/) as they were the version available when we started the GetFruved project. The corresponding SAS programs are available on the [NIH NCI website](https://epi.grants.cancer.gov/nhanes/dietscreen/programs.html): see **Self-Administered Questionnaire: Paper**.

When scoring, the code requires coefficients depending on subjects such as age group. 

## Usage

The basic usage is 

```
df_out <- DSQ_Score(df_in)
```

where `df_in` is the input data frame and `df_out` is the output data frame.

### Input Data Requirement

The input data frame requires the following variables:



## To-Do

* Read the CSV files in the folder "nhanes.dietvars.excel.0212-2013" before running the scoring code
* The "gender" variable coding: 1 = male; 2 = female
* The "AGE" variable should be an integer measured in year
