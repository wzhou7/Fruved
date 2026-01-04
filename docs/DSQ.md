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

Before running the scoring code, please ensure the data formatting (variable naming and value encoding) follows our [data encoding](DSQ_Variables.md) expectations.
Once your data (a data frame named `data`) is cleaned and formatted, you can run this:

```
data_scored <- DSQ_Scores(data)
```

Explanation of the DSQ output can be found at the [NCI website](https://epi.grants.cancer.gov/nhanes/dietscreen/scoring/current/variables.html).
