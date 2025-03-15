# What is NCI FV?

NCI FV stands for the NCI's Fruit & Vegetable Intake Screener, a 19-item survey instrument developed by NIH's [National Cancer Instritute](https://epi.grants.cancer.gov/diet/screeners/fruitveg/) (NCI). While [its official website](https://epi.grants.cancer.gov/diet/screeners/fruitveg/) provides [several versions of the questionaire](https://epi.grants.cancer.gov/diet/screeners/fruitveg/instrument.html), 
we focus on the **All-Day Screener**. 
A copy of the original questionaire can be obtained from [here](https://epi.grants.cancer.gov/diet/shortreg/instruments/eats_all-day.pdf) (PDF).

# Data Preparation

We provide the following utility function in R to rename your variable names. 
Please note that you still need to ensure that the [data encoding](NCIFV_input.md) is correct for each variable.

```
data <- NCIFV_Variables(data, UNIQUEID="ID",
                            DSQ_xx1="ageinyr",
                            DSQ_xx2="gender",
                            DSQ_010="cereal",
                            DSQ_020="cereal_type1",
                            DSQ_xx3="cereal_type2",
                            DSQ_030="milk",
                            DSQ_040="soda",
                            DSQ_050="juice",
                            DSQ_060="coffee",
                            DSQ_070="energy_drink",
                            DSQ_080="fruits",
                            DSQ_090="green_veges",
                            DSQ_100="fried_potatoes",
                            DSQ_110="other_potatoes",
                            DSQ_120="beans",
                            DSQ_130="other_veges",
                            DSQ_140="pizza",
                            DSQ_150="salsa",
                            DSQ_160="tomato_sauce",
                            DSQ_190="cheese",
                            DSQ_180="proc_meat",
                            DSQ_200="whole_grain_bread",
                            DSQ_210="whole_grains",
                            DSQ_220="choc_candy",
                            DSQ_230="donut_muffin",
                            DSQ_240="cookie_cake",
                            DSQ_250="icecream",
                            DSQ_260="popcorn")
```

Once you save the output data frame `data` in a standard data file (such as CSV), you can import it in SAS, and run the [official SAS scoring script](https://epi.grants.cancer.gov/diet/screeners/fruitveg/scoring/allday.html).

# How to Score Your Data in R?

The All-Day Screener scoring algorithm is documented [here](https://epi.grants.cancer.gov/diet/screeners/fruitveg/scoring/allday.html).
Two SAS program files are provided to estimate the output in [Pyramid Servings](https://epi.grants.cancer.gov/diet/screeners/fruitveg/scoring/sasall-day.txt) and [MyPyramid Cup Equivalents](https://epi.grants.cancer.gov/diet/screeners/fruitveg/scoring/cupequiv.sasall-day.txt).

This package implements the All-Day Screener scoring algorithms in R. While one function is provided with options of output unit, we followed the variable naming convention in the SAS code to the extent possible. 

Once your data (encapsulated in data frame named `data`) is cleaned and formatted according to the official codebook, you can run this:

```
data_scored <- NCIFV_Scores(data)
```

By default, the output data frame `data_scored` will only include the `UNIQUEID` and the [computed variables](https://epi.grants.cancer.gov/nhanes/dietscreen/scoring/current/variables.html).

# Citation

Thompson FE, Subar AF, Smith AF, Midthune D, Radimer KL, Kahle LL, Kipnis V. **Fruit and vegetable assessment: performance of 2 new short instruments and a food frequency questionnaire.** *J Am Diet Assoc* 2002 Dec;102(12):1764-72
