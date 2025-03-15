# What is NCI Fat?

NCI Fat stands for NCI's Percentage Energy from Fat Screener (NCIFat).
It is a 17-item survey instrument developed by NIH's [National Cancer Instritute](https://epi.grants.cancer.gov/diet/screeners/fat/) (NCI). 
A copy of the questionaire can be obtained [here](https://epi.grants.cancer.gov/diet/shortreg/instruments/percent-energy-from-fat-screener.pdf) (PDF).

# Data Preparation

We provide the following utility function in R to rename your variable names. 
Please note that you still need to ensure that the [data encoding](DSQ_input.md) is correct for each variable.

```
data <- NCIFat_Variables(data, UNIQUEID="ID",
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

Once you save the output data frame `data` in a standard data file (such as CSV), you can import it in SAS, and run the [official SAS scoring script](https://epi.grants.cancer.gov/nhanes/dietscreen/scoring/current/self.paper.zip).

# How to Score Your Data in R?

The [scoring algorithms](https://epi.grants.cancer.gov/diet/screeners/fat/scoring.html) are well documented 
and a [SAS program file](https://epi.grants.cancer.gov/diet/screeners/fat/sas.energy.10262004.txt) is provided on its official website. 
This package implements the scoring algorithms in R. We followed the scoring logic and variable naming as closely as possible.

Once your data (encapsulated in data frame named `data`) is cleaned and formatted according to the official codebook, you can run this:

```
data_scored <- NCIFat_Scores(data)
```

By default, the output data frame `data_scored` will only include the `UNIQUEID` and the computed variables.


# Input and Output

## NCIFat Input Data Formatting Requirements

Think about your eating habits over the past 12 months.  About how often did you eat or drink each of the following foods?  Remember breakfast, lunch, dinner, snacks, and eating out.  Blacken in only one bubble for each food.	

Ncifat1	…Cold cereal	1)	Never
2)	Less than once per month
3)	1-3 times per month
4)	1-2 times per week
5)	3-4 times per week
6)	5-6 times per week
7)	1 time per day
8)	2 or more times per day
Ncifat2	…Skim milk, on cereal or to drink	Same as  Ncifat1
Ncifat3	…Eggs, fried or scrambled in margarine, butter, or oil	Same as  Ncifat1
Ncifat4	…Sausage or bacon, regular-fat	Same as  Ncifat1
Ncifat5	…Margarine or butter on bread, rolls, pancakes	Same as  Ncifat1
Ncifat6	…Orange juice or grapefruit juice	Same as  Ncifat1
Ncifat7	…Fruit (not juices)	Same as  Ncifat1
Ncifat8	…Beef or pork hot dogs, regular-fat	Same as  Ncifat1
Ncifat9	…Cheese or cheese spread, regular-fat	Same as  Ncifat1
Ncifat10	…French fries, home fries, or hash brown potatoes	Same as  Ncifat1
Ncifat11	…Margarine or butter on vegetables, including potatoes	Same as  Ncifat1
Ncifat12	…Mayonnaise, regular-fat	Same as  Ncifat1
Ncifat13	…Salad dressings, regular-fat	Same as  Ncifat1
Ncifat14	…Rice	Same as  Ncifat1
Ncifat15	…Margarine, butter, or oil on rice or pasta	Same as  Ncifat1

Ncifat16	Over the past 12 months, when you prepared foods with margarine or ate margarine, how often did you use a reduced-fat margarine?	
1)	Didn’t use margarine
2)	Almost never
3)	About ¼ of the time
4)	About ½ of the time
5)	About ¾ of the time
6)	Almost always or always

Ncifat17	Overall, when you think about the foods you ate over the past 12 months, would you say your diet was high, medium, or low in fat?	
1)	High
2)	Medium
3)	Low



