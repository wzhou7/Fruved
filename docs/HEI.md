# What Is HEI?

The Healthy Eating Index (HEI) is developed by NIH's [National Cancer Institute](https://epi.grants.cancer.gov/hei/) (NCI). It calculates a score that represents the quality of one's diet. There are several versions of the scoring system and the latest is [HEI-2015](https://epi.grants.cancer.gov/hei/developing.html#2015). It calculates 9 adequacy components and 4 moderation components with a combined score that ranges from 0 to 100, where higher scores indicate higher quality. 

# How to Score Your Data in R?

To calculate HEI, it is necesary to estimate the intake of the above-mentioned 13 food components. One approach is to conduct a 24-hour recall (24HR) interview in which subjects report what they ate on their typical days. Such entries are entered into a database, and nutrients are calculated using a propriety software known as Nutrition Data System for Research ([NDSR](http://www.ncc.umn.edu/products/)). 

## Step 1. Prepare the Data

The NDSR database provides several output files, and we shall only need the "File 04" and "File 09" (see [NDSR User Manual](http://www.ncc.umn.edu/products/ndsr-user-manual/)). We assume both files are tab delimited with variable names in the first line. No special processing is needed.

## Step 2. Run the Scoring Code

To calculate HEI using NDSR output, SAS code and full documentation and are available [here](http://www.ncc.umn.edu/healthy-eating-index-hei/). 

While there are multiple versions possible for the data and NDSR software version, our assumption is to **calculate the HEI-2015 scores** per person when **multiple days of intake** data are available for each person and the **data were collected in NDSR 2013 or subsequent version**. The corresponding SAS code we followed is [here](https://drive.google.com/a/umn.edu/file/d/1XVnmQ38-ZU-Jds_AnP7mze1crz3XJvR8/view?usp=sharing). Our package provides an implementation in R. Despite a different programming language, we followed its variable naming convention as closely as possible.

You can run the following code to produce the HEI scores, as well as other variables of interest:

```
data_scored <- HEI_Scores(ID="Participant.ID",
                          DateVar="Date.of.Intake",
                          file4="file04.txt",
                          file9="file09.txt")
```

By default, the output data frame data_scored will include the `ID` and the [computed variables](HEI_output.md).

# Example

# Input and Output


"rikcal","rina","riwholegrains","rirefinedgrains",
                     "rias_ts","risfa","rimfa","ripfa","ripctsfa","water"


## HEI Output Variables

Our NDSR to HEI scoring code will output the following variables in one data frame. 

First, the HEI component and total scores:

* `HEIX1_TOTALVEG`       : HEI-2015 COMPONENT 1 TOTAL VEGETABLES (0-5)
* `HEIX2_GREEN_AND_BEAN` : HEI-2015 COMPONENT 2 GREENS AND BEANS (0-5)
* `HEIX3_TOTALFRUIT`     : HEI-2015 COMPONENT 3 TOTAL FRUIT (0-5)
* `HEIX4_WHOLEFRUIT`     : HEI-2015 COMPONENT 4 WHOLE FRUIT (0-5)
* `HEIX5_WHOLEGRAIN`     : HEI-2015 COMPONENT 5 WHOLE GRAINS (0-10)
* `HEIX6_TOTALDAIRY`     : HEI-2015 COMPONENT 6 DAIRY (0-10)
* `HEIX7_TOTPROT`        : HEI-2015 COMPONENT 7 TOTAL PROTEIN FOODS (0-5)
* `HEIX8_SEAPLANT_PROT`  : HEI-2015 COMPONENT 8 SEAFOOD AND PLANT PROTEIN (0-5)
* `HEIX9_FATTYACID`      : HEI-2015 COMPONENT 9 FATTY ACID RATIO (0-10)
* `HEIX10_SODIUM`        : HEI-2010 COMPONENT 10 SODIUM (0-10)
* `HEIX11_REFINEDGRAIN`  : HEI-2015 COMPONENT 11 REFINED GRAINS (0-10)
* `HEIX12_ADDEDSUGARS`   : HEI-2015 COMPONENT 12 ADDED SUGARS (0-10)
* `HEIX13_SATFATS`       : HEI-2015 COMPONENT 13 SATURATED FATS (0-10)
* `HEIX14_TOTAL` 

Then, the intake variables extracted from NDSR:

* `hei_totveg`           : total vegetable servings in cup equivalents
* `hei_greensbeans`      : greens and beans servings in cup equivalents
* `hei_totfruit`         : total fruit servings in cup equivalents
* `hei_wholefruit`       : whole fruit servings in cup equivalents
* `hei_dairy`            : dairy servings in cup equivalents
* `hei_wholegrains`      : whole grain servings in ounce equivalents
* `hei_totproteins`      : total protein servings in ounce equivalents
* `hei_seafoodplantprot` : sea food and plant protein servings in ounce equivalents
* `hei_refinedgrains`    : refined grains in ounce equivalents
* `hei_sodium`           : sodium intake in grams
* `hei_addedsugars`      : kcal from added sugars
* `rikcal`   : Total Energy (kilocalories) in kcal
* `ripfa`    : Total Polyunsaturated Fatty Acids (PUFA) in g
* `rimfa`    : Total Monounsaturated Fatty Acids (MUFA) in g
* `risfa`    : Total Saturated Fatty Acids (SFA) in g
* `ripctsfa` : Percent Calories from SFA
* `water`
* `calcium`
* `fiber`
* `greens` 
* `redorange` 
* `starchy` 
* `beans`
* `NDSRvlnf` 
* `NDSRfvl` 
* `NDSRfvlnf` 
* `NDSRssb`

Finally, the energy adjusted HEI component intake variables:

* `xhei_totfruit`         : total fruit servings in cup equivalents PER 1000 KCAL
* `xhei_wholefruit`       : whole fruit servings in cup equivalents PER 1000 KCAL
* `xhei_totveg`           : total vegetable servings in cup equivalents PER 1000 KCAL
* `xhei_greensbeans`      : greens and beans servings in cup equivalents PER 1000 KCAL
* `xhei_wholegrains`      : whole grain servings in ounce equivalents PER 1000 KCAL
* `xhei_dairy`            : dairy servings in cup equivalents PER 1000 KCAL
* `xhei_totproteins`      : protein servings in ounce equivalents PER 1000 KCAL
* `xhei_seafoodplantprot` : sea food and plant protein servings in ounce equivalents PER 1000 KCAL
* `xhei_refinedgrains`    : refined grains in ounce equivalents PER 1000 KCAL
* `xhei_fatacid`          : fatty acid ratio
* `xhei_sodium`           : sodium intake in grams PER 1000 KCAL
* `xhei_addedsugars`      : percent kcal from added sugars
* `xhei_satfats`          : percent saturated fatty acids
