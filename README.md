# What Is Fruved?

This package consists of a few survey scoring tools for R users. The tools have been used for health and behaviorial studies associated with the [GetFruved](http://fruved.com/) project. 

# How to Use This Package?

To use this package, you will need the `devtools` library to install it from github. 
For example, 

```
install.packages("devtools") # install devtools
library(devtools) # load devtools
install_github("wzhou7/Fruved") # install Fruved
```

The above code needs to be run just once.
Then, you can load the Fruved package each time you are ready to use it:

```
library(Fruved) # load Fruved
```

Check specific instrument pages for details about each available function.

# Which Instruments Are Included?

This `Fruved` package includes the following instruments: 
* [Body Mass Index](BMI.md) (BMI) and Growth Chart Standardized BMI (zBMI)
* [Cohen's 14-Item Stress Scale](Stress14.md)
* [International Physical Activity Questionnaire](IPAQ.md) (IPAQ)
* [Pittsburgh Sleep Quality Index](PSQI.md) (PSQI)
* [Short Pittsburgh Sleep Quality Index](sPSQI.md) (PSQI)
* [10-Item Personality](Personality.md)
* [Eating Disorder](ED.md)
* [Food Security](FS.md)
* [BECS](BECS.md)

If you are looking for one of the following dietary instruments, refer to the [`DietQ`](https://github.com/wzhou7/DietQ) package.
* Short Healthy Eating Index (sHEI)
* Healthy Eating Index (HEI) using NDSR
* NCI's Dietary Screener Questionnaire (DSQ)
* NCI's Percentage Energy from Fat Screener (NCIFat)
* NCT's Fruit & Vegetable Intake Screener (NCIFV)

If you are looking for one of the following instruments, refer to the [`CampusEnv`](https://github.com/wzhou7/CampusEnv) package.

* CEPS
* 41 Points
* Priorities
* Readiness
