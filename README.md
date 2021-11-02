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
Then, you can load the `Fruved` package each time of use:

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
* [Short Pittsburgh Sleep Quality Index](sPSQI.md) (sPSQI)
* [10-Item Personality](Personality.md)
* [26-Item Eating Disorder](ED.md)
* [Food Security](FS.md)

If you are looking for one of the following dietary instruments, refer to the [`DietQ`](https://github.com/wzhou7/DietQ) package.
* Short Healthy Eating Index (sHEI)
* Healthy Eating Index (HEI) using NDSR
* NCI's Dietary Screener Questionnaire (DSQ)
* NCI's Percentage Energy from Fat Screener (NCIFat)
* NCT's Fruit & Vegetable Intake Screener (NCIFV)

If you are looking for one of the following instruments, refer to the [`CampusEnv`](https://github.com/wzhou7/CampusEnv) package.

* Behavior, Environment, and Changeability Survey (BECS)
* CEPS
* 41 Points
* Priorities
* Readiness


physical activity,[40, 41] dietary quality,[42] fruit and vegetable intake,[43] body image,[44-48] substance abuse,[49, 50] food security,[51-53] art,[54] stress,[55] sleep,[56] sexual orientation and gender identification,[57] breastfeeding,[58] minority and diversity,[59, 60] family history of health and chronic disease,[61-63] personality,[64] social media usage,[65-68] and eating disorders.[69] Additional tools included in the survey were developed and validated for use in this study and these included perceptions of the healthfulness of the college environment,[70] gardening, cooking, meal planning, food choice priorities[71], food safety, sustainable transportation, and green eating.(41)
Add legend explaining the NCI, IPAQ and Cohen’s stress. For example is 22.94 a good outcome?
