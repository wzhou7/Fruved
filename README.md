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
* [Body Mass Index](BMI.md) (BMI)
* [Growth Chart Standardized BMI (zBMI)] (zBMI)
* [Cohen's 14-Item Stress Scale](Stress14.md)
* [International Physical Activity Questionnaire](IPAQ.md) (IPAQ)
* [Pittsburgh Sleep Quality Index](PSQI.md) (PSQI)
* [Short Pittsburgh Sleep Quality Index](sPSQI.md) (sPSQI)
* [10-Item Personality](Personality.md)
* [26-Item Eating Disorder](ED.md)
* [Food Security](FS.md)
