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
* [Body Mass Index](docs/BMI.md) (BMI)
* [Growth Chart Standardized BMI](docs/zBMI.md) (zBMI)
* [International Physical Activity Questionnaire](docs/IPAQ.md) (IPAQ)
* [Pittsburgh Sleep Quality Index](docs/PSQI.md) (PSQI)
* [Short Pittsburgh Sleep Quality Index](docs/sPSQI.md) (sPSQI)
* [Cohen's 14-Item Stress Scale](docs/Stress14.md)
* [10-Item Personality](docs/Personality.md)
