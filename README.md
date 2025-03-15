# What Is Fruved?

This package consists of a few survey scoring tools for R users. The tools have been used for health behavioral studies such as the GetFruved project. 

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

This `Fruved` package includes the following instruments. 
Click on the link to find a brief description and demonstrate its primary use.
* CDC's [Body Mass Index](docs/BMI.md) (BMI)
* CDC's [Growth Chart Standardized BMI](docs/zBMI.md) (zBMI)
* [Pittsburgh Sleep Quality Index](docs/PSQI.md) (PSQI)
* [Short Pittsburgh Sleep Quality Index](docs/sPSQI.md) (sPSQI)
* Cohen's [14-Item Stress Scale](docs/Stress14.md)
* [International Physical Activity Questionnaire](docs/IPAQ.md) (IPAQ)
* [10-Item Personality](docs/Personality.md)
* [Healthy Eating Index (HEI)](docs/HEI.md) calculated from NDSR output
* [Short Healthy Eating Index (sHEI)](docs/sHEI.md) 
* NCI's [Dietary Screener Questionnaire (DSQ)](docs/DSQ.md)
* NCI's [Percentage Energy from Fat Screener (FAT)](docs/FAT.md)
* NCI's [Fruit & Vegetable Intake Screener (FV)](docs/FV.md)

**Disclaimer:** This package is shared for your convenience. 
While we make sincere efforts to make it correct, the code is provided as is. 
Therefore, please use it at your own risk and make an effort to verify results. 
Please [report any issues](https://github.com/wzhou7/Fruved/issues) or [send revision suggestions](https://github.com/wzhou7/Fruved/pulls).
