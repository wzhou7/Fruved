# What Is Fruved?

This package consists of a few survey scoring tools for R users. The tools have been used for health and behaviorial studies associated with the [GetFruved](http://fruved.com/) project. In this project, survey data were collected online using Qualtrics. After [downloading data from Qualtrics](docs/qualtrics.md), functions in this package are used for data cleaning and instrument scoring.

# Which Instruments Are Included?

See [list of instruments included](./docs/instrument_list.md).

# How to Use This Package?

To use this package, you will need the `devtools` library to install it from github. For example,

```
install.packages("devtools") # install devtools
library(devtools) # load devtools
install_github("wzhou7/Fruved") # install Fruved
library(Fruved) # load Fruved
```

Then you can use the available scoring functions as long as the input data frame meet the formating requirements (see specific [instruments](./docs/instrument_list.md) for detail). For example, you may run

```
df_out <- DSQ_Score(df_in)
```

where `df_in` is the input data frame and `df_out` is the output data frame.

