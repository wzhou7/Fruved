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
```

Then you can load the Fruved package

```
library(Fruved) # load Fruved
```

and use the available functions. Check specific instrument pages for detail.
