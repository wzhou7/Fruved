# Body Mass Index (BMI)

The BMI is often used as a cost effective screening tool for a variety of health metrics.
For adults, the BMI is calculated as a person's weight in kilograms 
divided by the square of height in meters. The calculation formula is: 

$$
\begin{aligned}
 BMI = \frac{weight (kg)}{[height (m)]^2}
\end{aligned}
$$

## For Adults

According to CDC, "For adults 20 years old and older, BMI is interpreted using standard weight status categories. 
These categories are the same for men and women of all body types and ages.""
The categories are:

| BMI            | Weight Status  |
| -------------- | -------------- |
| Below 18.5     | Underweight    |
| 18.5 - 24.9    | Healthy Weight |
| 25.0 - 29.9    | Overweight     |
| 30.0 and Above | Obesity        |

More information can be found on the [CDC website](https://www.cdc.gov/healthyweight/assessing/bmi/adult_bmi/index.html) and the [Wikipedia](https://en.wikipedia.org/wiki/Body_mass_index).

If all samples are adults, to calculate BMI and its categories, we do not need age or gender. 
After importing your data as a data frame, use the following code to obtain the BMI score:

```
data <- Score_BMI_Adults(data, wt="WeightLB", ht = "HeightIN", wt_unit = "lb", ht_unit = "in")
```

Be careful of specifying the correct measurement units, so the numbers will be converted correctly.

## Unit Conversion

If the weight is not measured in kilograms or the height is not measured in meters 
in your raw data, unit conversion is needed. 
In the above example, units will be converted automatically. 
If you would like to convert the unit for the height and/or weight (without calculating BMI),
follow this example:

```
data <- ConvertWt(data, wt = "WeightLB", wt_unit = "lb")
data <- ConvertHt(data, ht = "HeightIN", ht_unit = "in")
```

## Plausibility and Outlier Flags

Any value outside of the recorded human range of height, weight, and BMI are flagged as implausible values. 
We followed such cutoff via Google search and Wikipedia (retrieved on 1/20/2024). 
* Highest height (Robert Wadlow, 272.034 cm), see [List of Tallest People](https://en.wikipedia.org/wiki/List_of_tallest_people)
* Lowest height (Chandra Bahadur Dangi, 54.6cm), see [List of the Verified Shortest People](https://en.wikipedia.org/wiki/List_of_the_verified_shortest_people)
* Highest weight (Jon Brower Minnoch, peak weight = 650kg)
* Lowest weight ([Lucia Zarate](https://en.wikipedia.org/wiki/Luc%C3%ADa_Z%C3%A1rate), 2.1kg)
* Highest BMI (Eman Ahmed Abd El Aty, peak BMI = 251.1), see [List_of_Heaviest_People](https://en.wikipedia.org/wiki/List_of_heaviest_people)
* Lowest BMI (6.7 kg/m2 reported in Suszko et al. (2022) Mortality in extremely low BMI anorexia nervosa patients - implications of gastrointestinal and endocrine system dysfunction. Psychiatr Pol. 2022 Feb 27;56(1):89-100. doi: 10.12740/PP/126233)

For data with at least 30 non-missing values (height, weight, or BMI), we determine outliers and extreme outliers according to the boxplot rules. 

# Growth Chart Standardized BMI (zBMI)

For children and teens, their BMIs are calculated using the same equation. 
The BMI categories depends on the age- and sex-specific percentiles.

According to the [CDC website](https://www.cdc.gov/healthyweight/assessing/bmi/childrens_bmi/about_childrens_bmi.html),
for children and teens, the BMI categories are:

| Weight Status Category | Percentile Range |
| Underweight            | Less than the 5th percentile |
| Healthy Weight         | 5th percentile to less than the 85th percentile |
| Overweight             | 85th to less than the 95th percentile |
| Obesity                | Equal to or greater than the 95th percentile |

## Age in Months

If your dataset has at least some children/teens, age information is required.

CDC's percentile table is available for Children and adolescents, 2 to 20 years. 
See [Data Table of BMI-for-age Charts](https://www.cdc.gov/growthcharts/html_charts/bmiagerev.htm#males).
Since the months are numbers like `[24, 24.5, 25.5, ..., 239.5, 240, 240.5]`, 
the age in the raw data needs to be converted into numbers rounded up/down like these. 
To do so, we implemented a stand-alone function to easier table join:

```
data$cdc_age_month <- CDC_AgeMonth(data, birth_date="DOB", data_date = "StartDate")
```

After calculating age in months, we can categorize BMI:
```
Score_zBMI <- function(data, wt="WeightLB", ht = "HeightIN", 
                       wt_unit = "lb", ht_unit = "in", 
                       cdc_age_month = "cdc_age_month", gender = "Gender")
```

## Missing Gender

It is possible gender can be missing from the data, while it is required for categorizing non-adults. 
In this case, we will categorize these samples as boys and girls, respectively,
and accept the category if it is the same for both gender groups.
If they are different, the BMI category will be a missing value.

## Missing Age

If the sample contains at least some non-adults, we require age (in months). 
Datasets with a mix of adults and non-adults can be scored together using the following code:
```
Score_BMI <- function(data, wt = "WeightLB", ht = "HeightIN", 
                      wt_unit = "lb", ht_unit = "in", 
                      birth_date = "birth_date", 
                      data_date = "data_date", 
                      gender = "Gender")
```
Note that this code assumes the birth date (`birth_date`) and 
the data collection date (`data_date`) will be used to determine age.
Cases for which the age cannot be determined will have missing values in their BMI categories.
