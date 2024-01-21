# Growth Chart Standardized BMI (zBMI)

For children and teens (2 to 20 years), their BMIs are calculated using the same equation as adults. 
The BMI categories depends on the age- and sex-specific percentiles.

According to the [CDC website](https://www.cdc.gov/healthyweight/assessing/bmi/childrens_bmi/about_childrens_bmi.html),
for children and teens, the BMI categories are:

| Weight Status Category | Percentile Range                                |
| ---------------------- | ----------------------------------------------- |
| Underweight            | Less than the 5th percentile                    |
| Healthy Weight         | 5th percentile to less than the 85th percentile |
| Overweight             | 85th to less than the 95th percentile           |
| Obesity                | Equal to or greater than the 95th percentile    |

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
data <- Score_zBMI(data, cdc_age_month = "cdc_age_month", gender = "Gender")
```

Note: infants less than 2 years old will not have a BMI category.

### Missing Age

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

### Missing Gender

It is possible gender can be missing from the data, while it is required for categorizing non-adults. 
In this case, we will categorize these samples as boys and girls, respectively,
and accept the category if it is the same for both gender groups.
If they are different, the BMI category will be a missing value.
