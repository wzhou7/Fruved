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

