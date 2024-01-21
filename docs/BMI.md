# Body Mass Index (BMI)

The BMI is often used as a cost effective screening tool for a variety of health metrics.
For children and teens, their BMIs should incorporate the growth chart. 
See [Growth Chart Standardized BMI](zBMI.md) (zBMI). 
For adults, see below.

The BMI is calculated as a person's weight in kilograms 
divided by the square of height in meters. The calculation formula is: 

$$
\begin{aligned}
 BMI = \frac{weight (kg)}{[height (m)]^2}
\end{aligned}
$$

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

## How to Use

After importing your data as a data frame, use the following code to obtain the BMI score:

```
data <- Score_BMI(data, wt="WeightLB", ht = "HeightIN", wt_unit = "lb", ht_unit = "in")
```

Be careful of specifying the correct measurement units, so the numbers will be converted correctly.

## Unit Conversion

If the weight is not measured in kilograms or the height is not measured in meters 
in your raw data, unit conversion is needed. 
In the above example, units will be converted automatically. 
If you would like to convert the unit for the height and/or weight (without calculating BMI),
follow this example:

```
data$WeightKG <- ConvertWt(data, wt = "WeightLB", wt_unit = "lb")
data$HeightM <- ConvertHt(data, ht = "HeightIN", ht_unit = "in")
```