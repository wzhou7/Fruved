# Body Mass Index (BMI)

The BMI is calculated as a person¡¯s weight in kilograms 
divided by the square of height in meters.
For adults, the calculation formula is: 

$BMI = \frac{weight (kg)}{[height (m)]^2}$

More information can be found on the [CDC website](https://www.cdc.gov/healthyweight/assessing/bmi/adult_bmi/index.html) and the [Wikipedia](https://en.wikipedia.org/wiki/Body_mass_index).

For children and teens, their BMIs should incorporate the growth chart. 
See [Growth Chart Standardized BMI](zBMI.md) (zBMI).

## How to Use

After importing your data as a data frame, use the following code to obtain the BMI score:

```
data <- Score_BMI(data, wt="WtLB", ht = "HtIN", wt_unit = "lb", ht_unit = "in")
```

Be careful of specifying the correct measurement units, so the numbers will be converted correctly.

## Unit Conversion

If the weight is not measured in kilograms or the height is not measured in meters 
in your raw data, unit conversion is needed. 

In the above example, units will be converted automatically. 
If you would like to convert the units and save the converted values in the data frame,
follow this example:

```
data$WtKG <- ConvertWt(data, wt = "WtLB", wt_unit = "lb")
data$HtM <- ConvertHt(data, ht = "HtIN", ht_unit = "in")
```