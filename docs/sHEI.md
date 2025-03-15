# Short Healthy Eating Index (sHEI)

sHEI is a 22-item survey instrument developed for assessing dietary quality. Unlike the regular HEI calculations, which require collecting comprehensive food records, sHEI is a cost-effective way to quantify dietary quality. 

## How to Score Your Data in R?

Once your data (`data`) is cleaned and formatted according to our [codebook](sHEI_input.md), you can run this:

```
data_scored <- sHEI_Scores(data)
```

By default, the output data frame `data_scored` will only include the [computed variables](sHEI_output.md).

## Citation

If you use our sHEI instrument in your research, please cite the following paper:

> Colby, S., Zhou, W., Allison, C., Mathews, A. E., Olfert, M. D., Morrell, J. S., Byrd-Bredbenner, C., Greene, G., Brown, O., Kattelmann, K., & Shelnutt, K. (2020). **Development and Validation of the Short Healthy Eating Index Survey with a College Population to Assess Dietary Quality and Intake.** *Nutrients*, 12(9), 2611. https://doi.org/10.3390/nu12092611

The DOI link provides the open-access paper with the questionaire and its scoring algorithm documented in its appendix.


# Input and Output

## sHEI Input Data Formatting Requirements

To score sHEI, we require the following variables and encoding:

* `Gender` : Are you male or female? `M` = Male; `F` = Female. Other values are not possible.
* `Q1` (Fruit) : On average, how many servings of fruit (not including juice) do eat per day? Choices are: 
  - `1` = Less than 1
  - `2` = 1
  - `3` = 2
  - `4` = 3
  - `5` = 4
  - `6` = 5
  - `7` = 6 or more
  - `8` = Choose not to answer
* `Q2` (Fruitjuice) : On average, how many servings of 100% fruit juice do you drink per day? Choices are the same as `Q1`.
* `Q3` (Vege) : On average, how many servings of vegetables do you eat per day? Choices are the same as `Q1`.
* `Q4` (Greenvege) : On average, how many servings of green vegetables do you eat per day? Choices are the same as `Q1`.
* `Q5` (Starchy) : On average, how many servings of starchy vegetables do you eat per day? Choices are the same as `Q1`.
* `Q6` (Grains) : On average, how many servings of grains do you eat per day? Choices are the same as `Q1`.
* `Q7` (Grains2) : *[shown only if "Less Than 1" is selected for `Q6`]*. On average, how often do you eat grains? Choices are: 
  - `1` = A couple times per week
  - `2` = A couple times per month 
  - `3` = A couple times per year 
  - `4` = Almost never
  - `5` = Never
  - `6` = Choose not to answer
* `Q8` (Wholegrains) : On average, how many servings of whole grains do you eat per day? Choices are the same as `Q1`.
* `Q9` (Wholegrains2) : *[shown only if "Less Than 1" is selected for `Q8`]*. On average, how often do you eat whole grains? Choices are the same as `Q7`.
* `Q10` (Milk) : On average, how many servings of milk do you eat or drink per day? Choices are the same as `Q1`.
* `Q11` (Milk2) : *[shown only if "Less Than 1" is selected for `Q10`]*. On average, how often do you drink or eat milk products? Choices are the same as `Q7`.
* `Q12` (Lowfatmilk) : On average, how many servings of low-fat milk products do you eat per day? Choices are the same as `Q1`.
* `Q13` (Lowfatmilk2) : *[shown only if "Less Than 1" is selected for `Q12`]*. On average, how often do you drink or eat low-fat milk products? Choices are the same as `Q7`.
* `Q14` (Beans) : On average, how many servings of beans (legumes) do you eat per day? Choices are the same as `Q1`.
* `Q15` (Nutseeds) : On average, how many servings of nuts or seeds do you eat per day? Choices are the same as `Q1`.
* `Q16` (Seafood): On average, how many servings of seafood do you eat per day? Choices are the same as `Q1`.
* `Q17` (Seafood2) : *[shown only if "Less Than 1" is selected for `Q16`]*. On average, how often do you eat seafood? Choices are the same as `Q7`.
* `Q18` (Ssb) : On average, how many sugar-sweetened beverages do you drink per day? Choices are the same as `Q1`.
* `Q19` (Ssb2) : *[shown only if "Less Than 1" is selected for `Q18`]*. On average, how often do you drink sugar-sweetened beverages? Choices are the same as `Q7`.
* `Q20` (Addedsugars) : On average, how much added sugars do you consume per day? Choices are: 
  - `1` = None/almost none
  - `2` = Some
  - `3` = A lot
  - `4` = Choose not to answer
* `Q21` (Satfat) : How many servings of saturated fat do you consume on average per day? Choices are the same as `Q20`.
* `Q22` (Water) : On average, how much water do you drink per day? Choices are the same as `Q20`.

## sHEI Output Variables

The output of our sHEI code is a data frame that includes estimated HEI component and total scores and certain food group intakes.

Estimates of component and total HEI scores are:
* `sHEI_total_fruits` : sHEI estimated Total Fruits Component (compared to total_fruits in HEI), 0–5
* `sHEI_whole_fruits` : sHEI estimated Whole Fruits Component (whole_fruits), 0–5
* `sHEI_tot_veg` : sHEI estimated Total Vegetables Component (tot_veg), 0–5
* `sHEI_greens_beans` : sHEI estimated  Greens and Beans Component (greens_beans), 0–5
* `sHEI_whole_grains` : sHEI estimated Whole Grains Component (whole_grains), 0–10
* `sHEI_dairy` : sHEI estimated Dairy Component (Dairy), 0–10
* `sHEI_tot_proteins` : sHEI estimated Total Protein Foods Component (tot_proteins), 0–5
* `sHEI_seafood_plant` : sHEI estimated Seafood and Plant Protein Component (Seafood_Plant), 0–5
* `sHEI_fatty_acid` : sHEI estimated Fatty Acid Ratio Component (Fatty_Acid), 0–10
* `sHEI_refined_grains` : sHEI estimated Refined Grains Component (Refined_Grains), 0–10
* `sHEI_sodium` : sHEI estimated Sodium Component (Sodium), 0–10
* `sHEI_added_sugars` : sHEI estimated Added Sugars Component (Added_Sugars), 0–10
* `sHEI_sat_fat` : sHEI estimated Saturated Fats Component (sat_fat), 0–10
* `sHEI_tot_score` : sHEI estimated Total DQ Score (0–100)

Estimates of food group intakes are:
* `sHEI_DSQfvl` : sHEI estimated Total Fruit and Vegetable Servings in Cup Equivalents Including Legumes and French Fries (DSQfvl) 
* `sHEI_DSQfvlnf` : sHEI estimated Total Fruit and Vegetable Servings in Cup Equivalents Including Legumes and Excluding French Fries
(Dsqfvlnf)
* `sHEI_DSQfrt` : sHEI estimated Total Fruit Servings in Cup Equivalents (Dsqfrt)
* `sHEI_DSQvlall` : sHEI estimated Total Vegetable Servings in Cup Equivalents Including Legumes and French Fries (Dsqvlall)
* `sHEI_DSQvlnf` : sHEI estimated Total Vegetable Servings in Cup Equivalents Including Legumes and Excluding French Fries (Dsqvlnf)
* `sHEI_DSQdairy` : sHEI estimated Dairy Servings in Cup Equivalents (Dsqdairy)
* `sHEI_DSQsug` :  sHEI estimated Added Sugars in Teaspoon Equivalents (Dsqsug)
* `sHEI_DSQssb` :  sHEI estimated Added Sugars from Sugar-Sweetened Beverages in Teaspoon Equivalents (DSQssb)
* `sHEI_DSQwhgr` : sHEI estimated Whole Grains in Ounce Equivalents (DSQwhgr)
* `sHEI_DSQfib` : sHEI estimated Fiber in Grams (DSQfib)
* `sHEI_DSQcalc` : sHEI estimated Calcium in Milligrams (DSQcalc)
* `sHEI_greens` : sHEI estimated Green Vegetables in Cup Equivalents
