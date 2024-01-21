Boys_BMI_Percentiles <- read.csv("raw_data/BMI_Percentiles_Boys.csv")
Girls_BMI_Percentiles <- read.csv("raw_data/BMI_Percentiles_Girls.csv")

Boys_BMI_Percentiles$Gender <- "M"
Girls_BMI_Percentiles$Gender <- "F"

old_vars <- c("Gender","Age..in.months.",
              "X5th.Percentile.BMI.Value",
              "X85th.Percentile.BMI.Value",
              "X95th.Percentile.BMI.Value")
new_vars <- c("Gender","CDC_AgeMonths","P5","P85","P95")

Boys_BMI_Percentiles <- Boys_BMI_Percentiles[,old_vars]
names(Boys_BMI_Percentiles) <- new_vars

Girls_BMI_Percentiles <- Girls_BMI_Percentiles[,old_vars]
names(Girls_BMI_Percentiles) <- new_vars

BMI_Percentiles <- rbind(Boys_BMI_Percentiles,Girls_BMI_Percentiles)
save(BMI_Percentiles, file="data/BMI_Percentiles.RData")

