#' Comput sHEI scores
#'
#' Compute the estimated HEI scores using sHEI questionnaire responses
#' @param df The data frame with sHEI questionnaire responses
#' @param miss_val The value to use for missing data
#' @return A data frame with computed columns in addition
#' @examples
#' df_scored <- sHEI_scores(df);
#' df_scored <- sHEI_scores(df,miss_val=0);
#' @export
sHEI_scores <- function(df, miss_val=0){

  # encode missing values
  df$Q1[is.na(df$Q1)] <- 8 # Fruit
  df$Q2[is.na(df$Q2)] <- 8 # Fruit Juice
  df$Q3[is.na(df$Q3)] <- 8 # Vege
  df$Q4[is.na(df$Q4)] <- 8 # Green vege
  df$Q5[is.na(df$Q5)] <- 8 # Starchy
  df$Q6[is.na(df$Q6)] <- 8 # Grains
  df$Q7[is.na(df$Q7)] <- 6 # Grains2
  df$Q8[is.na(df$Q8)] <- 8 # Wholegrains
  df$Q9[is.na(df$Q9)] <- 6 # Wholegrains2
  df$Q10[is.na(df$Q10)] <- 8 # Milk
  df$Q11[is.na(df$Q11)] <- 6 # Milk2
  df$Q12[is.na(df$Q12)] <- 8 # Lowfatmilk
  df$Q13[is.na(df$Q13)] <- 6 # Lowfatmilk2
  df$Q14[is.na(df$Q14)] <- 8 # Beans
  df$Q15[is.na(df$Q15)] <- 8 # Nutseeds
  df$Q16[is.na(df$Q16)] <- 8 # Seafood
  df$Q17[is.na(df$Q17)] <- 6 # Seafood2
  df$Q18[is.na(df$Q18)] <- 8 # Ssb
  df$Q19[is.na(df$Q19)] <- 6 # Ssb2
  df$Q20[is.na(df$Q20)] <- 4 # Addedsugars
  df$Q21[is.na(df$Q21)] <- 4 # Satfat
  df$Q22[is.na(df$Q22)] <- 4 # Water

  #### encode combo items ###

  # Q16_17 (seafood_combo)
  df$Q16_17 <- 12 # missing value
  df$Q16_17[df$Q16==1 & df$Q17==5] <- 1
  df$Q16_17[df$Q16==1 & df$Q17==4] <- 2
  df$Q16_17[df$Q16==1 & df$Q17==3] <- 3
  df$Q16_17[df$Q16==1 & df$Q17==2] <- 4
  df$Q16_17[df$Q16==1 & df$Q17==1] <- 5
  df$Q16_17[df$Q16==2] <- 6
  df$Q16_17[df$Q16==3] <- 7
  df$Q16_17[df$Q16==4] <- 8
  df$Q16_17[df$Q16==5] <- 9
  df$Q16_17[df$Q16==6] <- 10
  df$Q16_17[df$Q16==7] <- 11

  # Q10_11 (milk_combo)
  df$Q10_11 <- 12 # missing value
  df$Q10_11[df$Q10==1 & df$Q11==5] <- 1
  df$Q10_11[df$Q10==1 & df$Q11==4] <- 2
  df$Q10_11[df$Q10==1 & df$Q11==3] <- 3
  df$Q10_11[df$Q10==1 & df$Q11==2] <- 4
  df$Q10_11[df$Q10==1 & df$Q11==1] <- 5
  df$Q10_11[df$Q10==2] <- 6
  df$Q10_11[df$Q10==3] <- 7
  df$Q10_11[df$Q10==4] <- 8
  df$Q10_11[df$Q10==5] <- 9
  df$Q10_11[df$Q10==6] <- 10
  df$Q10_11[df$Q10==7] <- 11

  # Q12_13 (lowfatmilk_combo)
  df$Q12_13 <- 12 # missing value
  df$Q12_13[df$Q12==1 & df$Q13==5] <- 1
  df$Q12_13[df$Q12==1 & df$Q13==4] <- 2
  df$Q12_13[df$Q12==1 & df$Q13==3] <- 3
  df$Q12_13[df$Q12==1 & df$Q13==2] <- 4
  df$Q12_13[df$Q12==1 & df$Q13==1] <- 5
  df$Q12_13[df$Q12==2] <- 6
  df$Q12_13[df$Q12==3] <- 7
  df$Q12_13[df$Q12==4] <- 8
  df$Q12_13[df$Q12==5] <- 9
  df$Q12_13[df$Q12==6] <- 10
  df$Q12_13[df$Q12==7] <- 11

  # Q18_19 (ssb_combo)
  df$Q18_19 <- 12 # missing value
  df$Q18_19[df$Q18==1 & df$Q19==5] <- 1
  df$Q18_19[df$Q18==1 & df$Q19==4] <- 2
  df$Q18_19[df$Q18==1 & df$Q19==3] <- 3
  df$Q18_19[df$Q18==1 & df$Q19==2] <- 4
  df$Q18_19[df$Q18==1 & df$Q19==1] <- 5
  df$Q18_19[df$Q18==2] <- 6
  df$Q18_19[df$Q18==3] <- 7
  df$Q18_19[df$Q18==4] <- 8
  df$Q18_19[df$Q18==5] <- 9
  df$Q18_19[df$Q18==6] <- 10
  df$Q18_19[df$Q18==7] <- 11

  # Q8_9 (wholegrains_combo)
  df$Q8_9 <- 12 # missing value
  df$Q8_9[df$Q8==1 & df$Q9==5] <- 1
  df$Q8_9[df$Q8==1 & df$Q9==4] <- 2
  df$Q8_9[df$Q8==1 & df$Q9==3] <- 3
  df$Q8_9[df$Q8==1 & df$Q9==2] <- 4
  df$Q8_9[df$Q8==1 & df$Q9==1] <- 5
  df$Q8_9[df$Q8==2] <- 6
  df$Q8_9[df$Q8==3] <- 7
  df$Q8_9[df$Q8==4] <- 8
  df$Q8_9[df$Q8==5] <- 9
  df$Q8_9[df$Q8==6] <- 10
  df$Q8_9[df$Q8==7] <- 11

  # Q6_7 (grains_combo)
  df$Q6_7 <- 12 # missing value
  df$Q6_7[df$Q6==1 & df$Q7==5] <- 1
  df$Q6_7[df$Q6==1 & df$Q7==4] <- 2
  df$Q6_7[df$Q6==1 & df$Q7==3] <- 3
  df$Q6_7[df$Q6==1 & df$Q7==2] <- 4
  df$Q6_7[df$Q6==1 & df$Q7==1] <- 5
  df$Q6_7[df$Q6==2] <- 6
  df$Q6_7[df$Q6==3] <- 7
  df$Q6_7[df$Q6==4] <- 8
  df$Q6_7[df$Q6==5] <- 9
  df$Q6_7[df$Q6==6] <- 10
  df$Q6_7[df$Q6==7] <- 11

  #####################################################
  #### Estimating sHEI Total Dietary Quality Score ####
  #####################################################

  # --------------------------------------------------#
  # Total Fruits Component (total_fruits), 0-5

  # IF Q1 (fruit) = 1 THEN total_fruits_Q1 = 0;
  # IF Q1 (fruit) = 2 THEN total_fruits_Q1 = 2;
  # IF Q1 (fruit) = 3 THEN total_fruits_Q1 = 3.5;
  # IF Q1 (fruit) = 4,5,6,7 THEN total_fruits_Q1 = 5.
  total_fruits_Q1 <- rep(miss_val,NROW(df))
  total_fruits_Q1[df$Q1==1] <- 0
  total_fruits_Q1[df$Q1==2] <- 2
  total_fruits_Q1[df$Q1==3] <- 3.5
  total_fruits_Q1[df$Q1 %in% c(4,5,6,7)] <- 5

  # IF Q2 (fruitjuice) = 1 THEN total_fruits_Q2 = 0;
  # IF Q2 (fruitjuice) = 2 THEN total_fruits_Q2 = 2;
  # IF Q2 (fruitjuice) = 3 THEN total_fruits_Q2 = 3.5;
  # IF Q2 (fruitjuice) = 4,5,6,7 THEN total_fruits_Q2 = 5.
  total_fruits_Q2 <- rep(miss_val,NROW(df))
  total_fruits_Q2[df$Q2==1] <- 0
  total_fruits_Q2[df$Q2==2] <- 2
  total_fruits_Q2[df$Q2==3] <- 3.5
  total_fruits_Q2[df$Q2 %in% c(4,5,6,7)] <- 5

  total_fruits = total_fruits_Q1 + total_fruits_Q2

  # IF total_fruits > 5 THEN total_fruits = 5.
  total_fruits[(!is.na(total_fruits)) & (total_fruits>5)] <- 5

  # --------------------------------------------------#
  # Whole Fruits Component (whole_fruits), 0-5
  # IF Q1 (fruit) = 1 THEN whole_fruits = 0;
  # IF Q1 (fruit) = 2 THEN whole_fruits = 2.5;
  # IF Q1 (fruit) = 3,4,5,6,7 THEN whole_fruits = 5.
  whole_fruits <- rep(miss_val,NROW(df))
  whole_fruits[df$Q1==1] <- 0
  whole_fruits[df$Q1==2] <- 2.5
  whole_fruits[df$Q1 %in% c(3,4,5,6,7)] <- 5

  # --------------------------------------------------#
  # Total Vegetables Component (tot_veg), 0-5
  # IF Q4 (greenvege) = 1 THEN tot_veg = 1.60;
  # IF Q5 (startchy) = 2,3,4,5,6,7 AND Q4 (greenvege) = 2 THEN tot_veg = 2.46;
  # IF Q5 (startchy) = 2,3,4,5,6,7 AND Q4 (greenvege) = 3,4,5,6,7 THEN tot_veg = 3.24;
  # IF Q5 (startchy) = 1 AND Q4 (greenvege) = 2,3,4,5,6,7 THEN tot_veg = 3.56.
  tot_veg <- rep(miss_val,NROW(df))

  # Q4=1, Q5=1
  # Q4=1, Q5=2,3,4,5,6,7
  tot_veg[df$Q4==1] <- 1.60

  # Q4=2, Q5=2,3,4,5,6,7
  tot_veg[df$Q4==2 & df$Q5 %in% c(2,3,4,5,6,7)] <- 2.46

  # Q4=3,4,5,6,7, Q5=2,3,4,5,6,7
  tot_veg[df$Q4 %in% c(3,4,5,6,7) & df$Q5 %in% c(2,3,4,5,6,7)] <- 3.24

  # Q4=2,3,4,5,6,7, Q5=1
  tot_veg[df$Q4 %in% c(2,3,4,5,6,7) & df$Q5==1] <- 3.56

  # --------------------------------------------------#
  # Greens and Beans Component (greens_beans), 0-5
  # IF Q4 (greenvege) = 1 THEN greens_beans_Q7 = 0;
  # IF Q4 (greenvege) = 2,3,4,5,6,7 THEN greens_beans_Q7 = 5.
  greens_beans_Q4 <- rep(miss_val,NROW(df))
  greens_beans_Q4[df$Q4==1] <- 0
  greens_beans_Q4[df$Q4 %in% c(2,3,4,5,6,7)] <- 5

  # IF Q14 (beans) = 1 THEN greens_beans_Q14 = 0;
  # IF Q14 (beans) = 2,3,4,5,6,7 THEN greens_beans_Q14 = 5.
  greens_beans_Q14 <- rep(miss_val,NROW(df))
  greens_beans_Q14[df$Q14==1] <- 0
  greens_beans_Q14[df$Q14 %in% c(2,3,4,5,6,7)] <- 5

  greens_beans = greens_beans_Q4 + greens_beans_Q14

  # IF greens_beans > 5 THEN greens_beans = 5.
  greens_beans[(!is.na(greens_beans)) & (greens_beans>5)] <- 5

  # --------------------------------------------------#
  # Whole Grains Component (whole_grains), 0-10
  # IF Q8 (wholegrains) = 1 THEN whole_grains = 0.51;
  # IF Gender = M AND Q8 (wholegrains) = 2,3,4,5,6,7 THEN whole_grains = 2.97;
  # IF Gender = F AND Q8 (wholegrains) = 2,3 THEN whole_grains = 5.20;
  # IF Gender = F AND Q8 (wholegrains) = 4,5,6,7 THEN whole_grains = 6.94.
  whole_grains <- rep(miss_val,NROW(df))
  whole_grains[df$Q8==1] <- 0.51
  whole_grains[df$Q8 %in% c(2,3,4,5,6,7) & df$Gender=="M"] <- 2.97
  whole_grains[df$Q8 %in% c(2,3) & df$Gender=="F"] <- 5.20
  whole_grains[df$Q8 %in% c(4,5,6,7) & df$Gender=="F"] <- 6.94

  # --------------------------------------------------#
  # Dairy Component (Dairy), 0-10
  # IF Gender = M AND Q10 (milk) = 1,2,3 THEN dairy = 3.22;
  # IF Gender = F AND Q10 (milk) = 1,2,3 AND Q12 (lowfatmilk) = 1 THEN dairy = 3.32;
  # IF Gender = F AND Q10 (milk) = 1,2,3 AND Q12 (lowfatmilk) = 2,3,4,5,6,7
  #     THEN dairy = 4.81;
  # IF Q10 (milk) = 4,5,6,7 THEN dairy = 6.51.
  dairy <- rep(miss_val,NROW(df))
  dairy[df$Gender=="M" & df$Q10 %in% c(1,2,3)] <- 3.22
  dairy[df$Gender=="F" & df$Q10 %in% c(1,2,3) & df$Q12==1] <- 3.32
  dairy[df$Gender=="F" & df$Q10 %in% c(1,2,3) & df$Q12 %in% c(2,3,4,5,6,7)] <- 4.81
  dairy[df$Q10 %in% c(4,5,6,7)] <- 6.51

  # --------------------------------------------------#
  # Total Protein Foods Component (tot_proteins), 0-5
  # IF Gender = M AND Q16_17 (seafood_combo) = 1,2,3,4 THEN tot_proteins = 4.11;
  # IF Gender = M AND Q16_17 (seafood_combo) = 5,6,7,8,9,10,11 THEN tot_proteins = 4.98;
  # IF Gender = F THEN tot_proteins = 4.97.
  tot_proteins <- rep(miss_val,NROW(df))
  tot_proteins[df$Gender=="M" & df$Q16_17 %in% c(1,2,3,4)] <- 4.11
  tot_proteins[df$Gender=="M" & df$Q16_17 %in% c(5,6,7,8,9,10,11)] <- 4.98
  tot_proteins[df$Gender=="F"] <- 4.97

  # --------------------------------------------------#
  # Seafood and Plant Protein Component (Seafood_Plant), 0-5
  # IF Gender = M AND Q15 (nutseeds) = 1,2 THEN seafood_plant = 0.49;
  # IF Gender = F AND Q15 (nutseeds) = 1,2 THEN seafood_plant = 1.50;
  # IF Q15 (nutseeds) = 3,4,5,6,7 THEN seafood_plant = 4.20.
  seafood_plant <- rep(miss_val,NROW(df))
  seafood_plant[df$Gender=="M" & df$Q15 %in% c(1,2)] <- 0.49
  seafood_plant[df$Gender=="F" & df$Q15 %in% c(1,2)] <- 1.50
  seafood_plant[df$Q15 %in% c(3,4,5,6,7)] <- 4.20

  # --------------------------------------------------#
  # Fatty Acid Ratio Component (Fatty_Acid), 0-10
  # IF Q10 (milk) = 4,5,6,7 THEN fatty_acid = 2.56;
  # IF Q21 (satfat) = 2,3 AND Q10_11 (milk_combo) = 1,2,3,4,5,6,7
  #     AND Q12-13 (lowfatmilk_combo) = 1,2 THEN fatty_acid = 2.63;
  # IF Q21 (satfat) = 2,3 AND Q10_11 (milk_combo) = 1,2,3,4,5,6,7
  #     AND Q12_13 (lowfatmilk_combo) = 3,4,5,6,7,8,9,10,11
  #     THEN fatty_acid = 4.54;
  # IF Q21 (satfat) = 1 AND Q10_11 (milk_combo) = 1,2,3,4,5,6,7
  #     THEN fatty_acid = 5.93.
  fatty_acid <- rep(miss_val,NROW(df))
  fatty_acid[df$Q10 %in% c(4,5,6,7)] <- 2.56
  fatty_acid[df$Q21 %in% c(2,3) &
               df$Q10_11 %in% c(1,2,3,4,5,6,7) &
               df$Q12_13 %in% c(1,2)] <- 2.63
  fatty_acid[df$Q21 %in% c(2,3) &
               df$Q10_11 %in% c(1,2,3,4,5,6,7) &
               df$Q12_13 %in% c(3,4,5,6,7,8,9,10,11)] <- 4.54
  fatty_acid[df$Q21==1 &
               df$Q10_11 %in% c(1,2,3,4,5,6,7)] <- 5.93

  # --------------------------------------------------#
  # Refined Grains Component (Refined_Grains), 0-10
  refined_grains <- rep(miss_val,NROW(df))
  # IF Q4 (greenvege) = 1 THEN refined_grains = 2.13;
  refined_grains[df$Q4==1] <- 2.13
  # IF Q6 (grains) = 3,4,5,6,7 AND Q16 (seafood) = 2,3,4,5,6,7
  #     AND Q4 (greenvege) = 2,3,4,5,6,7 THEN refined_grains = 2.27;
  refined_grains[df$Q6 %in% c(3,4,5,6,7) &
                   df$Q16 %in% c(2,3,4,5,6,7) &
                   df$Q4 %in% c(2,3,4,5,6,7)] <- 2.27
  # IF Q6 (grains) = 3,4,5,6,7 AND Q15 (nutseeds) = 1,2
  #     AND Q16 (seafood) = 1 AND Q4 (greenvege) = 2,3,4,5,6,7
  #     THEN refined_grains = 4.73;
  refined_grains[df$Q6 %in% c(3,4,5,6,7) &
                   df$Q15 %in% c(1,2) &
                   df$Q16==1 &
                   df$Q4 %in% c(2,3,4,5,6,7)] <- 4.73
  # IF Q6 (grains) = 3,4,5,6,7 AND Q15 (nutseeds) = 3,4,5,6,7
  #     AND Q16 (seafood) = 1 AND Q4 (greenvege) = 2,3,4,5,6,7
  #     THEN refined_grains = 8.45;
  refined_grains[df$Q6 %in% c(3,4,5,6,7) &
                   df$Q15 %in% c(3,4,5,6,7) &
                   df$Q16==1 &
                   df$Q4 %in% c(2,3,4,5,6,7)] <- 8.45
  # IF Q6 (grains) = 1,2 AND Q4 (greenvege) = 2,3,4,5,6,7
  #    THEN refined_grains = 9.25.
  refined_grains[df$Q6 %in% c(1,2) &
                   df$Q4 %in% c(2,3,4,5,6,7)] <- 9.25

  # --------------------------------------------------#
  # Sodium Component (Sodium), 0-10
  sodium <- rep(miss_val,NROW(df))
  # IF Q1 (fruit) = 1,2 AND Q6 (grains) = 3,4,5,6,7 AND Q22 (water) = 3
  #     THEN sodium = 0.70;
  sodium[df$Q1 %in% c(1,2) &
           df$Q6 %in% c(3,4,5,6,7) &
           df$Q22==3] <- 0.70
  # IF Q1 (fruit) = 3,4,5,6,7 AND Q6 (grains) = 3,4,5,6,7 AND Q22 (water) = 3
  #     THEN sodium = 2.30;
  sodium[df$Q1 %in% c(3,4,5,6,7) &
           df$Q6 %in% c(3,4,5,6,7) &
           df$Q22==3] <- 2.30
  # IF Q6 (grains) = 3,4,5,6,7 AND Q22 (water) = 1,2 THEN sodium = 4.94;
  sodium[df$Q6 %in% c(3,4,5,6,7) &
           df$Q22 %in% c(1,2)] <- 4.94
  # IF Q6 (grains) = 1,2 THEN sodium = 6.07.
  sodium[df$Q6 %in% c(1,2)] <- 6.07

  # --------------------------------------------------#
  # Added Sugars Component (Added_Sugars), 0-10
  # IF Q18 (ssb) = 1 THEN sugar_calories_Q18 = 0;
  # IF Q18 (ssb) = 2 THEN sugar_calories_Q18 = 156;
  # IF Q18 (ssb) = 3 THEN sugar_calories_Q18 = 312;
  # IF Q18 (ssb) = 4 THEN sugar_calories_Q18 = 468;
  # IF Q18 (ssb) = 5 THEN sugar_calories_Q18 = 624;
  # IF Q18 (ssb) = 6 THEN sugar_calories_Q18 = 780;
  # IF Q18 (ssb) = 7 THEN sugar_calories_Q18 = 936.
  sugar_calories_Q18 <- rep(miss_val,NROW(df))
  sugar_calories_Q18[df$Q18==1] <- 0
  sugar_calories_Q18[df$Q18==2] <- 156
  sugar_calories_Q18[df$Q18==3] <- 312
  sugar_calories_Q18[df$Q18==4] <- 468
  sugar_calories_Q18[df$Q18==5] <- 624
  sugar_calories_Q18[df$Q18==6] <- 780
  sugar_calories_Q18[df$Q18==7] <- 936

  # IF Q20 (addedsugars) = 1 THEN sugar_calories_Q20 = 130;
  # IF Q20 (addedsugars) = 2 THEN sugar_calories_Q20 = 260;
  # IF Q20 (addedsugars) = 3 THEN sugar_calories_Q20 = 520.
  sugar_calories_Q20 <- rep(miss_val,NROW(df))
  sugar_calories_Q20[df$Q20==1] <- 130
  sugar_calories_Q20[df$Q20==2] <- 260
  sugar_calories_Q20[df$Q20==3] <- 520

  sugar_calories = sugar_calories_Q18 + sugar_calories_Q20

  # IF sugar_calories < =130 THEN added_sugars = 10;
  # IF sugar_calories > 130 AND sugar_calories < 520 THEN added_sugars = 5;
  # IF sugar_calories > =520 THEN added_sugars = 0.
  added_sugars <- rep(miss_val,NROW(df))
  added_sugars[(!is.na(sugar_calories)) & (sugar_calories<=130)] <- 10
  added_sugars[(!is.na(sugar_calories)) & (sugar_calories>130) & (sugar_calories < 520)] <- 5
  added_sugars[(!is.na(sugar_calories)) & (sugar_calories>=520)] <- 0

  # --------------------------------------------------#
  # Saturated Fats Component (sat_fat), 0-10
  sat_fat <- rep(miss_val,NROW(df))
  # IF Q18 (ssb) = 3,4,5,6,7 THEN sat_fat = 1.82;
  sat_fat[df$Q18 %in% c(3,4,5,6,7)] <- 1.82
  # IF Q6 (grains) = 1,2 AND Q18 (ssb) = 1,2 THEN sat_fat = 3.20;
  sat_fat[df$Q6 %in% c(1,2) &
            df$Q18 %in% c(1,2)] <- 3.20
  # IF Q6 (grains) = 3,4,5,6,7 AND Q15 (nutseeds) = 1,2 AND Q18 (ssb) = 1,2
  #     THEN sat_fat = 4.64;
  sat_fat[df$Q6 %in% c(3,4,5,6,7) &
            df$Q15 %in% c(1,2) &
            df$Q18 %in% c(1,2)] <- 4.64
  # IF Q6 (grains) = 3,4,5,6,7 AND Q15 (nutseeds) = 3,4,5,6,7 AND
  #     Q18 (ssb) = 1,2 THEN sat_fat = 6.56.
  sat_fat[df$Q6 %in% c(3,4,5,6,7) &
            df$Q15 %in% c(3,4,5,6,7) &
            df$Q18 %in% c(1,2)] <- 6.56

  # --------------------------------------------------#
  # Total DQ Score (0-100)
  tot_score = total_fruits + whole_fruits + tot_veg +
    greens_beans + whole_grains + dairy + tot_proteins +
    seafood_plant + fatty_acid + refined_grains +
    sodium + added_sugars + sat_fat
  df_sHEI_scores <- data.frame(total_fruits, whole_fruits, tot_veg,
                               greens_beans, whole_grains, dairy, tot_proteins,
                               seafood_plant, fatty_acid, refined_grains,
                               sodium, added_sugars, sat_fat, tot_score)
  names(df_sHEI_scores) <- paste0("sHEI_",names(df_sHEI_scores))

  ###########################################
  #### Estimating Food Group Consumption ####
  ###########################################

  # --------------------------------------------------#
  # Total Fruit and Vegetable Servings in Cup Equivalents
  # Including Legumes and French Fries (DSQfvl)
  DSQfvl <- rep(miss_val,NROW(df))
  # IF Q1 (fruit) = 1 THEN DSQfvl = 1.81;
  DSQfvl[df$Q1==1] <- 1.81
  # IF Gender = F AND Q1 (fruit) = 2,3 THEN DSQfvl = 2.22;
  DSQfvl[df$Gender=="F" & df$Q1 %in% c(2,3)] <- 2.22
  # IF Gender = M AND Q1 (fruit) = 2,3 THEN DSQfvl = 2.53;
  DSQfvl[df$Gender=="M" & df$Q1 %in% c(2,3)] <- 2.53
  # IF Q1 (fruit) = 4,5,6,7 THEN DSQfvl = 3.21.
  DSQfvl[df$Q1 %in% c(4,5,6,7)] <- 3.21

  # --------------------------------------------------#
  # Total Fruit and Vegetable Servings in Cup Equivalents
  # Including Legumes and Excluding French Fries (Dsqfvlnf)
  DSQfvlnf <- rep(miss_val,NROW(df))
  # IF Q1 (fruit) = 1 THEN DSQfvlnf = 1.72;
  DSQfvlnf[df$Q1==1] <- 1.72
  # IF Gender = F AND Q1 (fruit) = 2,3 THEN DSQfvlnf = 2.15;
  DSQfvlnf[df$Gender=="F" & df$Q1 %in% c(2,3)] <- 2.15
  # IF Gender = M AND Q1 (fruit) = 2,3 THEN DSQfvlnf = 2.43;
  DSQfvlnf[df$Gender=="M" & df$Q1 %in% c(2,3)] <- 2.43
  # IF Q1 (fruit) = 4,5,6,7 THEN DSQfvlnf = 3.10.
  DSQfvlnf[df$Q1 %in% c(4,5,6,7)] <- 3.10

  # --------------------------------------------------#
  # Total Fruit Servings in Cup Equivalents (Dsqfrt)
  DSQfrt <- rep(miss_val,NROW(df))
  # IF Q1 (fruit) = 1 THEN DSQfrt = 0.52;
  DSQfrt[df$Q1==1] <- 0.52
  # IF Q1 (fruit) = 2 THEN DSQfrt = 0.78;
  DSQfrt[df$Q1==2] <- 0.78
  # IF Q1 (fruit) = 3 THEN DSQfrt = 0.99;
  DSQfrt[df$Q1==3] <- 0.99
  # IF Q1 (fruit) = 4,5,6,7 THEN DSQfrt = 1.52.
  DSQfrt[df$Q1 %in% c(4,5,6,7)] <- 1.52

  # --------------------------------------------------#
  # Total Vegetable Servings in Cup Equivalents Including
  # Legumes and French Fries (DSQvlall)
  DSQvlall <- rep(miss_val,NROW(df))

  # IF Q3 (vege) = 1,2 THEN DSQvlall = 1.34;
  DSQvlall[df$Q3 %in% c(1,2)] <- 1.34

  # IF Gender = F AND Q3 (vege) = 3,4 THEN DSQvlall = 1.40;
  DSQvlall[df$Gender=="F" & df$Q3 %in% c(3,4)] <- 1.40

  # IF Gender = F AND Q3 (vege) = 5,6,7 THEN DSQvlall = 1.68;
  DSQvlall[df$Gender=="F" & df$Q3 %in% c(5,6,7)] <- 1.68

  # IF Gender = M AND Q3 (vege) = 3,4,5,6,7 THEN DSQvlall = 1.84.
  DSQvlall[df$Gender=="M" & df$Q3 %in% c(3,4,5,6,7)] <- 1.84

  # --------------------------------------------------#
  # Total Vegetable Servings in Cup Equivalents Including
  # Legumes and Excluding French Fries (Dsqvlnf)
  DSQvlnf <- rep(miss_val,NROW(df))

  # IF Q3 (vege) = 1,2 THEN DSQvlnf = 1.19;
  DSQvlnf[df$Q3 %in% c(1,2)] <- 1.19

  # IF Gender = F AND Q3 (vege) = 3,4 THEN DSQvlnf = 1.28;
  DSQvlnf[df$Gender=="F" & df$Q3 %in% c(3,4)] <- 1.28

  # IF Gender = F AND Q3 (vege) = 5,6,7 THEN DSQvlnf = 1.59;
  DSQvlnf[df$Gender=="F" & df$Q3 %in% c(5,6,7)] <- 1.59

  # IF Gender = M AND Q3 (vege) = 3,4,5,6,7 THEN DSQvlnf = 1.71.
  DSQvlnf[df$Gender=="M" & df$Q3 %in% c(3,4,5,6,7)] <- 1.71

  # --------------------------------------------------#
  # Dairy Servings in Cup Equivalents (Dsqdairy)
  DSQdairy <- rep(miss_val,NROW(df))

  # IF Q10 (milk) = 1 THEN DSQdairy = 1.16;
  DSQdairy[df$Q10==1] <- 1.16

  # IF Gender = F AND Q10 (milk) = 2,3 THEN DSQdairy = 1.48;
  DSQdairy[df$Gender=="F" & df$Q10 %in% c(2,3)] <- 1.48

  # IF Gender = M AND Q10 (milk) = 2,3 THEN DSQdairy = 1.90;
  DSQdairy[df$Gender=="M" & df$Q10 %in% c(2,3)] <- 1.90

  # IF Q10 (milk) = 4,5,6,7 THEN DSQdairy = 2.36.
  DSQdairy[df$Q10 %in% c(4,5,6,7)] <- 2.36

  # --------------------------------------------------#
  # Added Sugars in Teaspoon Equivalents (Dsqsug)
  DSQsug <- rep(miss_val,NROW(df))

  # IF Q18_19 (ssb_combo) = 1,2,3,4 THEN DSQsug = 13.26;
  DSQsug[df$Q18_19 %in% c(1,2,3,4)] <- 13.26

  # IF Q18_19 (ssb_combo) = 5,6 THEN DSQsug = 16.00;
  DSQsug[df$Q18_19 %in% c(5,6)] <- 16.00

  # IF Q18 (ssb) = 3,4,5,6,7 THEN DSQsug = 26.87.
  DSQsug[df$Q18_19 %in% c(3,4,5,6,7)] <- 26.87

  # --------------------------------------------------#
  # Added Sugars from Sugar-Sweetened Beverages in Teaspoon Equivalents (DSQssb)
  DSQssb <- rep(miss_val,NROW(df))

  # IF Gender = F AND Q18_19 (ssb_combo) = 1,2,3,4 THEN DSQssb = 4.13;
  DSQssb[df$Gender=="F" & df$Q18_19 %in% c(1,2,3,4)] <- 4.13

  # IF Gender = M AND Q18_19 (ssb_combo) = 1,2,3,4 THEN DSQssb = 5.73;
  DSQssb[df$Gender=="M" & df$Q18_19 %in% c(1,2,3,4)] <- 5.73

  # IF Q18_19 (ssb_combo) = 5,6 THEN DSQssb = 6.79;
  DSQssb[df$Q18_19 %in% c(5,6)] <- 6.79

  # IF Q18 (ssb) = 3,4,5,6,7 THEN DSQssb = 15.78.
  DSQssb[df$Q18 %in% c(3,4,5,6,7)] <- 15.78

  # --------------------------------------------------#
  # Whole Grains in Ounce Equivalents (DSQwhgr)
  DSQwhgr <- rep(miss_val,NROW(df))

  # IF Q8_9 (wholegrains_combo) = 1,2,3,4 THEN DSQwhgr= 0.50;
  DSQwhgr[df$Q8_9 %in% c(1,2,3,4)] <- 0.50

  # IF Q6_7 (grains_combo) = 1,2,3,4,5,6 AND Q8_9 (wholegrains_combo) = 5,6,7
  #     THEN DSQwhgr = 0.63;
  DSQwhgr[df$Q6_7 %in% c(1,2,3,4,5,6) &
            df$Q8_9 %in% c(5,6,7)] <- 0.63

  # IF Q6 (grains) = 3,4,5,6,7 AND Q8_9 (wholegrains_combo) = 5,6,7
  #     THEN DSQwhgr = 0.77;
  DSQwhgr[df$Q6 %in% c(3,4,5,6,7) &
            df$Q8_9 %in% c(5,6,7)] <- 0.77

  # IF Q8 (wholegrains) = 4,5,6,7 THEN DSQwhgr = 1.01.
  DSQwhgr[df$Q8 %in% c(4,5,6,7)] <- 1.01

  # --------------------------------------------------#
  # Fiber in Grams (DSQfib)
  DSQfib <- rep(miss_val,NROW(df))

  # IF Gender = F AND Q1 (fruit) = 1,2,3 THEN DSQfib = 13.69;
  DSQfib[df$Gender=="F" & df$Q1 %in% c(1,2,3)] <- 13.69

  # IF Gender = M AND Q1 (fruit) = 1,2,3 THEN DSQfib = 16.74;
  DSQfib[df$Gender=="M" & df$Q1 %in% c(1,2,3)] <- 16.74

  # IF Q1 (fruit) = 4,5,6,7 THEN DSQfib = 19.32.
  DSQfib[df$Q1 %in% c(4,5,6,7)] <- 19.32

  # --------------------------------------------------#
  # Calcium in Milligrams (DSQcalc)
  DSQcalc <- rep(miss_val,NROW(df))

  # IF Gender = F AND Q10 (milk) = 1,2 THEN DSQcalc = 851.61;
  DSQcalc[df$Gender=="F" & df$Q10 %in% c(1,2)] <- 851.61

  # IF Gender = F AND Q10 (milk) = 3,4,5,6,7 THEN DSQcalc = 1010.78;
  DSQcalc[df$Gender=="F" & df$Q10 %in% c(3,4,5,6,7)] <- 1010.78

  # IF Gender = M AND Q10 (milk) = 1,2 THEN DSQcalc = 1062.55;
  DSQcalc[df$Gender=="M" & df$Q10 %in% c(1,2)] <- 1062.55

  # IF Gender = M AND Q10 (milk) = 3,4,5,6,7 THEN DSQcalc = 1319.71.
  DSQcalc[df$Gender=="M" & df$Q10 %in% c(3,4,5,6,7)] <- 1319.71

  # --------------------------------------------------#
  # Green Vegetables in Cup Equivalents
  greens <- rep(miss_val,NROW(df))

  # IF Q4 (greenvege) = 1 THEN greens = 0.00
  greens[df$Q4==1] <- 0.00

  # IF Q3 (vege) = 1,2,3,4 AND Q7 (greenvege) = 2,3,4,5,6,7 THEN greens = 0.13
  greens[df$Q3 %in% c(1,2,3,4) &
           df$Q4 %in% c(2,3,4,5,6,7)] <- 0.13

  # IF Q3 (vege) = 5,6,7 AND Q7 (greenvege) = 2,3,4,5,6,7 THEN greens = 0.31
  greens[df$Q3 %in% c(5,6,7) &
           df$Q4 %in% c(2,3,4,5,6,7)] <- 0.31

  df_sHEI_consump <- data.frame(DSQfvl, DSQfvlnf, DSQfrt, DSQvlall, DSQvlnf,
                                DSQdairy, DSQsug, DSQssb, DSQwhgr, DSQfib,
                                DSQcalc, greens)
  names(df_sHEI_consump) <- paste0("sHEI_",names(df_sHEI_consump))

  df <- cbind(df,df_sHEI_scores)
  df <- cbind(df,df_sHEI_consump)
  return(df)
}
