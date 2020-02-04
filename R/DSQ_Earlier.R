#trim <- function (x) gsub("^\\s+|\\s+$", "", x)
# library(stringr) => str_trim()

Score_DSQ_Earlier <- function(data){
  
  #########################
  #### Data Validation ####
  #########################
  
  # check all needed variables are available
  all_needed_vars <- c("Gender",
                       "Age",
                       "Dsqmilk1",
                       "Dsqmilk2",
                       "Dsqsoda",
                       "Dsqjuice",
                       "Dsqcoffee",
                       "Dsqdrink",
                       "Dsqfruit",
                       "Dsqsalad",
                       "Dsqfried",
                       "Dsqpotato",
                       "Dsqbean",
                       "Dsqgrain",
                       "Dsqveg",
                       "Dsqsalsa",
                       "Dsqpizza",
                       "Dsqsauce",
                       "Dsqcheese",
                       "Dsqmeat",
                       "Dsqproc",
                       "Dsqbread",
                       "Dsqcandy",
                       "Dsqrolls",
                       "Dsqcake",
                       "Dsqice",
                       "Dsqcorn",
                       "Dsqcereal",
                       "Dsqcertyp1",
                       "Dsqcertyp2")
  missing_vars <- setdiff(all_needed_vars,names(data))
  if(length(missing_vars)>0){
    stop(paste("There variables are not found:",
               paste(missing_vars,collapse = ", ")))
  }
  
  names(data)[names(data)=="Gender"] <- "gender"
  names(data)[names(data)=="Age"] <- "AGE"
  
  # make sure cols are char
  data <- fixColClasses(data,all_needed_vars)
  
  # check possible values - data validation
  # only coded values shown in the data dictionary allowed (use Wave1 data dictionary)
  # if unexpecvted values exist, throw an error - information (which variables)
  
  ######################
  #### Demographics ####
  ######################
  
  gender <- data$gender # need to ensure gender coding correct
  
  # AGE is an integer
  data$AGE <- round(as.numeric(data$AGE)) # ensure there is no AGE=2.5
  data$AGEGRP <- rep(NA,nrow(data))
  data$AGEGRP[data$AGE>=2 & data$AGE<=3] <- 1
  data$AGEGRP[data$AGE>=4 & data$AGE<=5] <- 2
  data$AGEGRP[data$AGE>=6 & data$AGE<=7] <- 3
  data$AGEGRP[data$AGE>=8 & data$AGE<=9] <- 4
  data$AGEGRP[data$AGE>=10 & data$AGE<=11] <- 5
  data$AGEGRP[data$AGE>=12 & data$AGE<=13] <- 6
  data$AGEGRP[data$AGE>=14 & data$AGE<=15] <- 7
  data$AGEGRP[data$AGE>=16 & data$AGE<=17] <- 8
  data$AGEGRP[data$AGE>=18 & data$AGE<=27] <- 9
  data$AGEGRP[data$AGE>=28 & data$AGE<=37] <- 10
  data$AGEGRP[data$AGE>=38 & data$AGE<=47] <- 11
  data$AGEGRP[data$AGE>=48 & data$AGE<=57] <- 12
  data$AGEGRP[data$AGE>=58 & data$AGE<=67] <- 13
  data$AGEGRP[data$AGE>=68 & data$AGE<=77] <- 14
  data$AGEGRP[data$AGE>=78] <- 15
  
  data$agecut <- rep(NA,nrow(data))
  data$agecut[data$AGEGRP>=1 & data$AGEGRP<=5] <- 3
  data$agecut[data$AGEGRP>=6 & data$AGEGRP<=8] <- 4
  data$agecut[data$AGEGRP>=9 & data$AGEGRP<=15] <- 2
  
  ###################
  #### Frequency ####
  ###################
  
  # hccerxpd='number of times per day eat hot or cold cereal'
  hccerxpd <- rep(NA,nrow(data))
  hccerxpd[data$Dsqcereal==1] <- 0
  hccerxpd[data$Dsqcereal==2] <- 0.033
  hccerxpd[data$Dsqcereal==3] <- 0.083
  hccerxpd[data$Dsqcereal==4] <- 0.143
  hccerxpd[data$Dsqcereal==5] <- 0.286
  hccerxpd[data$Dsqcereal==6] <- 0.5
  hccerxpd[data$Dsqcereal==7] <- 0.786
  hccerxpd[data$Dsqcereal==8] <- 1
  hccerxpd[data$Dsqcereal==9] <- 2
  summary(factor(hccerxpd))
  
  # milkxpd='number of times per day drink milk'
  milkxpd <- rep(NA,nrow(data))
  milkxpd[data$Dsqmilk1==1] <- 0
  milkxpd[data$Dsqmilk1==2] <- 0.033
  milkxpd[data$Dsqmilk1==3] <- 0.083
  milkxpd[data$Dsqmilk1==4] <- 0.143
  milkxpd[data$Dsqmilk1==5] <- 0.286
  milkxpd[data$Dsqmilk1==6] <- 0.5
  milkxpd[data$Dsqmilk1==7] <- 0.786
  milkxpd[data$Dsqmilk1==8] <- 1
  milkxpd[data$Dsqmilk1==9] <- 2.5
  milkxpd[data$Dsqmilk1==10] <- 4.5
  milkxpd[data$Dsqmilk1==11] <- 6
  summary(factor(milkxpd))
  
  # sodaxpd='number of times per day drink soda'
  sodaxpd <- rep(NA,nrow(data))
  sodaxpd[data$Dsqsoda==1] <- 0
  sodaxpd[data$Dsqsoda==2] <- 0.033
  sodaxpd[data$Dsqsoda==3] <- 0.083
  sodaxpd[data$Dsqsoda==4] <- 0.143
  sodaxpd[data$Dsqsoda==5] <- 0.286
  sodaxpd[data$Dsqsoda==6] <- 0.5
  sodaxpd[data$Dsqsoda==7] <- 0.786
  sodaxpd[data$Dsqsoda==8] <- 1
  sodaxpd[data$Dsqsoda==9] <- 2.5
  sodaxpd[data$Dsqsoda==10] <- 4.5
  sodaxpd[data$Dsqsoda==11] <- 6
  summary(factor(sodaxpd))
  # frtjcxpd='number of times per day drink fruit juice'
  frtjcxpd <- rep(NA,nrow(data))
  frtjcxpd[data$Dsqjuice==1] <- 0
  frtjcxpd[data$Dsqjuice==2] <- 0.033
  frtjcxpd[data$Dsqjuice==3] <- 0.083
  frtjcxpd[data$Dsqjuice==4] <- 0.143
  frtjcxpd[data$Dsqjuice==5] <- 0.286
  frtjcxpd[data$Dsqjuice==6] <- 0.5
  frtjcxpd[data$Dsqjuice==7] <- 0.786
  frtjcxpd[data$Dsqjuice==8] <- 1
  frtjcxpd[data$Dsqjuice==9] <- 2.5
  frtjcxpd[data$Dsqjuice==10] <- 4.5
  frtjcxpd[data$Dsqjuice==11] <- 6
  summary(factor(frtjcxpd))
  
  # swtctxpd='number of times per day drink sweet coffee/tea'
  swtctxpd <- rep(NA,nrow(data))
  swtctxpd[data$Dsqcoffee==1] <- 0
  swtctxpd[data$Dsqcoffee==2] <- 0.033
  swtctxpd[data$Dsqcoffee==3] <- 0.083
  swtctxpd[data$Dsqcoffee==4] <- 0.143
  swtctxpd[data$Dsqcoffee==5] <- 0.286
  swtctxpd[data$Dsqcoffee==6] <- 0.5
  swtctxpd[data$Dsqcoffee==7] <- 0.786
  swtctxpd[data$Dsqcoffee==8] <- 1
  swtctxpd[data$Dsqcoffee==9] <- 2.5
  swtctxpd[data$Dsqcoffee==10] <- 4.5
  swtctxpd[data$Dsqcoffee==11] <- 6
  swtctxpd[data$agecut==3] <- 0
  summary(factor(swtctxpd))
  # energyxpd='number of times per day drink fruit/sports/energy drink'
  energyxpd <- rep(NA,nrow(data))
  energyxpd[data$Dsqdrink==1] <- 0
  energyxpd[data$Dsqdrink==2] <- 0.033
  energyxpd[data$Dsqdrink==3] <- 0.083
  energyxpd[data$Dsqdrink==4] <- 0.143
  energyxpd[data$Dsqdrink==5] <- 0.286
  energyxpd[data$Dsqdrink==6] <- 0.5
  energyxpd[data$Dsqdrink==7] <- 0.786
  energyxpd[data$Dsqdrink==8] <- 1
  energyxpd[data$Dsqdrink==9] <- 2.5
  energyxpd[data$Dsqdrink==10] <- 4.5
  energyxpd[data$Dsqdrink==11] <- 6
  summary(factor(energyxpd))
  # fruitxpd='number of times per day eat fruit'
  fruitxpd <- rep(NA,nrow(data))
  fruitxpd[data$Dsqfruit==1] <- 0
  fruitxpd[data$Dsqfruit==2] <- 0.033
  fruitxpd[data$Dsqfruit==3] <- 0.083
  fruitxpd[data$Dsqfruit==4] <- 0.143
  fruitxpd[data$Dsqfruit==5] <- 0.286
  fruitxpd[data$Dsqfruit==6] <- 0.5
  fruitxpd[data$Dsqfruit==7] <- 0.786
  fruitxpd[data$Dsqfruit==8] <- 1
  fruitxpd[data$Dsqfruit==9] <- 2
  summary(as.factor(fruitxpd))
  # saladxpd='number of times per day eat salad'
  saladxpd <- rep(NA,nrow(data))
  saladxpd[data$Dsqsalad==1] <- 0
  saladxpd[data$Dsqsalad==2] <- 0.033
  saladxpd[data$Dsqsalad==3] <- 0.083
  saladxpd[data$Dsqsalad==4] <- 0.143
  saladxpd[data$Dsqsalad==5] <- 0.286
  saladxpd[data$Dsqsalad==6] <- 0.5
  saladxpd[data$Dsqsalad==7] <- 0.786
  saladxpd[data$Dsqsalad==8] <- 1
  saladxpd[data$Dsqsalad==9] <- 2
  summary(factor(saladxpd))
  # frfryxpd='number of times per day eat fried potatoes'
  frfryxpd <- rep(NA,nrow(data))
  frfryxpd[data$Dsqfried==1] <- 0
  frfryxpd[data$Dsqfried==2] <- 0.033
  frfryxpd[data$Dsqfried==3] <- 0.083
  frfryxpd[data$Dsqfried==4] <- 0.143
  frfryxpd[data$Dsqfried==5] <- 0.286
  frfryxpd[data$Dsqfried==6] <- 0.5
  frfryxpd[data$Dsqfried==7] <- 0.786
  frfryxpd[data$Dsqfried==8] <- 1
  frfryxpd[data$Dsqfried==9] <- 2
  summary(factor(frfryxpd))
  # othpotxpd='number of times per day eat other potatoes'
  othpotxpd <- rep(NA,nrow(data))
  othpotxpd[data$Dsqpotato==1] <- 0
  othpotxpd[data$Dsqpotato==2] <- 0.033
  othpotxpd[data$Dsqpotato==3] <- 0.083
  othpotxpd[data$Dsqpotato==4] <- 0.143
  othpotxpd[data$Dsqpotato==5] <- 0.286
  othpotxpd[data$Dsqpotato==6] <- 0.5
  othpotxpd[data$Dsqpotato==7] <- 0.786
  othpotxpd[data$Dsqpotato==8] <- 1
  othpotxpd[data$Dsqpotato==9] <- 2
  summary(factor(othpotxpd))
  # beanxpd='number of times per day eat beans'
  beanxpd <- rep(NA,nrow(data))
  beanxpd[data$Dsqbean==1] <- 0
  beanxpd[data$Dsqbean==2] <- 0.033
  beanxpd[data$Dsqbean==3] <- 0.083
  beanxpd[data$Dsqbean==4] <- 0.143
  beanxpd[data$Dsqbean==5] <- 0.286
  beanxpd[data$Dsqbean==6] <- 0.5
  beanxpd[data$Dsqbean==7] <- 0.786
  beanxpd[data$Dsqbean==8] <- 1
  beanxpd[data$Dsqbean==9] <- 2
  summary(factor(beanxpd))
  # othvegxpd='number of times per day eat other vegetables'
  othvegxpd <- rep(NA,nrow(data))
  othvegxpd[data$Dsqveg==1] <- 0
  othvegxpd[data$Dsqveg==2] <- 0.033
  othvegxpd[data$Dsqveg==3] <- 0.083
  othvegxpd[data$Dsqveg==4] <- 0.143
  othvegxpd[data$Dsqveg==5] <- 0.286
  othvegxpd[data$Dsqveg==6] <- 0.5
  othvegxpd[data$Dsqveg==7] <- 0.786
  othvegxpd[data$Dsqveg==8] <- 1
  othvegxpd[data$Dsqveg==9] <- 2
  summary(factor(othvegxpd))
  # pizzaxpd='number of times per day eat pizza'
  pizzaxpd <- rep(NA,nrow(data))
  pizzaxpd[data$Dsqpizza==1] <- 0
  pizzaxpd[data$Dsqpizza==2] <- 0.033
  pizzaxpd[data$Dsqpizza==3] <- 0.083
  pizzaxpd[data$Dsqpizza==4] <- 0.143
  pizzaxpd[data$Dsqpizza==5] <- 0.286
  pizzaxpd[data$Dsqpizza==6] <- 0.5
  pizzaxpd[data$Dsqpizza==7] <- 0.786
  pizzaxpd[data$Dsqpizza==8] <- 1
  pizzaxpd[data$Dsqpizza==9] <- 2
  summary(factor(pizzaxpd))
  # salsaxpd='number of times per day eat salsa'
  salsaxpd <- rep(NA,nrow(data))
  salsaxpd[data$Dsqsalsa==1] <- 0
  salsaxpd[data$Dsqsalsa==2] <- 0.033
  salsaxpd[data$Dsqsalsa==3] <- 0.083
  salsaxpd[data$Dsqsalsa==4] <- 0.143
  salsaxpd[data$Dsqsalsa==5] <- 0.286
  salsaxpd[data$Dsqsalsa==6] <- 0.5
  salsaxpd[data$Dsqsalsa==7] <- 0.786
  salsaxpd[data$Dsqsalsa==8] <- 1
  salsaxpd[data$Dsqsalsa==9] <- 2
  summary(factor(salsaxpd))
  # tomscxpd='number of times per day eat tomtato sauce'
  tomscxpd <- rep(NA,nrow(data))
  tomscxpd[data$Dsqsauce==1] <- 0
  tomscxpd[data$Dsqsauce==2] <- 0.033
  tomscxpd[data$Dsqsauce==3] <- 0.083
  tomscxpd[data$Dsqsauce==4] <- 0.143
  tomscxpd[data$Dsqsauce==5] <- 0.286
  tomscxpd[data$Dsqsauce==6] <- 0.5
  tomscxpd[data$Dsqsauce==7] <- 0.786
  tomscxpd[data$Dsqsauce==8] <- 1
  tomscxpd[data$Dsqsauce==9] <- 2
  summary(factor(tomscxpd))
  # redmtxpd='number of times per day eat red meat'
  redmtxpd <- rep(NA,nrow(data))
  redmtxpd[data$Dsqmeat==1] <- 0
  redmtxpd[data$Dsqmeat==2] <- 0.033
  redmtxpd[data$Dsqmeat==3] <- 0.083
  redmtxpd[data$Dsqmeat==4] <- 0.143
  redmtxpd[data$Dsqmeat==5] <- 0.286
  redmtxpd[data$Dsqmeat==6] <- 0.5
  redmtxpd[data$Dsqmeat==7] <- 0.786
  redmtxpd[data$Dsqmeat==8] <- 1
  redmtxpd[data$Dsqmeat==9] <- 2
  summary(factor(redmtxpd))
  # procmtxpd='number of times per day eat processed meat'
  procmtxpd <- rep(NA,nrow(data))
  procmtxpd[data$Dsqproc==1] <- 0
  procmtxpd[data$Dsqproc==2] <- 0.033
  procmtxpd[data$Dsqproc==3] <- 0.083
  procmtxpd[data$Dsqproc==4] <- 0.143
  procmtxpd[data$Dsqproc==5] <- 0.286
  procmtxpd[data$Dsqproc==6] <- 0.5
  procmtxpd[data$Dsqproc==7] <- 0.786
  procmtxpd[data$Dsqproc==8] <- 1
  procmtxpd[data$Dsqproc==9] <- 2
  summary(factor(procmtxpd))
  # cheesexpd='number of times per day eat cheese'
  cheesexpd <- rep(NA,nrow(data))
  cheesexpd[data$Dsqcheese==1] <- 0
  cheesexpd[data$Dsqcheese==2] <- 0.033
  cheesexpd[data$Dsqcheese==3] <- 0.083
  cheesexpd[data$Dsqcheese==4] <- 0.143
  cheesexpd[data$Dsqcheese==5] <- 0.286
  cheesexpd[data$Dsqcheese==6] <- 0.5
  cheesexpd[data$Dsqcheese==7] <- 0.786
  cheesexpd[data$Dsqcheese==8] <- 1
  cheesexpd[data$Dsqcheese==9] <- 2
  summary(factor(cheesexpd))
  # whgbrdxpd='number of times per day eat whole grain bread'
  whgbrdxpd <- rep(NA,nrow(data))
  whgbrdxpd[data$Dsqbread==1] <- 0
  whgbrdxpd[data$Dsqbread==2] <- 0.033
  whgbrdxpd[data$Dsqbread==3] <- 0.083
  whgbrdxpd[data$Dsqbread==4] <- 0.143
  whgbrdxpd[data$Dsqbread==5] <- 0.286
  whgbrdxpd[data$Dsqbread==6] <- 0.5
  whgbrdxpd[data$Dsqbread==7] <- 0.786
  whgbrdxpd[data$Dsqbread==8] <- 1
  whgbrdxpd[data$Dsqbread==9] <- 2
  summary(factor(whgbrdxpd))
  # brricexpd='number of times per day eat cooked whole grain (brown rice)'
  brricexpd <- rep(NA,nrow(data))
  brricexpd[data$Dsqgrain==1] <- 0
  brricexpd[data$Dsqgrain==2] <- 0.033
  brricexpd[data$Dsqgrain==3] <- 0.083
  brricexpd[data$Dsqgrain==4] <- 0.143
  brricexpd[data$Dsqgrain==5] <- 0.286
  brricexpd[data$Dsqgrain==6] <- 0.5
  brricexpd[data$Dsqgrain==7] <- 0.786
  brricexpd[data$Dsqgrain==8] <- 1
  brricexpd[data$Dsqgrain==9] <- 2
  summary(factor(brricexpd))
  # candyxpd='number of times per day eat candy'
  candyxpd <- rep(NA,nrow(data))
  candyxpd[data$Dsqcandy==1] <- 0
  candyxpd[data$Dsqcandy==2] <- 0.033
  candyxpd[data$Dsqcandy==3] <- 0.083
  candyxpd[data$Dsqcandy==4] <- 0.143
  candyxpd[data$Dsqcandy==5] <- 0.286
  candyxpd[data$Dsqcandy==6] <- 0.5
  candyxpd[data$Dsqcandy==7] <- 0.786
  candyxpd[data$Dsqcandy==8] <- 1
  candyxpd[data$Dsqcandy==9] <- 2
  summary(factor(candyxpd))
  # donutxpd='number of times per day eat pastries'
  donutxpd <- rep(NA,nrow(data))
  donutxpd[data$Dsqrolls==1] <- 0
  donutxpd[data$Dsqrolls==2] <- 0.033
  donutxpd[data$Dsqrolls==3] <- 0.083
  donutxpd[data$Dsqrolls==4] <- 0.143
  donutxpd[data$Dsqrolls==5] <- 0.286
  donutxpd[data$Dsqrolls==6] <- 0.5
  donutxpd[data$Dsqrolls==7] <- 0.786
  donutxpd[data$Dsqrolls==8] <- 1
  donutxpd[data$Dsqrolls==9] <- 2
  summary(factor(donutxpd))
  # cakexpd='number of times per day eat cookies/cake'
  cakexpd <- rep(NA,nrow(data))
  cakexpd[data$Dsqcake==1] <- 0
  cakexpd[data$Dsqcake==2] <- 0.033
  cakexpd[data$Dsqcake==3] <- 0.083
  cakexpd[data$Dsqcake==4] <- 0.143
  cakexpd[data$Dsqcake==5] <- 0.286
  cakexpd[data$Dsqcake==6] <- 0.5
  cakexpd[data$Dsqcake==7] <- 0.786
  cakexpd[data$Dsqcake==8] <- 1
  cakexpd[data$Dsqcake==9] <- 2
  summary(factor(cakexpd))
  # icecrmxpd='number of times per day eat ice cream'
  icecrmxpd <- rep(NA,nrow(data))
  icecrmxpd[data$Dsqice==1] <- 0
  icecrmxpd[data$Dsqice==2] <- 0.033
  icecrmxpd[data$Dsqice==3] <- 0.083
  icecrmxpd[data$Dsqice==4] <- 0.143
  icecrmxpd[data$Dsqice==5] <- 0.286
  icecrmxpd[data$Dsqice==6] <- 0.5
  icecrmxpd[data$Dsqice==7] <- 0.786
  icecrmxpd[data$Dsqice==8] <- 1
  icecrmxpd[data$Dsqice==9] <- 2
  summary(factor(icecrmxpd))
  # popcornxpd='number of times per day eat pop corn';
  popcornxpd <- rep(NA,nrow(data))
  popcornxpd[data$Dsqcorn==1] <- 0
  popcornxpd[data$Dsqcorn==2] <- 0.033
  popcornxpd[data$Dsqcorn==3] <- 0.083
  popcornxpd[data$Dsqcorn==4] <- 0.143
  popcornxpd[data$Dsqcorn==5] <- 0.286
  popcornxpd[data$Dsqcorn==6] <- 0.5
  popcornxpd[data$Dsqcorn==7] <- 0.786
  popcornxpd[data$Dsqcorn==8] <- 1
  popcornxpd[data$Dsqcorn==9] <- 2
  summary(factor(popcornxpd))
  
  c1hotcold <- rep(NA,nrow(data))
  c1whgnt <- rep(NA,nrow(data))
  c1sugnt <- rep(NA,nrow(data))
  c1calcnt <- rep(NA,nrow(data))
  c1fibnt <- rep(NA,nrow(data))
  c2hotcold <- rep(NA,nrow(data))
  c2whgnt <- rep(NA,nrow(data))
  c2sugnt <- rep(NA,nrow(data))
  c2calcnt <- rep(NA,nrow(data))
  c2fibnt <- rep(NA,nrow(data))
  dtdcer <- rep(NA,nrow(data))
  dtdcer1 <- rep(NA,nrow(data))
  dtdcer2 <- rep(NA,nrow(data))
  
  
  data$Dsqcertyp1 <- as.character(data$Dsqcertyp1)
  data$Dsqcertyp2 <- as.character(data$Dsqcertyp2)
  
  for(k in 1:nrow(data)){
    x <- data$Dsqcertyp1[k]
    z <- data$Dsqcertyp2[k]
    if((is.na(x) | x=="") & (!is.na(z) & z!="")){
      y <- x
      x <- z
      z <- y
    }
    if(is.na(x) | x==""){dtdcer1[k] <- 0}
    if(!is.na(x) & x!=""){dtdcer1[k] <- 1}
    dtdcer2[k] <- length(z[!is.na(z) & z!=""])
    dtdcer[k] <- dtdcer1[k] + dtdcer2[k]
  }
  if(sum(dtdcer1==0 & dtdcer2>=1)>0){
    print("Warn: check your skip logic. Students can answer Dsqcertyp2 without answering Dsqcertyp1")
  } #to make sure here should be 0
  
  hwg1f <- rep(NA,nrow(data))  
  hwg2f <- rep(NA,nrow(data))  
  cwg1f <- rep(NA,nrow(data))  
  cwg2f <- rep(NA,nrow(data))  
  cwg3f <- rep(NA,nrow(data))  
  cwg4f <- rep(NA,nrow(data)) 
  has1f <- rep(NA,nrow(data))  
  has2f <- rep(NA,nrow(data))  
  cas1f <- rep(NA,nrow(data))  
  cas2f <- rep(NA,nrow(data))  
  cas3f <- rep(NA,nrow(data))  
  cas4f <- rep(NA,nrow(data)) 
  hcm1f <- rep(NA,nrow(data))  
  hcm2f <- rep(NA,nrow(data))  
  hcm3f <- rep(NA,nrow(data))  
  ccm1f <- rep(NA,nrow(data))  
  ccm2f <- rep(NA,nrow(data))  
  ccm3f <- rep(NA,nrow(data))  
  ccm4f <- rep(NA,nrow(data)) 
  hfb1f <- rep(NA,nrow(data))  
  hfb2f <- rep(NA,nrow(data))  
  hfb3f <- rep(NA,nrow(data))  
  cfb1f <- rep(NA,nrow(data))  
  cfb2f <- rep(NA,nrow(data))  
  cfb3f <- rep(NA,nrow(data))  
  cfb4f <- rep(NA,nrow(data)) 
  
  for(k in 1:nrow(data)){
    if((dtdcer[k] %in% c(0,1,2)) & !is.na(hccerxpd[k])){
      hwg1f[k] <- 0  
      hwg2f[k] <- 0  
      cwg1f[k] <- 0  
      cwg2f[k] <- 0  
      cwg3f[k] <- 0  
      cwg4f[k] <- 0 
      has1f[k] <- 0  
      has2f[k] <- 0  
      cas1f[k] <- 0  
      cas2f[k] <- 0  
      cas3f[k] <- 0  
      cas4f[k] <- 0 
      hcm1f[k] <- 0  
      hcm2f[k] <- 0  
      hcm3f[k] <- 0  
      ccm1f[k] <- 0  
      ccm2f[k] <- 0  
      ccm3f[k] <- 0  
      ccm4f[k] <- 0 
      hfb1f[k] <- 0  
      hfb2f[k] <- 0  
      hfb3f[k] <- 0  
      cfb1f[k] <- 0  
      cfb2f[k] <- 0  
      cfb3f[k] <- 0  
      cfb4f[k] <- 0 
    }
    if(dtdcer[k]==1 & (data$Dsqcertyp1[k] %in% Dsqcertyp$Number)){
      x <- Dsqcertyp$Name[Dsqcertyp$Number==data$Dsqcertyp1[k]]
      c1hotcold <- Cereal$hotcold[Cereal$Cereal_Name==x]
      c1whgnt <- Cereal$whgnt[Cereal$Cereal_Name==x]
      c1sugnt <- Cereal$sugnt[Cereal$Cereal_Name==x]
      c1calcnt <- Cereal$calcnt[Cereal$Cereal_Name==x]
      c1fibnt <- Cereal$fibnt[Cereal$Cereal_Name==x]
      
      if(c1hotcold==2 & c1whgnt==1){cwg1f[k] <- cwg1f[k] + hccerxpd[k]}
      if(c1hotcold==2 & c1whgnt==2){cwg2f[k] <- cwg2f[k] + hccerxpd[k]}
      if(c1hotcold==2 & c1whgnt==3){cwg3f[k] <- cwg3f[k] + hccerxpd[k]}
      if(c1hotcold==2 & c1whgnt==4){cwg4f[k] <- cwg4f[k] + hccerxpd[k]}
      if(c1hotcold==1 & c1whgnt==1){hwg1f[k] <- hwg1f[k] + hccerxpd[k]}
      if(c1hotcold==1 & c1whgnt==2){hwg2f[k] <- hwg2f[k] + hccerxpd[k]}
      
      if(c1hotcold==2 & c1sugnt==1){cas1f[k] <- cas1f[k] + hccerxpd[k]}
      if(c1hotcold==2 & c1sugnt==2){cas2f[k] <- cas2f[k] + hccerxpd[k]}
      if(c1hotcold==2 & c1sugnt==3){cas3f[k] <- cas3f[k] + hccerxpd[k]}
      if(c1hotcold==2 & c1sugnt==4){cas4f[k] <- cas4f[k] + hccerxpd[k]}
      if(c1hotcold==1 & c1sugnt==1){has1f[k] <- has1f[k] + hccerxpd[k]}
      if(c1hotcold==1 & c1sugnt==2){has2f[k] <- has2f[k] + hccerxpd[k]}
      
      if(c1hotcold==2 & c1calcnt==1){ccm1f[k] <- ccm1f[k] + hccerxpd[k]}
      if(c1hotcold==2 & c1calcnt==2){ccm2f[k] <- ccm2f[k] + hccerxpd[k]}
      if(c1hotcold==2 & c1calcnt==3){ccm3f[k] <- ccm3f[k] + hccerxpd[k]}
      if(c1hotcold==2 & c1calcnt==4){ccm4f[k] <- ccm4f[k] + hccerxpd[k]}
      if(c1hotcold==1 & c1calcnt==1){hcm1f[k] <- hcm1f[k] + hccerxpd[k]}
      if(c1hotcold==1 & c1calcnt==2){hcm2f[k] <- hcm2f[k] + hccerxpd[k]}
      if(c1hotcold==1 & c1calcnt==3){hcm3f[k] <- hcm3f[k] + hccerxpd[k]}
      
      if(c1hotcold==2 & c1fibnt==1){cfb1f[k] <- cfb1f[k] + hccerxpd[k]}
      if(c1hotcold==2 & c1fibnt==2){cfb2f[k] <- cfb2f[k] + hccerxpd[k]}
      if(c1hotcold==2 & c1fibnt==3){cfb3f[k] <- cfb3f[k] + hccerxpd[k]}
      if(c1hotcold==2 & c1fibnt==4){cfb4f[k] <- cfb4f[k] + hccerxpd[k]}
      if(c1hotcold==1 & c1fibnt==1){hfb1f[k] <- hfb1f[k] + hccerxpd[k]}
      if(c1hotcold==1 & c1fibnt==2){hfb2f[k] <- hfb2f[k] + hccerxpd[k]}
      if(c1hotcold==1 & c1fibnt==3){hfb3f[k] <- hfb3f[k] + hccerxpd[k]}
    }
    if(dtdcer[k]==2){
      z <- strsplit(as.character(data$Dsqcertyp2[k]),",")[[1]][1]
      if((data$Dsqcertyp1[k] %in% Dsqcertyp$Number) & (z %in% Dsqcertyp$Number)){
        x <- Dsqcertyp$Name[Dsqcertyp$Number==data$Dsqcertyp1[k]]
        c1hotcold <- Cereal$hotcold[Cereal$Cereal_Name==x]
        c1whgnt <- Cereal$whgnt[Cereal$Cereal_Name==x]
        c1sugnt <- Cereal$sugnt[Cereal$Cereal_Name==x]
        c1calcnt <- Cereal$calcnt[Cereal$Cereal_Name==x]
        c1fibnt <- Cereal$fibnt[Cereal$Cereal_Name==x]
        
        
        s <- Dsqcertyp$Name[Dsqcertyp$Number==z]
        c2hotcold <- unique(Cereal$hotcold[Cereal$Cereal_Name==s])
        c2whgnt <- unique(Cereal$whgnt[Cereal$Cereal_Name==s])
        c2sugnt <- unique(Cereal$sugnt[Cereal$Cereal_Name==s])
        c2calcnt <- unique(Cereal$calcnt[Cereal$Cereal_Name==s])
        c2fibnt <- unique(Cereal$fibnt[Cereal$Cereal_Name==s])
        
        if((c1hotcold==2) & (c1whgnt==1)){cwg1f[k] <- cwg1f[k] + 0.75*hccerxpd[k]}
        if((c1hotcold==2) & (c1whgnt==2)){cwg2f[k] <- cwg2f[k] + 0.75*hccerxpd[k]}
        if((c1hotcold==2) & (c1whgnt==3)){cwg3f[k] <- cwg3f[k] + 0.75*hccerxpd[k]}
        if((c1hotcold==2) & (c1whgnt==4)){cwg4f[k] <- cwg4f[k] + 0.75*hccerxpd[k]}
        if((c1hotcold==1) & (c1whgnt==1)){hwg1f[k] <- hwg1f[k] + 0.75*hccerxpd[k]}
        if((c1hotcold==1) & (c1whgnt==2)){hwg2f[k] <- hwg2f[k] + 0.75*hccerxpd[k]}
        
        if((c2hotcold==2) & (c2whgnt==1)){cwg1f[k] <- cwg1f[k] + 0.25*hccerxpd[k]}
        if((c2hotcold==2) & (c2whgnt==2)){cwg2f[k] <- cwg2f[k] + 0.25*hccerxpd[k]}
        if((c2hotcold==2) & (c2whgnt==3)){cwg3f[k] <- cwg3f[k] + 0.25*hccerxpd[k]}
        if((c2hotcold==2) & (c2whgnt==4)){cwg4f[k] <- cwg4f[k] + 0.25*hccerxpd[k]}
        if((c2hotcold==1) & (c2whgnt==1)){hwg1f[k] <- hwg1f[k] + 0.25*hccerxpd[k]}
        if((c2hotcold==1) & (c2whgnt==2)){hwg2f[k] <- hwg2f[k] + 0.25*hccerxpd[k]}
        
        if((c1hotcold==2) & (c1sugnt==1)){cas1f[k] <- cas1f[k] + 0.75*hccerxpd[k]}
        if((c1hotcold==2) & (c1sugnt==2)){cas2f[k] <- cas2f[k] + 0.75*hccerxpd[k]}
        if((c1hotcold==2) & (c1sugnt==3)){cas3f[k] <- cas3f[k] + 0.75*hccerxpd[k]}
        if((c1hotcold==2) & (c1sugnt==4)){cas4f[k] <- cas4f[k] + 0.75*hccerxpd[k]}
        if((c1hotcold==1) & (c1sugnt==1)){has1f[k] <- has1f[k] + 0.75*hccerxpd[k]}
        if((c1hotcold==1) & (c1sugnt==2)){has2f[k] <- has2f[k] + 0.75*hccerxpd[k]}
        
        if((c2hotcold==2) & (c2sugnt==1)){cas1f[k] <- cas1f[k] + 0.25*hccerxpd[k]}
        if((c2hotcold==2) & (c2sugnt==2)){cas2f[k] <- cas2f[k] + 0.25*hccerxpd[k]}
        if((c2hotcold==2) & (c2sugnt==3)){cas3f[k] <- cas3f[k] + 0.25*hccerxpd[k]}
        if((c2hotcold==2) & (c2sugnt==4)){cas4f[k] <- cas4f[k] + 0.25*hccerxpd[k]}
        if((c2hotcold==1) & (c2sugnt==1)){has1f[k] <- has1f[k] + 0.25*hccerxpd[k]}
        if((c2hotcold==1) & (c2sugnt==2)){has2f[k] <- has2f[k] + 0.25*hccerxpd[k]}
        
        
        
        if((c1hotcold==2) & (c1calcnt==1)){ccm1f[k] <- ccm1f[k] + 0.75*hccerxpd[k]}
        if((c1hotcold==2) & (c1calcnt==2)){ccm2f[k] <- ccm2f[k] + 0.75*hccerxpd[k]}
        if((c1hotcold==2) & (c1calcnt==3)){ccm3f[k] <- ccm3f[k] + 0.75*hccerxpd[k]}
        if((c1hotcold==2) & (c1calcnt==4)){ccm4f[k] <- ccm4f[k] + 0.75*hccerxpd[k]}
        if((c1hotcold==1) & (c1calcnt==1)){hcm1f[k] <- hcm1f[k] + 0.75*hccerxpd[k]}
        if((c1hotcold==1) & (c1calcnt==2)){hcm2f[k] <- hcm2f[k] + 0.75*hccerxpd[k]}
        if((c1hotcold==1) & (c1calcnt==3)){hcm3f[k] <- hcm3f[k] + 0.75*hccerxpd[k]}
        
        if((c2hotcold==2) & (c2calcnt==1)){ccm1f[k] <- ccm1f[k] + 0.25*hccerxpd[k]}
        if((c2hotcold==2) & (c2calcnt==2)){ccm2f[k] <- ccm2f[k] + 0.25*hccerxpd[k]}
        if((c2hotcold==2) & (c2calcnt==3)){ccm3f[k] <- ccm3f[k] + 0.25*hccerxpd[k]}
        if((c2hotcold==2) & (c2calcnt==4)){ccm4f[k] <- ccm4f[k] + 0.25*hccerxpd[k]}
        if((c2hotcold==1) & (c2calcnt==1)){hcm1f[k] <- hcm1f[k] + 0.25*hccerxpd[k]}
        if((c2hotcold==1) & (c2calcnt==2)){hcm2f[k] <- hcm2f[k] + 0.25*hccerxpd[k]}
        if((c2hotcold==1) & (c2calcnt==3)){hcm3f[k] <- hcm3f[k] + 0.25*hccerxpd[k]}
        
        
        
        if((c1hotcold==2) & (c1fibnt==1)){cfb1f[k] <- cfb1f[k] + 0.75*hccerxpd[k]}
        if((c1hotcold==2) & (c1fibnt==2)){cfb2f[k] <- cfb2f[k] + 0.75*hccerxpd[k]}
        if((c1hotcold==2) & (c1fibnt==3)){cfb3f[k] <- cfb3f[k] + 0.75*hccerxpd[k]}
        if((c1hotcold==2) & (c1fibnt==4)){cfb4f[k] <- cfb4f[k] + 0.75*hccerxpd[k]}
        if((c1hotcold==1) & (c1fibnt==1)){hfb1f[k] <- hfb1f[k] + 0.75*hccerxpd[k]}
        if((c1hotcold==1) & (c1fibnt==2)){hfb2f[k] <- hfb2f[k] + 0.75*hccerxpd[k]}
        if((c1hotcold==1) & (c1fibnt==3)){hfb3f[k] <- hfb3f[k] + 0.75*hccerxpd[k]}
        
        if((c2hotcold==2) & (c2fibnt==1)){cfb1f[k] <- cfb1f[k] + 0.25*hccerxpd[k]}
        if((c2hotcold==2) & (c2fibnt==2)){cfb2f[k] <- cfb2f[k] + 0.25*hccerxpd[k]}
        if((c2hotcold==2) & (c2fibnt==3)){cfb3f[k] <- cfb3f[k] + 0.25*hccerxpd[k]}
        if((c2hotcold==2) & (c2fibnt==4)){cfb4f[k] <- cfb4f[k] + 0.25*hccerxpd[k]}
        if((c2hotcold==1) & (c2fibnt==1)){hfb1f[k] <- hfb1f[k] + 0.25*hccerxpd[k]}
        if((c2hotcold==1) & (c2fibnt==2)){hfb2f[k] <- hfb2f[k] + 0.25*hccerxpd[k]}
        if((c2hotcold==1) & (c2fibnt==3)){hfb3f[k] <- hfb3f[k] + 0.25*hccerxpd[k]}
      }
    }
  }
  
  ghcerfib1 <- rep(NA,nrow(data))
  ghcerfib2 <- rep(NA,nrow(data))
  ghcerfib3 <- rep(NA,nrow(data))
  gccerfib1 <- rep(NA,nrow(data))
  gccerfib2 <- rep(NA,nrow(data))
  gccerfib3 <- rep(NA,nrow(data))
  gccerfib4 <- rep(NA,nrow(data))
  gwhgbrd <- rep(NA,nrow(data))
  gpopcorn <- rep(NA,nrow(data))
  gbrownrice <- rep(NA,nrow(data))
  gcheese <- rep(NA,nrow(data))
  gpizza <- rep(NA,nrow(data))
  gmilk <- rep(NA,nrow(data))
  gicecream <- rep(NA,nrow(data))
  gregsoda <- rep(NA,nrow(data))
  gsportfrdrnk <- rep(NA,nrow(data))
  gcookietc <- rep(NA,nrow(data))
  gdonut <- rep(NA,nrow(data))
  gsugincof <- rep(NA,nrow(data))
  gcandy <- rep(NA,nrow(data))
  gfrtj <- rep(NA,nrow(data))
  gfruit <- rep(NA,nrow(data))
  gsalad <- rep(NA,nrow(data))
  gothpot <- rep(NA,nrow(data))
  gdrbean <- rep(NA,nrow(data))
  gothveg <- rep(NA,nrow(data))
  gfrfry <- rep(NA,nrow(data))
  gtomsc <- rep(NA,nrow(data))
  gsalsa <- rep(NA,nrow(data))
  ghcercalc1 <- rep(NA,nrow(data))
  ghcercalc2 <- rep(NA,nrow(data))
  ghcercalc3 <- rep(NA,nrow(data))
  gccercalc1 <- rep(NA,nrow(data))
  gccercalc2 <- rep(NA,nrow(data))
  gccercalc3 <- rep(NA,nrow(data))
  gccercalc4 <- rep(NA,nrow(data))
  shceraddsug1 <- rep(NA,nrow(data))
  shceraddsug2 <- rep(NA,nrow(data))
  scceraddsug1 <- rep(NA,nrow(data))
  scceraddsug2 <- rep(NA,nrow(data))
  scceraddsug3 <- rep(NA,nrow(data))
  scceraddsug4 <- rep(NA,nrow(data))
  sicecream <- rep(NA,nrow(data))
  sregsoda <- rep(NA,nrow(data))
  ssportfrdrnk <- rep(NA,nrow(data))
  scookietc <- rep(NA,nrow(data))
  sdonut <- rep(NA,nrow(data))
  ssugincof <- rep(NA,nrow(data))
  scandy <- rep(NA,nrow(data))
  ghcerwg1 <- rep(NA,nrow(data))
  ghcerwg2 <- rep(NA,nrow(data))
  gccerwg1 <- rep(NA,nrow(data))
  gccerwg2 <- rep(NA,nrow(data))
  gccerwg3 <- rep(NA,nrow(data))
  gccerwg4 <- rep(NA,nrow(data))
  dcheese <- rep(NA,nrow(data))
  dpizza <- rep(NA,nrow(data))
  dmilk <- rep(NA,nrow(data))
  dicecream <- rep(NA,nrow(data))
  vfrtj <- rep(NA,nrow(data))
  vfruit <- rep(NA,nrow(data))
  vsalad <- rep(NA,nrow(data))
  vothpot <- rep(NA,nrow(data))
  vdrbean <- rep(NA,nrow(data))
  vothveg <- rep(NA,nrow(data))
  vfrfry <- rep(NA,nrow(data))
  vtomsc <- rep(NA,nrow(data))
  vsalsa <- rep(NA,nrow(data))
  vpizza <- rep(NA,nrow(data))
  fd8sug <- rep(NA,nrow(data))
  fd7sug <- rep(NA,nrow(data))
  btfd8sug <- rep(NA,nrow(data))
  btfd7sug <- rep(NA,nrow(data))
  fd10fvl <- rep(NA,nrow(data))
  fd9fvl <- rep(NA,nrow(data))
  sqrtfd10fvl <- rep(NA,nrow(data))
  sqrtfd9fvl <- rep(NA,nrow(data))
  fd3ssb <- rep(NA,nrow(data))
  sqrtfd3ssb <- rep(NA,nrow(data))
  fd4dairy <- rep(NA,nrow(data))
  sqfd4dairy <- rep(NA,nrow(data))
  
  predfib <- rep(NA,nrow(data))
  predcalc <- rep(NA,nrow(data))
  predsug <- rep(NA,nrow(data))
  predsugnc <- rep(NA,nrow(data))
  predwhgrn <- rep(NA,nrow(data))
  preddairy <- rep(NA,nrow(data))
  predfvl <- rep(NA,nrow(data))
  predfvlnf <- rep(NA,nrow(data))
  predssb <- rep(NA,nrow(data))
  
  for(k in 1:nrow(data)){
    x <- data$gender[k]
    AGEGRP <- NA
    agecut <- NA
    if(!is.na(x) & x %in% c(1,2) & !is.na(data$AGE[k])){
      
      AGEGRP <- data$AGEGRP[k]
      agecut <- data$agecut[k]
      
      for(j in 3:ncol(PortionSize)){
        assign(names(PortionSize)[j],NA)
        assign(names(PortionSize)[j],
               PortionSize[PortionSize$gender==x & PortionSize$AGEGRP==AGEGRP,j])
      }
      for(j in 3:ncol(Coefficients)){
        assign(tolower(names(Coefficients)[j]),NA)
        assign(tolower(names(Coefficients)[j]),
               Coefficients[Coefficients$gender==x & Coefficients$agecut==agecut,j])
      }
      ghcerfib1[k] <- hfb1f[k] * gp50hcereal
      ghcerfib2[k] <- hfb2f[k] * gp50hcereal
      ghcerfib3[k] <- hfb3f[k] * gp50hcereal
      gccerfib1[k] <- cfb1f[k] * gp50ccereal
      gccerfib2[k] <- cfb2f[k] * gp50ccereal
      gccerfib3[k] <- cfb3f[k] * gp50ccereal
      gccerfib4[k] <- cfb4f[k] * gp50ccereal
      gwhgbrd[k] <- whgbrdxpd[k] * gp50whgrd
      gpopcorn[k] <- popcornxpd[k] * gp50popcorn
      gbrownrice[k] <- brricexpd[k] * gp50brownrice
      gcheese[k] <- cheesexpd[k] * gp50cheese
      gpizza[k] <- pizzaxpd[k] * gp50pizza
      gmilk[k] <- milkxpd[k] * gp50milk
      gicecream[k] <- icecrmxpd[k] * gp50icecream
      gregsoda[k] <- sodaxpd[k] * gp50regsoda
      gsportfrdrnk[k] <- energyxpd[k] * gp50sportfrdrnk
      gcookietc[k] <- cakexpd[k] * gp50cookietc
      gdonut[k] <- donutxpd[k] * gp50donut
      gsugincof[k] <- swtctxpd[k] * gp50sugincof
      gcandy[k] <- candyxpd[k] * gp50candy
      gfrtj[k] <- frtjcxpd[k] * gp50frtj
      gfruit[k] <- fruitxpd[k] * gp50fruit
      gsalad[k] <- saladxpd[k] * gp50salad
      gothpot[k] <- othpotxpd[k] * gp50othpot
      gdrbean[k] <- beanxpd[k] * gp50drbean
      gothveg[k] <- othvegxpd[k] * gp50othveg
      gfrfry[k] <- frfryxpd[k] * gp50frfry
      gtomsc[k] <- tomscxpd[k] * gp50tomsc
      gsalsa[k] <- salsaxpd[k] * gp50salsa  
      
      ghcercalc1[k] <- hcm1f[k] * gp50hcereal
      ghcercalc2[k] <- hcm2f[k] * gp50hcereal
      ghcercalc3[k] <- hcm3f[k] * gp50hcereal
      gccercalc1[k] <- ccm1f[k] * gp50ccereal
      gccercalc2[k] <- ccm2f[k] * gp50ccereal
      gccercalc3[k] <- ccm3f[k] * gp50ccereal
      gccercalc4[k] <- ccm4f[k] * gp50ccereal
      
      shceraddsug1[k] <- has1f[k] * sp50hcereal
      shceraddsug2[k] <- has2f[k] * sp50hcereal
      scceraddsug1[k] <- cas1f[k] * sp50ccereal
      scceraddsug2[k] <- cas2f[k] * sp50ccereal
      scceraddsug3[k] <- cas3f[k] * sp50ccereal
      scceraddsug4[k] <- cas4f[k] * sp50ccereal
      sicecream[k] <- icecrmxpd[k] * sp50icecream
      sregsoda[k] <- sodaxpd[k] * sp50regsoda
      ssportfrdrnk[k] <- energyxpd[k] * sp50sportfrdrnk
      scookietc[k] <- cakexpd[k] * sp50cookietc
      sdonut[k] <- donutxpd[k] * sp50donut
      ssugincof[k] <- swtctxpd[k] * sp50sugincof
      scandy[k] <- candyxpd[k] * sp50candy  
      
      ghcerwg1[k] <- hwg1f[k] * gp50hcereal
      ghcerwg2[k] <- hwg2f[k] * gp50hcereal
      gccerwg1[k] <- cwg1f[k] * gp50ccereal
      gccerwg2[k] <- cwg2f[k] * gp50ccereal
      gccerwg3[k] <- cwg3f[k] * gp50ccereal
      gccerwg4[k] <- cwg4f[k] * gp50ccereal
      
      dcheese[k] <- cheesexpd[k] * dp50cheese
      dpizza[k] <- pizzaxpd[k] * dp50pizza
      dmilk[k] <- milkxpd[k] * dp50milk
      dicecream[k] <- icecrmxpd[k] * dp50icecream
      
      vfrtj[k] <- frtjcxpd[k] * vp50frtj
      vfruit[k] <- fruitxpd[k] * vp50fruit
      vsalad[k] <- saladxpd[k] * vp50salad
      vothpot[k] <- othpotxpd[k] * vp50othpot
      vdrbean[k] <- beanxpd[k] * vp50drbean
      vothveg[k] <- othvegxpd[k] * vp50othveg
      vfrfry[k] <- frfryxpd[k] * vp50frfry
      vtomsc[k] <- tomscxpd[k] * vp50tomsc
      vsalsa[k] <- salsaxpd[k] * vp50salsa
      vpizza[k] <- pizzaxpd[k] * vp50pizza
      
      #sum variables then transform;
      fd8sug[k] <- shceraddsug1[k] + shceraddsug2[k] + scceraddsug1[k] + scceraddsug2[k] + scceraddsug3[k] + scceraddsug4[k] + sicecream[k] + 
        sregsoda[k] + ssportfrdrnk[k] + scookietc[k] + sdonut[k] + ssugincof[k] + scandy[k]
      fd7sug[k] <- sregsoda[k] + ssportfrdrnk[k] + scookietc[k] + sdonut[k] + sicecream[k] + ssugincof[k] + scandy[k]
      btfd8sug[k] <- (fd8sug[k])^(0.33)
      btfd7sug[k] <- (fd7sug[k])^(0.33)
      
      fd10fvl[k] <- vfrtj[k] + vfruit[k] + vsalad[k] + vothpot[k] + vdrbean[k] + vothveg[k] + vfrfry[k] + vtomsc[k] + vsalsa[k] + vpizza[k]
      fd9fvl[k] <- vfrtj[k] + vfruit[k] + vsalad[k] + vothpot[k] + vdrbean[k] + vothveg[k] + vtomsc[k] + vsalsa[k] + vpizza[k]
      sqrtfd10fvl[k] <- sqrt(fd10fvl[k])
      sqrtfd9fvl[k] <- sqrt(fd9fvl[k])
      
      fd3ssb[k] <- sregsoda[k] + ssportfrdrnk[k] + ssugincof[k]
      sqrtfd3ssb[k] <- sqrt(fd3ssb[k])
      
      fd4dairy[k] <- dcheese[k] + dpizza[k] + dmilk[k] + dicecream[k]
      sqfd4dairy[k] <- sqrt(fd4dairy[k])
      
      ###using regression coefficients###
      predfib[k] <- (bintercept +  (ghcerfib1[k] * bghcerfib1) + (ghcerfib2[k] * bghcerfib2) + (ghcerfib3[k] * bghcerfib3) + (gccerfib1[k] * bgccerfib1) +  
                       (gccerfib2[k] * bgccerfib2) +  (gccerfib3[k] * bgccerfib3) +  (gccerfib4[k] * bgccerfib4) +  (gwhgbrd[k] * bgwhgbrd) + (gbrownrice[k] * bgbrownrice) + 
                       (gcheese[k] * bgcheese) +  (gpizza[k] * bgpizza) +  (gmilk[k] * bgmilk) +  (gicecream[k] * bgicecream) +  (gpopcorn[k] * bgpopcorn) + 
                       (gregsoda[k] * bgregsoda) +  (gsportfrdrnk[k] * bgsportfrdrnk) +  (gcookietc[k] * bgcookietc) +  (gdonut[k] * bgdonut) +  (gsugincof[k] * bgsugincof) +  (gcandy[k] * bgcandy) + 
                       (gfrtj[k] * bgfrtj) +  (gfruit[k] * bgfruit)  +  (gsalad[k] * bgsalad) + (gothpot[k] * bgothpot) +  (gdrbean[k] * bgdrbean) +
                       (gothveg[k] * bgothveg) +  (gfrfry[k] * bgfrfry) +  (gtomsc[k] * bgtomsc) +  (gsalsa[k] * bgsalsa) ) ^ 4
      
      
      predcalc[k] <- (cintercept + (ghcercalc1[k] * cghcercalc1) + (ghcercalc2[k] * cghcercalc2) +  (ghcercalc3[k] * cghcercalc3) +  (gccercalc1[k] * cgccercalc1) +  
                        (gccercalc2[k] * cgccercalc2) +  (gccercalc3[k] * cgccercalc3) +  (gccercalc4[k] * cgccercalc4) +  (gwhgbrd[k] * cgwhgbrd) + (gbrownrice[k] * cgbrownrice) + 
                        (gcheese[k] * cgcheese) +  (gpizza[k] * cgpizza) +  (gmilk[k] * cgmilk) +  (gicecream[k] * cgicecream) + (gpopcorn[k] * cgpopcorn) +
                        (gregsoda[k] * cgregsoda) +  (gsportfrdrnk[k] * cgsportfrdrnk) +  (gcookietc[k] * cgcookietc) +  (gdonut[k] * cgdonut) +  (gsugincof[k] * cgsugincof) +  (gcandy[k] * cgcandy) + 
                        (gfrtj[k] * cgfrtj) +  (gfruit[k] * cgfruit) +  (gsalad[k] * cgsalad) +  (gothpot[k] * cgothpot) +  (gdrbean[k] * cgdrbean) +
                        (gothveg[k] * cgothveg) +  (gfrfry[k] * cgfrfry) +  (gtomsc[k] * cgtomsc) +  (gsalsa[k] * cgsalsa) ) ^ 4
      
      predsug[k] <- (wintercept + (wbeta1 * btfd8sug[k] ) )^3
      
      
      predsugnc[k] <- (sintercept + (sbeta1 * btfd7sug[k] ) )^3
      
      
      predwhgrn[k] <- (gintercept + (ghcerwg1[k] * gghcerwg1) + (ghcerwg2[k] * gghcerwg2) + (gccerwg1[k] * ggccerwg1) + (gccerwg2[k] * ggccerwg2)
                       + (gccerwg3[k] * ggccerwg3) + (gccerwg4[k] * ggccerwg4) + (gwhgbrd[k] * ggwhgbrd) + (gpopcorn[k] * ggpopcorn) + (gbrownrice[k] * ggbrownrice) ) ^2
      
      
      preddairy[k] <-  (dintercept + (dbeta1 * sqfd4dairy[k] ) )^2
      
      
      predfvl[k] <-  (vintercept + (vbeta1 * sqrtfd10fvl[k] ) )^2
      
      
      predfvlnf[k] <-  (nintercept + (nbeta1 * sqrtfd9fvl[k] ) )^2
      
      
      predssb[k] <-  (xintercept + (xbeta1 * sqrtfd3ssb[k]) ) ^2
    }
  }
  
  out <- data.frame(predfib = predfib,
                    predfib = predfib,
                    predcalc = predcalc,
                    predsug = predsug,
                    predsugnc = predsugnc,
                    predwhgrn = predwhgrn,
                    preddairy = preddairy,
                    predfvl = predfvl,
                    predfvlnf = predfvlnf,
                    predssb = predssb)
  return(out)
}
