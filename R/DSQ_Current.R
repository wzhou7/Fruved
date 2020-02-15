#trim <- function (x) gsub("^\\s+|\\s+$", "", x)
# library(stringr) => str_trim()

Score_DSQ_Current <- function(data){
  
  Dsqcertyp <- Dsqcertyp_Current
  
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
  
  # make sure cols are char
  data <- fixColClasses(data,all_needed_vars)
  
  # check possible values - data validation
  # only coded values shown in the data dictionary allowed (use Wave1 data dictionary)
  # if unexpecvted values exist, throw an error - information (which variables)
  
  ######################
  #### Demographics ####
  ######################
  
  gender <- data$Gender # need to ensure gender coding correct
  # AGE is an integer
  ageinyr <- round(as.numeric(data$Age)) # ensure there is no AGE=2.5
  
  ######################
  ####### numcer #######
  ######################
  dtdcer <- rep(NA,nrow(data))
  dtdcer1 <- rep(NA,nrow(data))
  dtdcer2 <- rep(NA,nrow(data))
  
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
  
  ###################
  #### Frequency ####
  ###################
  
  foodrange <- function(var,maxv){
    out <- rep(NA,nrow(data))
    out[data[,var]==1] <- 0
    out[data[,var]==2] <- 0.033
    out[data[,var]==3] <- 0.083
    out[data[,var]==4] <- 0.143
    out[data[,var]==5] <- 0.286
    out[data[,var]==6] <- 0.5
    out[data[,var]==7] <- 0.786
    out[data[,var]==8] <- 1
    out[data[,var]==9] <- 2
    out[out>maxv] <- maxv
    return(out)
  }
  
  bevrange <- function(var,maxv){
    out <- rep(NA,nrow(data))
    out[data[,var]==1] <- 0
    out[data[,var]==2] <- 0.033
    out[data[,var]==3] <- 0.083
    out[data[,var]==4] <- 0.143
    out[data[,var]==5] <- 0.286
    out[data[,var]==6] <- 0.5
    out[data[,var]==7] <- 0.786
    out[data[,var]==8] <- 1
    out[data[,var]==9] <- 2.5
    out[data[,var]==10] <- 4.5
    out[data[,var]==11] <- 6
    out[out>maxv] <- maxv
    return(out)
  }
  
  
  # hccerxpd='number of times per day eat hot or cold cereal'
  hccerxpd <- foodrange("Dsqcereal",7)
  
  # milkxpd='number of times per day drink milk'
  milkxpd <- bevrange("Dsqmilk1",10)
  
  # sodaxpd='number of times per day drink soda'
  sodaxpd <- bevrange("Dsqsoda",8)
  
  # frtjcxpd='number of times per day drink fruit juice'
  frtjcxpd <- bevrange("Dsqjuice",8)
  
  # swtctxpd='number of times per day drink sweet coffee/tea'
  swtctxpd <- bevrange("Dsqcoffee",10)
  
  # energyxpd='number of times per day drink fruit/sports/energy drink'
  energyxpd <- bevrange("Dsqdrink",7)
  
  # fruitxpd='number of times per day eat fruit'
  fruitxpd <- foodrange("Dsqfruit",8)
  
  # saladxpd='number of times per day eat salad'
  saladxpd <- foodrange("Dsqsalad",5)
  
  # frfryxpd='number of times per day eat fried potatoes'
  frfryxpd <- foodrange("Dsqfried",5)
  
  # othpotxpd='number of times per day eat other potatoes'
  othpotxpd <- foodrange("Dsqpotato",3)
  
  # beanxpd='number of times per day eat beans'
  beanxpd <- foodrange("Dsqbean",4)
  
  # othvegxpd='number of times per day eat other vegetables'
  othvegxpd <- foodrange("Dsqveg",5)
  
  # pizzaxpd='number of times per day eat pizza'
  pizzaxpd <- foodrange("Dsqpizza",2)
  
  # salsaxpd='number of times per day eat salsa'
  salsaxpd <- foodrange("Dsqsalsa",3)
  
  # tomscxpd='number of times per day eat tomtato sauce'
  tomscxpd <- foodrange("Dsqsauce",2)
  
  # cheesexpd='number of times per day eat cheese'
  cheesexpd <- foodrange("Dsqcheese",6)
  
  # whgbrdxpd='number of times per day eat whole grain bread'
  whgbrdxpd <- foodrange("Dsqbread",6)
  
  # brricexpd='number of times per day eat cooked whole grain (brown rice)'
  brricexpd <- foodrange("Dsqgrain",4)
  
  # candyxpd='number of times per day eat candy'
  candyxpd <- foodrange("Dsqcandy",8)
  
  # donutxpd='number of times per day eat pastries'
  donutxpd <- foodrange("Dsqrolls",5)
  
  # cakexpd='number of times per day eat cookies/cake'
  cakexpd <- foodrange("Dsqcake",7)
  
  # icecrmxpd='number of times per day eat ice cream'
  icecrmxpd <- foodrange("Dsqice",5)
  
  # popcornxpd='number of times per day eat pop corn';
  popcornxpd <- foodrange("Dsqcorn",3)
  
  ######################
  ######### Age ########
  ######################
  bcage <- rep(NA,nrow(data))
  bcage[ageinyr>=2 & ageinyr<=3] <- 1
  bcage[ageinyr>=4 & ageinyr<=5] <- 2
  bcage[ageinyr>=6 & ageinyr<=7] <- 3
  bcage[ageinyr>=8 & ageinyr<=9] <- 4
  bcage[ageinyr>=10 & ageinyr<=11] <- 5
  bcage[ageinyr>=12 & ageinyr<=13] <- 6
  bcage[ageinyr>=14 & ageinyr<=15] <- 7
  bcage[ageinyr>=16 & ageinyr<=17] <- 8
  bcage[ageinyr>=18 & ageinyr<=27] <- 9
  bcage[ageinyr>=26 & ageinyr<=35] <- 10
  bcage[ageinyr>=36 & ageinyr<=45] <- 11
  bcage[ageinyr>=46 & ageinyr<=60] <- 12
  bcage[ageinyr>=61 & ageinyr<=69] <- 13
  bcage[ageinyr>=70 & ageinyr<=99] <- 14
  
  kidgrp <- teengrp <- rep(NA,nrow(data))
  kidgrp[ageinyr>=2 & ageinyr<=11] <- 1
  kidgrp[ageinyr>11 | ageinyr<2] <- 0
  teengrp[ageinyr>=12 & ageinyr<=17] <- 1
  teengrp[ageinyr>17 | ageinyr<12] <- 0
  
  c1whgnt <- rep(NA,nrow(data))
  c1sugnt <- rep(NA,nrow(data))
  c1calcnt <- rep(NA,nrow(data))
  c1fibnt <- rep(NA,nrow(data))
  c2whgnt <- rep(NA,nrow(data))
  c2sugnt <- rep(NA,nrow(data))
  c2calcnt <- rep(NA,nrow(data))
  c2fibnt <- rep(NA,nrow(data))
  
  
  wg1f <- rep(NA,nrow(data))
  wg2f <- rep(NA,nrow(data))
  wg3f <- rep(NA,nrow(data))
  as1f <- rep(NA,nrow(data))
  as2f <- rep(NA,nrow(data))
  as3f <- rep(NA,nrow(data))
  cm1f <- rep(NA,nrow(data))
  cm2f <- rep(NA,nrow(data))
  cm3f <- rep(NA,nrow(data))
  fb1f <- rep(NA,nrow(data))
  fb2f <- rep(NA,nrow(data))
  fb3f <- rep(NA,nrow(data))
  
  for(k in 1:nrow(data)){
    if((dtdcer[k] %in% c(0,1,2)) & !is.na(hccerxpd[k]) & hccerxpd[k]!=""){
      wg1f[k] <- 0  
      wg2f[k] <- 0  
      wg3f[k] <- 0  
      as1f[k] <- 0  
      as2f[k] <- 0  
      as3f[k] <- 0 
      cm1f[k] <- 0  
      cm2f[k] <- 0  
      cm3f[k] <- 0  
      fb1f[k] <- 0  
      fb2f[k] <- 0  
      fb3f[k] <- 0
    }
    
    if(dtdcer[k]==1 & !(data$Dsqcertyp1[k] %in% Dsqcertyp$Number)){
      stop("Check the coded value of Dsqcertyp1")
    }
    
    if(dtdcer[k]==1 & (data$Dsqcertyp1[k] %in% Dsqcertyp$Number)){
      x <- Dsqcertyp$Name[Dsqcertyp$Number==data$Dsqcertyp1[k]]
      c1whgnt <- ntile$whgnt[ntile$Cereal_Name==x]
      c1sugnt <- ntile$sugnt[ntile$Cereal_Name==x]
      c1calcnt <- ntile$calcnt[ntile$Cereal_Name==x]
      c1fibnt <- ntile$fibnt[ntile$Cereal_Name==x]
      
      if(c1whgnt==1){wg1f[k] <- wg1f[k] + hccerxpd[k]}
      if(c1whgnt==2){wg2f[k] <- wg2f[k] + hccerxpd[k]}
      if(c1whgnt==3){wg3f[k] <- wg3f[k] + hccerxpd[k]}
      
      if(c1sugnt==1){as1f[k] <- as1f[k] + hccerxpd[k]}
      if(c1sugnt==2){as2f[k] <- as2f[k] + hccerxpd[k]}
      if(c1sugnt==3){as3f[k] <- as3f[k] + hccerxpd[k]}
      
      if(c1calcnt==1){cm1f[k] <- cm1f[k] + hccerxpd[k]}
      if(c1calcnt==2){cm2f[k] <- cm2f[k] + hccerxpd[k]}
      if(c1calcnt==3){cm3f[k] <- cm3f[k] + hccerxpd[k]}
      
      if(c1fibnt==1){fb1f[k] <- fb1f[k] + hccerxpd[k]}
      if(c1fibnt==2){fb2f[k] <- fb2f[k] + hccerxpd[k]}
      if(c1fibnt==3){fb3f[k] <- fb3f[k] + hccerxpd[k]}
    }
    if(dtdcer[k]==2){
      z <- strsplit(as.character(data$Dsqcertyp2[k]),",")[[1]][1]
      
      if(!(data$Dsqcertyp1[k] %in% Dsqcertyp$Number)){
        stop("Check the coded value of Dsqcertyp1")
      }
      
      if(!(z %in% Dsqcertyp$Number)){
        stop("Check the coded value of Dsqcertyp2")
      }
      
      if((data$Dsqcertyp1[k] %in% Dsqcertyp$Number) & (z %in% Dsqcertyp$Number)){
        x <- Dsqcertyp$Name[Dsqcertyp$Number==data$Dsqcertyp1[k]]
        c1whgnt <- ntile$whgnt[ntile$Cereal_Name==x]
        c1sugnt <- ntile$sugnt[ntile$Cereal_Name==x]
        c1calcnt <- ntile$calcnt[ntile$Cereal_Name==x]
        c1fibnt <- ntile$fibnt[ntile$Cereal_Name==x]
        
        
        s <- Dsqcertyp$Name[Dsqcertyp$Number==z]
        c2whgnt <- ntile$whgnt[ntile$Cereal_Name==s]
        c2sugnt <- ntile$sugnt[ntile$Cereal_Name==s]
        c2calcnt <- ntile$calcnt[ntile$Cereal_Name==s]
        c2fibnt <- ntile$fibnt[ntile$Cereal_Name==s]
        
        if(c1whgnt==1){wg1f[k] <- wg1f[k] + 0.75*hccerxpd[k]}
        if(c1whgnt==2){wg2f[k] <- wg2f[k] + 0.75*hccerxpd[k]}
        if(c1whgnt==3){wg3f[k] <- wg3f[k] + 0.75*hccerxpd[k]}
        
        if(c2whgnt==1){wg1f[k] <- wg1f[k] + 0.25*hccerxpd[k]}
        if(c2whgnt==2){wg2f[k] <- wg2f[k] + 0.25*hccerxpd[k]}
        if(c2whgnt==3){wg3f[k] <- wg3f[k] + 0.25*hccerxpd[k]}
        
        if(c1sugnt==1){as1f[k] <- as1f[k] + 0.75*hccerxpd[k]}
        if(c1sugnt==2){as2f[k] <- as2f[k] + 0.75*hccerxpd[k]}
        if(c1sugnt==3){as3f[k] <- as3f[k] + 0.75*hccerxpd[k]}
        
        if(c2sugnt==1){as1f[k] <- as1f[k] + 0.25*hccerxpd[k]}
        if(c2sugnt==2){as2f[k] <- as2f[k] + 0.25*hccerxpd[k]}
        if(c2sugnt==3){as3f[k] <- as3f[k] + 0.25*hccerxpd[k]}
        
        if(c1calcnt==1){cm1f[k] <- cm1f[k] + 0.75*hccerxpd[k]}
        if(c1calcnt==2){cm2f[k] <- cm2f[k] + 0.75*hccerxpd[k]}
        if(c1calcnt==3){cm3f[k] <- cm3f[k] + 0.75*hccerxpd[k]}
        
        if(c2calcnt==1){cm1f[k] <- cm1f[k] + 0.25*hccerxpd[k]}
        if(c2calcnt==2){cm2f[k] <- cm2f[k] + 0.25*hccerxpd[k]}
        if(c2calcnt==3){cm3f[k] <- cm3f[k] + 0.25*hccerxpd[k]}
        
        
        if(c1fibnt==1){fb1f[k] <- fb1f[k] + 0.75*hccerxpd[k]}
        if(c1fibnt==2){fb2f[k] <- fb2f[k] + 0.75*hccerxpd[k]}
        if(c1fibnt==3){fb3f[k] <- fb3f[k] + 0.75*hccerxpd[k]}
        
        if(c2fibnt==1){fb1f[k] <- fb1f[k] + 0.25*hccerxpd[k]}
        if(c2fibnt==2){fb2f[k] <- fb2f[k] + 0.25*hccerxpd[k]}
        if(c2fibnt==3){fb3f[k] <- fb3f[k] + 0.25*hccerxpd[k]}
      }
    }
  }
  
  gfb1f <- rep(NA,nrow(data))
  gfb2f <- rep(NA,nrow(data))
  gfb3f <- rep(NA,nrow(data))
  gmilk <- rep(NA,nrow(data))
  gsoda <- rep(NA,nrow(data))
  gfrtjc <- rep(NA,nrow(data))
  gswtct <- rep(NA,nrow(data))
  genergy <- rep(NA,nrow(data))
  gfruit <- rep(NA,nrow(data))
  gsalad <- rep(NA,nrow(data))
  gfrfry <- rep(NA,nrow(data))
  gothpot <- rep(NA,nrow(data))
  gbean <- rep(NA,nrow(data))
  gothveg <- rep(NA,nrow(data))
  gpizza <- rep(NA,nrow(data))
  gsalsa <- rep(NA,nrow(data))
  gtomsc <- rep(NA,nrow(data))
  gcheese <- rep(NA,nrow(data))
  gwhgbrd <- rep(NA,nrow(data))
  gbrrice <- rep(NA,nrow(data))
  gcandy <- rep(NA,nrow(data))
  gdonut <- rep(NA,nrow(data))
  gcake <- rep(NA,nrow(data))
  gicecrm <- rep(NA,nrow(data))
  gpopcorn <- rep(NA,nrow(data))
  
  #calcium
  gcm1f <- rep(NA,nrow(data))
  gcm2f <- rep(NA,nrow(data))
  gcm3f <- rep(NA,nrow(data))
  
  #for whole grain;
  gwg1f <- rep(NA,nrow(data))
  gwg2f <- rep(NA,nrow(data))
  gwg3f <- rep(NA,nrow(data))
  
  #for dairy;
  dmilk <- rep(NA,nrow(data))
  dcheese <- rep(NA,nrow(data))
  dpizza <- rep(NA,nrow(data))
  dicecrm <- rep(NA,nrow(data))
  
  #for sugar/ssb;
  sas1f <- rep(NA,nrow(data))
  sas2f <- rep(NA,nrow(data))
  sas3f <- rep(NA,nrow(data))
  sicecrm <- rep(NA,nrow(data))
  scake <- rep(NA,nrow(data))
  ssoda <- rep(NA,nrow(data))
  sswtct <- rep(NA,nrow(data))
  senergy <- rep(NA,nrow(data))
  scandy <- rep(NA,nrow(data))
  sdonut <- rep(NA,nrow(data))
  
  #for fruit;
  ffrtjc <- rep(NA,nrow(data))
  ffruit <- rep(NA,nrow(data))
  
  #for veg;
  vsalad <- rep(NA,nrow(data))
  vfrfry <- rep(NA,nrow(data))
  vothpot <- rep(NA,nrow(data))
  vbean <- rep(NA,nrow(data))
  vothveg <- rep(NA,nrow(data))
  vpizza <- rep(NA,nrow(data))
  vsalsa <- rep(NA,nrow(data))
  vtomsc <- rep(NA,nrow(data))
  
  #for tot frt/veg;
  pfrtjc <- rep(NA,nrow(data))
  pfruit <- rep(NA,nrow(data))
  psalad <- rep(NA,nrow(data))
  pfrfry <- rep(NA,nrow(data))
  pothpot <- rep(NA,nrow(data))
  pbean <- rep(NA,nrow(data))
  pothveg <- rep(NA,nrow(data))
  ppizza <- rep(NA,nrow(data))
  psalsa <- rep(NA,nrow(data))
  ptomsc <- rep(NA,nrow(data))
  
  DSQfib <- rep(NA,nrow(data))
  DSQcalc <- rep(NA,nrow(data))
  DSQwhgr <- rep(NA,nrow(data))
  DSQsug <- rep(NA,nrow(data))
  DSQdairy <- rep(NA,nrow(data))
  DSQfvl <- rep(NA,nrow(data))
  DSQvlall <- rep(NA,nrow(data))
  DSQfvlnf <- rep(NA,nrow(data))
  DSQvlnf <- rep(NA,nrow(data))
  DSQfrt <- rep(NA,nrow(data))
  DSQssb <- rep(NA,nrow(data))
  
  DSQfib_low <- rep(NA,nrow(data))
  DSQcalc_low <- rep(NA,nrow(data))
  DSQwhgr_low <- rep(NA,nrow(data))
  DSQsug_low <- rep(NA,nrow(data))
  DSQdairy_low <- rep(NA,nrow(data))
  DSQfvl_low <- rep(NA,nrow(data))
  DSQvlall_low <- rep(NA,nrow(data))
  DSQfvlnf_low <- rep(NA,nrow(data))
  DSQvlnf_low <- rep(NA,nrow(data))
  DSQfrt_low <- rep(NA,nrow(data))
  DSQssb_low <- rep(NA,nrow(data))
  
  DSQfib_high <- rep(NA,nrow(data))
  DSQcalc_high <- rep(NA,nrow(data))
  DSQwhgr_high <- rep(NA,nrow(data))
  DSQsug_high <- rep(NA,nrow(data))
  DSQdairy_high <- rep(NA,nrow(data))
  DSQfvl_high <- rep(NA,nrow(data))
  DSQvlall_high <- rep(NA,nrow(data))
  DSQfvlnf_high <- rep(NA,nrow(data))
  DSQvlnf_high <- rep(NA,nrow(data))
  DSQfrt_high <- rep(NA,nrow(data))
  DSQssb_high <- rep(NA,nrow(data))
  
  for(k in 1:nrow(data)){
    x <- data$Gender[k]
    AGEGRP <- NA
    agecut <- NA
    if(!is.na(x) & x %in% c(1,2) & !is.na(data$Age[k])){
      
      AGEGRP <- bcage[k]
      
      for(j in 3:ncol(psize)){
        assign(names(psize)[j],NA)
        assign(names(psize)[j],
               psize[psize$gender==x & psize$agegrp==AGEGRP,j])
      }
      
      for(j in 2:ncol(rcoeff)){
        assign(tolower(names(rcoeff)[j]),NA)
        assign(tolower(names(rcoeff)[j]),
               rcoeff[rcoeff$gender==x,j])
      }
      
      #make psize adj freq vars
      gfb1f[k] <- fb1f[k] * gadj25
      gfb2f[k] <- fb2f[k] * gadj26
      gfb3f[k] <- fb3f[k] * gadj27
      gmilk[k] <- milkxpd[k] * gadj3
      gsoda[k] <- sodaxpd[k] * gadj4
      gfrtjc[k] <- frtjcxpd[k] * gadj5
      gswtct[k] <- swtctxpd[k] * gadj6
      genergy[k] <- energyxpd[k] * gadj7
      gfruit[k] <- fruitxpd[k] * gadj8
      gsalad[k] <- saladxpd[k] * gadj9
      gfrfry[k] <- frfryxpd[k] * gadj10
      gothpot[k] <- othpotxpd[k] * gadj11
      gbean[k] <- beanxpd[k] * gadj12
      gothveg[k] <- othvegxpd[k] * gadj13
      gpizza[k] <- pizzaxpd[k] * gadj14
      gsalsa[k] <- salsaxpd[k] * gadj15
      gtomsc[k] <- tomscxpd[k] * gadj16
      gcheese[k] <- cheesexpd[k] * gadj17
      gwhgbrd[k] <- whgbrdxpd[k] * gadj18
      gbrrice[k] <- brricexpd[k] * gadj19
      gcandy[k] <- candyxpd[k] * gadj20
      gdonut[k] <- donutxpd[k] * gadj21
      gcake[k] <- cakexpd[k] * gadj22
      gicecrm[k] <- icecrmxpd[k] * gadj23
      gpopcorn[k] <- popcornxpd[k] * gadj24
      
      #calcium
      gcm1f[k] <- cm1f[k] * gadj28
      gcm2f[k] <- cm2f[k] * gadj29
      gcm3f[k] <- cm3f[k] * gadj30
      
      #for whole grain
      gwg1f[k] <- wg1f[k] * gadj34
      gwg2f[k] <- wg2f[k] * gadj35
      gwg3f[k] <- wg3f[k] * gadj36
      
      #for dairy
      dmilk[k] <- milkxpd[k] * dadj3
      dcheese[k] <- cheesexpd[k] * dadj17
      dpizza[k] <- pizzaxpd[k] * dadj14
      dicecrm[k] <- icecrmxpd[k] * dadj23
      
      
      #for sugar/ssb
      sas1f[k] <- as1f[k] * sadj31
      sas2f[k] <- as2f[k] * sadj32
      sas3f[k] <- as3f[k] * sadj33
      sicecrm[k] <- icecrmxpd[k] * sadj23
      scake[k] <- cakexpd[k] * sadj22
      ssoda[k] <- sodaxpd[k] * sadj4
      sswtct[k] <- swtctxpd[k] * sadj6
      senergy[k] <- energyxpd[k] * sadj7
      scandy[k] <- candyxpd[k] * sadj20
      sdonut[k] <- donutxpd[k] * sadj21
      
      
      #for fruit
      ffrtjc[k] <- frtjcxpd[k] * fadj5
      ffruit[k] <- fruitxpd[k] * fadj8
      
      
      #for veg
      vsalad[k] <- saladxpd[k] * vadj9
      vfrfry[k] <- frfryxpd[k] * vadj10
      vothpot[k] <- othpotxpd[k] * vadj11
      vbean[k] <- beanxpd[k] * vadj12
      vothveg[k] <- othvegxpd[k] * vadj13
      vpizza[k] <- pizzaxpd[k] * vadj14
      vsalsa[k] <- salsaxpd[k] * vadj15
      vtomsc[k] <- tomscxpd[k] * vadj16
      
      #for tot frt/veg
      pfrtjc[k] <- frtjcxpd[k] * padj5
      pfruit[k] <- fruitxpd[k] * padj8
      psalad[k] <- saladxpd[k] * padj9
      pfrfry[k] <- frfryxpd[k] * padj10
      pothpot[k] <- othpotxpd[k] * padj11
      pbean[k] <- beanxpd[k] * padj12
      pothveg[k] <- othvegxpd[k] * padj13
      ppizza[k] <- pizzaxpd[k] * padj14
      psalsa[k] <- salsaxpd[k] * padj15
      ptomsc[k] <- tomscxpd[k] * padj16
      
      ###using regression coefficients###
      #pivotal values
      DSQfib[k] <- mfintercept +  (kidgrp[k] * mfkidb) + (teengrp[k] * mfteenb)  + (gfb1f[k] * mfcer1b) + (gfb2f[k] * mfcer2b) + (gfb3f[k] * mfcer3b) + (gwhgbrd[k] * mfwgbb) + (gbrrice[k] * mfbrricb) + 
        (gcheese[k] * mfcheesb) +  (gpizza[k] * mfpizzab) +  (gmilk[k] * mfmilkb) +  (gicecrm[k] * mficecrb) +  (gpopcorn[k] * mfpcornb) + 
        (gsoda[k] * mfsodab) +  (genergy[k] * mfspdrb) +  (gcake[k] * mfcakeb) +  (gdonut[k] * mfdonutb) +  (gswtct[k] * mfswctb) +  (gcandy[k] * mfcandyb) + 
        (gfrtjc[k] * mffjcb) +  (gfruit[k] * mffruitb)  +  (gsalad[k] * mfsaladb) + (gothpot[k] * mfothptb) +  (gbean[k] * mfbeanb) +
        (gothveg[k] * mfothvgb) +  (gfrfry[k] * mffrfrb) +  (gtomsc[k] * mftomscb) +  (gsalsa[k] * mfsalsab)
      
      DSQcalc[k] <- mcintercept +  (kidgrp[k] * mckidb) + (teengrp[k] * mcteenb)  + (gcm1f[k] * mccer1b) + (gcm2f[k] * mccer2b) + (gcm3f[k] * mccer3b) + (gwhgbrd[k] * mcwgbb) + (gbrrice[k] * mcbrricb) + 
        (gcheese[k] * mccheesb) +  (gpizza[k] * mcpizzab) +  (gmilk[k] * mcmilkb) +  (gicecrm[k] * mcicecrb) +  (gpopcorn[k] * mcpcornb) + 
        (gsoda[k] * mcsodab) +  (genergy[k] * mcspdrb) +  (gcake[k] * mccakeb) +  (gdonut[k] * mcdonutb) +  (gswtct[k] * mcswctb) +  (gcandy[k] * mccandyb) + 
        (gfrtjc[k] * mcfjcb) +  (gfruit[k] * mcfruitb)  +  (gsalad[k] * mcsaladb) + (gothpot[k] * mcothptb) +  (gbean[k] * mcbeanb) +
        (gothveg[k] * mcothvgb) +  (gfrfry[k] * mcfrfrb) +  (gtomsc[k] * mctomscb) +  (gsalsa[k] * mcsalsab) 
      
      DSQwhgr[k] <- mgintercept +  (kidgrp[k] * mgkidb) + (teengrp[k] * mgteenb)  + (gwg1f[k] * mgcer1b) + (gwg2f[k] * mgcer2b) + (gwg3f[k] * mgcer3b) + (gwhgbrd[k] * mgwgbb) + (gbrrice[k] * mgbrricb) + 
        (gpopcorn[k] * mgpcornb)  
      
      DSQsug[k] <- msintercept +  (kidgrp[k] * mskidb) + (teengrp[k] * msteenb)  + (sas1f[k] * mscer1b) + (sas2f[k] * mscer2b) + (sas3f[k] * mscer3b) +  
        (sicecrm[k] * msicecrb) +  (ssoda[k] * mssodab) +  (senergy[k] * msspdrb) +  (scake[k] * mscakeb) +  (sdonut[k] * msdonutb) +  (sswtct[k] * msswctb) +  (scandy[k] * mscandyb) 
      
      DSQdairy[k] <- mdintercept +   (kidgrp[k] * mdkidb) + (teengrp[k] * mdteenb)  + (dcheese[k] * mdcheesb) +  (dpizza[k] * mdpizzab) +  (dmilk[k] * mdmilkb) +  (dicecrm[k] * mdicecrb)  
      
      DSQfvl[k] <- mpintercept +   (kidgrp[k] * mpkidb) + (teengrp[k] * mpteenb) +  
        (pfrtjc[k] * mpfjcb) +  (pfruit[k] * mpfruitb)  +  (psalad[k] * mpsaladb) + (pothpot[k] * mpothptb) +  (pbean[k] * mpbeanb) +
        (pothveg[k] * mpothvgb) +  (pfrfry[k] * mpfrfrb) +  (ptomsc[k] * mptomscb) +  (psalsa[k] * mpsalsab) +  (ppizza[k] * mppizzab) 
      
      DSQvlall[k] <- mvintercept +   (kidgrp[k] * mvkidb) + (teengrp[k] * mvteenb) +  
        (vsalad[k] * mvsaladb) + (vothpot[k] * mvothptb) +  (vbean[k] * mvbeanb) +  (vpizza[k] * mvpizzab) +
        (vothveg[k] * mvothvgb) +  (vfrfry[k] * mvfrfrb) +  (vtomsc[k] * mvtomscb) +  (vsalsa[k] * mvsalsab) 
      
      DSQfvlnf[k] <- mnintercept +   (kidgrp[k] * mnkidb) + (teengrp[k] * mnteenb) +  
        (pfrtjc[k] * mnfjcb) +  (pfruit[k] * mnfruitb)  +  (psalad[k] * mnsaladb) + (pothpot[k] * mnothptb) +  (pbean[k] * mnbeanb) +
        (pothveg[k] * mnothvgb) +  (ptomsc[k] * mntomscb) +  (psalsa[k] * mnsalsab) + (ppizza[k] * mnpizzab) 
      
      DSQvlnf[k] <- muintercept +   (kidgrp[k] * mukidb) + (teengrp[k] * muteenb) +  
        (vsalad[k] * musaladb) + (vothpot[k] * muothptb) +  (vbean[k] * mubeanb) +  (vpizza[k] * mupizzab) +
        (vothveg[k] * muothvgb) +  (vtomsc[k] * mutomscb) +  (vsalsa[k] * musalsab) 
      
      DSQfrt[k] <- mrintercept +   (kidgrp[k] * mrkidb) + (teengrp[k] * mrteenb)  + (ffrtjc[k] * mrfjcb) +  (ffruit[k] * mrfruitb)  
      
      DSQssb[k] <- mxintercept +  (kidgrp[k] * mxkidb) + (teengrp[k] * mxteenb) +  (ssoda[k] * mxsodab) +  (senergy[k] * mxspdrb) + (sswtct[k] * mxswctb)  
      
      #low values
      DSQfib_low[k] <- lfintercept +  (kidgrp[k] * lfkidb) + (teengrp[k] * lfteenb)  + (gfb1f[k] * lfcer1b) + (gfb2f[k] * lfcer2b) + (gfb3f[k] * lfcer3b) + (gwhgbrd[k] * lfwgbb) + (gbrrice[k] * lfbrricb) + 
        (gcheese[k] * lfcheesb) +  (gpizza[k] * lfpizzab) +  (gmilk[k] * lfmilkb) +  (gicecrm[k] * lficecrb) +  (gpopcorn[k] * lfpcornb) + 
        (gsoda[k] * lfsodab) +  (genergy[k] * lfspdrb) +  (gcake[k] * lfcakeb) +  (gdonut[k] * lfdonutb) +  (gswtct[k] * lfswctb) +  (gcandy[k] * lfcandyb) + 
        (gfrtjc[k] * lffjcb) +  (gfruit[k] * lffruitb)  +  (gsalad[k] * lfsaladb) + (gothpot[k] * lfothptb) +  (gbean[k] * lfbeanb) +
        (gothveg[k] * lfothvgb) +  (gfrfry[k] * lffrfrb) +  (gtomsc[k] * lftomscb) +  (gsalsa[k] * lfsalsab) 
      
      DSQcalc_low[k] <- lcintercept +  (kidgrp[k] * lckidb) + (teengrp[k] * lcteenb)  + (gcm1f[k] * lccer1b) + (gcm2f[k] * lccer2b) + (gcm3f[k] * lccer3b) + (gwhgbrd[k] * lcwgbb) + (gbrrice[k] * lcbrricb) + 
        (gcheese[k] * lccheesb) +  (gpizza[k] * lcpizzab) +  (gmilk[k] * lcmilkb) +  (gicecrm[k] * lcicecrb) +  (gpopcorn[k] * lcpcornb) + 
        (gsoda[k] * lcsodab) +  (genergy[k] * lcspdrb) +  (gcake[k] * lccakeb) +  (gdonut[k] * lcdonutb) +  (gswtct[k] * lcswctb) +  (gcandy[k] * lccandyb) + 
        (gfrtjc[k] * lcfjcb) +  (gfruit[k] * lcfruitb)  +  (gsalad[k] * lcsaladb) + (gothpot[k] * lcothptb) +  (gbean[k] * lcbeanb) +
        (gothveg[k] * lcothvgb) +  (gfrfry[k] * lcfrfrb) +  (gtomsc[k] * lctomscb) +  (gsalsa[k] * lcsalsab) 
      
      DSQwhgr_low[k] <- lgintercept +  (kidgrp[k] * lgkidb) + (teengrp[k] * lgteenb)  + (gwg1f[k] * lgcer1b) + (gwg2f[k] * lgcer2b) + (gwg3f[k] * lgcer3b) + (gwhgbrd[k] * lgwgbb) + (gbrrice[k] * lgbrricb) + 
        (gpopcorn[k] * lgpcornb)  
      
      DSQsug_low[k] <- lsintercept +  (kidgrp[k] * lskidb) + (teengrp[k] * lsteenb)  + (sas1f[k] * lscer1b) + (sas2f[k] * lscer2b) + (sas3f[k] * lscer3b) +  
        (sicecrm[k] * lsicecrb) +  (ssoda[k] * lssodab) +  (senergy[k] * lsspdrb) +  (scake[k] * lscakeb) +  (sdonut[k] * lsdonutb) +  (sswtct[k] * lsswctb) +  (scandy[k] * lscandyb) 
      
      DSQdairy_low[k] <- ldintercept +   (kidgrp[k] * ldkidb) + (teengrp[k] * ldteenb)  + (dcheese[k] * ldcheesb) +  (dpizza[k] * ldpizzab) +  (dmilk[k] * ldmilkb) +  (dicecrm[k] * ldicecrb)  
      
      DSQfvl_low[k] <- lpintercept +   (kidgrp[k] * lpkidb) + (teengrp[k] * lpteenb) +  
        (pfrtjc[k] * lpfjcb) +  (pfruit[k] * lpfruitb)  +  (psalad[k] * lpsaladb) + (pothpot[k] * lpothptb) +  (pbean[k] * lpbeanb) +
        (pothveg[k] * lpothvgb) +  (pfrfry[k] * lpfrfrb) +  (ptomsc[k] * lptomscb) +  (psalsa[k] * lpsalsab) +  (ppizza[k] * lppizzab) 
      
      DSQvlall_low[k] <- lvintercept +   (kidgrp[k] * lvkidb) + (teengrp[k] * lvteenb) +  
        (vsalad[k] * lvsaladb) + (vothpot[k] * lvothptb) +  (vbean[k] * lvbeanb) +  (vpizza[k] * lvpizzab) +
        (vothveg[k] * lvothvgb) +  (vfrfry[k] * lvfrfrb) +  (vtomsc[k] * lvtomscb) +  (vsalsa[k] * lvsalsab) 
      
      DSQfvlnf_low[k] <- lnintercept +   (kidgrp[k] * lnkidb) + (teengrp[k] * lnteenb) +  
        (pfrtjc[k] * lnfjcb) +  (pfruit[k] * lnfruitb)  +  (psalad[k] * lnsaladb) + (pothpot[k] * lnothptb) +  (pbean[k] * lnbeanb) +
        (pothveg[k] * lnothvgb) +  (ptomsc[k] * lntomscb) +  (psalsa[k] * lnsalsab) + (ppizza[k] * lnpizzab) 
      
      DSQvlnf_low[k] <- luintercept +   (kidgrp[k] * lukidb) + (teengrp[k] * luteenb) +  
        (vsalad[k] * lusaladb) + (vothpot[k] * luothptb) +  (vbean[k] * lubeanb) +  (vpizza[k] * lupizzab) +
        (vothveg[k] * luothvgb) +  (vtomsc[k] * lutomscb) +  (vsalsa[k] * lusalsab) 
      
      DSQfrt_low[k] <- lrintercept +   (kidgrp[k] * lrkidb) + (teengrp[k] * lrteenb)  + (ffrtjc[k] * lrfjcb) +  (ffruit[k] * lrfruitb)  
      
      DSQssb_low[k] <- lxintercept +  (kidgrp[k] * lxkidb) + (teengrp[k] * lxteenb) +  (ssoda[k] * lxsodab) +  (senergy[k] * lxspdrb) + (sswtct[k] * lxswctb)  
      
      #high values
      DSQfib_high[k] <- ufintercept +  (kidgrp[k] * ufkidb) + (teengrp[k] * ufteenb)  + (gfb1f[k] * ufcer1b) + (gfb2f[k] * ufcer2b) + (gfb3f[k] * ufcer3b) + (gwhgbrd[k] * ufwgbb) + (gbrrice[k] * ufbrricb) + 
        (gcheese[k] * ufcheesb) +  (gpizza[k] * ufpizzab) +  (gmilk[k] * ufmilkb) +  (gicecrm[k] * uficecrb) +  (gpopcorn[k] * ufpcornb) + 
        (gsoda[k] * ufsodab) +  (genergy[k] * ufspdrb) +  (gcake[k] * ufcakeb) +  (gdonut[k] * ufdonutb) +  (gswtct[k] * ufswctb) +  (gcandy[k] * ufcandyb) + 
        (gfrtjc[k] * uffjcb) +  (gfruit[k] * uffruitb)  +  (gsalad[k] * ufsaladb) + (gothpot[k] * ufothptb) +  (gbean[k] * ufbeanb) +
        (gothveg[k] * ufothvgb) +  (gfrfry[k] * uffrfrb) +  (gtomsc[k] * uftomscb) +  (gsalsa[k] * ufsalsab) 
      
      DSQcalc_high[k] <- ucintercept +  (kidgrp[k] * uckidb) + (teengrp[k] * ucteenb)  + (gcm1f[k] * uccer1b) + (gcm2f[k] * uccer2b) + (gcm3f[k] * uccer3b) + (gwhgbrd[k] * ucwgbb) + (gbrrice[k] * ucbrricb) + 
        (gcheese[k] * uccheesb) +  (gpizza[k] * ucpizzab) +  (gmilk[k] * ucmilkb) +  (gicecrm[k] * ucicecrb) +  (gpopcorn[k] * ucpcornb) + 
        (gsoda[k] * ucsodab) +  (genergy[k] * ucspdrb) +  (gcake[k] * uccakeb) +  (gdonut[k] * ucdonutb) +  (gswtct[k] * ucswctb) +  (gcandy[k] * uccandyb) + 
        (gfrtjc[k] * ucfjcb) +  (gfruit[k] * ucfruitb)  +  (gsalad[k] * ucsaladb) + (gothpot[k] * ucothptb) +  (gbean[k] * ucbeanb) +
        (gothveg[k] * ucothvgb) +  (gfrfry[k] * ucfrfrb) +  (gtomsc[k] * uctomscb) +  (gsalsa[k] * ucsalsab) 
      
      DSQwhgr_high[k] <- ugintercept +  (kidgrp[k] * ugkidb) + (teengrp[k] * ugteenb)  + (gwg1f[k] * ugcer1b) + (gwg2f[k] * ugcer2b) + (gwg3f[k] * ugcer3b) + (gwhgbrd[k] * ugwgbb) + (gbrrice[k] * ugbrricb) + 
        (gpopcorn[k] * ugpcornb)  
      
      DSQsug_high[k] <- usintercept +  (kidgrp[k] * uskidb) + (teengrp[k] * usteenb)  + (sas1f[k] * uscer1b) + (sas2f[k] * uscer2b) + (sas3f[k] * uscer3b) +  
        (sicecrm[k] * usicecrb) +  (ssoda[k] * ussodab) +  (senergy[k] * usspdrb) +  (scake[k] * uscakeb) +  (sdonut[k] * usdonutb) +  (sswtct[k] * usswctb) +  (scandy[k] * uscandyb) 
      
      DSQdairy_high[k] <- udintercept +   (kidgrp[k] * udkidb) + (teengrp[k] * udteenb)  + (dcheese[k] * udcheesb) +  (dpizza[k] * udpizzab) +  (dmilk[k] * udmilkb) +  (dicecrm[k] * udicecrb)  
      
      DSQfvl_high[k] <- upintercept +   (kidgrp[k] * upkidb) + (teengrp[k] * upteenb) +  
        (pfrtjc[k] * upfjcb) +  (pfruit[k] * upfruitb)  +  (psalad[k] * upsaladb) + (pothpot[k] * upothptb) +  (pbean[k] * upbeanb) +
        (pothveg[k] * upothvgb) +  (pfrfry[k] * upfrfrb) +  (ptomsc[k] * uptomscb) +  (psalsa[k] * upsalsab) +  (ppizza[k] * uppizzab) 
      
      DSQvlall_high[k] <- uvintercept +   (kidgrp[k] * uvkidb) + (teengrp[k] * uvteenb) +  
        (vsalad[k] * uvsaladb) + (vothpot[k] * uvothptb) +  (vbean[k] * uvbeanb) +  (vpizza[k] * uvpizzab) +
        (vothveg[k] * uvothvgb) +  (vfrfry[k] * uvfrfrb) +  (vtomsc[k] * uvtomscb) +  (vsalsa[k] * uvsalsab) 
      
      DSQfvlnf_high[k] <- unintercept +   (kidgrp[k] * unkidb) + (teengrp[k] * unteenb) +  
        (pfrtjc[k] * unfjcb) +  (pfruit[k] * unfruitb)  +  (psalad[k] * unsaladb) + (pothpot[k] * unothptb) +  (pbean[k] * unbeanb) +
        (pothveg[k] * unothvgb) +  (ptomsc[k] * untomscb) +  (psalsa[k] * unsalsab) + (ppizza[k] * unpizzab) 
      
      DSQvlnf_high[k] <- uuintercept +   (kidgrp[k] * uukidb) + (teengrp[k] * uuteenb) +  
        (vsalad[k] * uusaladb) + (vothpot[k] * uuothptb) +  (vbean[k] * uubeanb) +  (vpizza[k] * uupizzab) +
        (vothveg[k] * uuothvgb) +  (vtomsc[k] * uutomscb) +  (vsalsa[k] * uusalsab) 
      
      DSQfrt_high[k] <- urintercept +   (kidgrp[k] * urkidb) + (teengrp[k] * urteenb)  + (ffrtjc[k] * urfjcb) +  (ffruit[k] * urfruitb)  
      
      DSQssb_high[k] <- uxintercept +  (kidgrp[k] * uxkidb) + (teengrp[k] * uxteenb) +  (ssoda[k] * uxsodab) +  (senergy[k] * uxspdrb) + (sswtct[k] * uxswctb)  
    }
  }
  
  #pivotal values
  DSQfib[DSQfib<0] <- 0
  DSQcalc[DSQcalc<0] <- 0
  DSQwhgr[DSQwhgr<0] <- 0
  DSQsug[DSQsug<0] <- 0
  DSQdairy[DSQdairy<0] <- 0
  DSQfvl[DSQfvl<0] <- 0
  DSQvlall[DSQvlall<0] <- 0
  DSQfvlnf[DSQfvlnf<0] <- 0
  DSQvlnf[DSQvlnf<0] <- 0
  DSQfrt[DSQfrt<0] <- 0
  DSQssb[DSQssb<0] <- 0
  
  #low values
  DSQfib_low[DSQfib_low < (-100)] <- -100
  DSQcalc_low[DSQcalc_low < (-100)] <- -100
  DSQwhgr_low[DSQwhgr_low < (-100)] <- -100
  DSQsug_low[DSQsug_low < (-100)] <- -100
  DSQdairy_low[DSQdairy_low < (-100)] <- -100
  DSQfvl_low[DSQfvl_low < (-100)] <- -100
  DSQvlall_low[DSQvlall_low < (-100)] <- -100
  DSQfvlnf_low[DSQfvlnf_low < (-100)] <- -100
  DSQvlnf_low[DSQvlnf_low < (-100)] <- -100
  DSQfrt_low[DSQfrt_low < (-100)] <- -100
  DSQssb_low[DSQssb_low < (-100)] <- -100
  
  DSQfib_low[DSQfib_low > 100] <- 100
  DSQcalc_low[DSQcalc_low > 100] <- 100
  DSQwhgr_low[DSQwhgr_low > 100] <- 100
  DSQsug_low[DSQsug_low > 100] <- 100
  DSQdairy_low[DSQdairy_low > 100] <- 100
  DSQfvl_low[DSQfvl_low > 100] <- 100
  DSQvlall_low[DSQvlall_low > 100] <- 100
  DSQfvlnf_low[DSQfvlnf_low > 100] <- 100
  DSQvlnf_low[DSQvlnf_low > 100] <- 100
  DSQfrt_low[DSQfrt_low > 100] <- 100
  DSQssb_low[DSQssb_low > 100] <- 100
  
  DSQfib_low <- exp(DSQfib_low)/(1+exp(DSQfib_low))
  DSQcalc_low <- exp(DSQcalc_low)/(1+exp(DSQcalc_low))
  DSQwhgr_low <- exp(DSQwhgr_low)/(1+exp(DSQwhgr_low))
  DSQsug_low <- exp(DSQsug_low)/(1+exp(DSQsug_low))
  DSQdairy_low <- exp(DSQdairy_low)/(1+exp(DSQdairy_low))
  DSQfvl_low <- exp(DSQfvl_low)/(1+exp(DSQfvl_low))
  DSQvlall_low <- exp(DSQvlall_low)/(1+exp(DSQvlall_low))
  DSQfvlnf_low <- exp(DSQfvlnf_low)/(1+exp(DSQfvlnf_low))
  DSQvlnf_low <- exp(DSQvlnf_low)/(1+exp(DSQvlnf_low))
  DSQfrt_low <- exp(DSQfrt_low)/(1+exp(DSQfrt_low))
  DSQssb_low <- exp(DSQssb_low)/(1+exp(DSQssb_low))
  
  
  #high values
  DSQfib_high[DSQfib_high < (-100)] <- -100
  DSQcalc_high[DSQcalc_high < (-100)] <- -100
  DSQwhgr_high[DSQwhgr_high < (-100)] <- -100
  DSQsug_high[DSQsug_high < (-100)] <- -100
  DSQdairy_high[DSQdairy_high < (-100)] <- -100
  DSQfvl_high[DSQfvl_high < (-100)] <- -100
  DSQvlall_high[DSQvlall_high < (-100)] <- -100
  DSQfvlnf_high[DSQfvlnf_high < (-100)] <- -100
  DSQvlnf_high[DSQvlnf_high < (-100)] <- -100
  DSQfrt_high[DSQfrt_high < (-100)] <- -100
  DSQssb_high[DSQssb_high < (-100)] <- -100
  
  DSQfib_high[DSQfib_high > 100] <- 100
  DSQcalc_high[DSQcalc_high > 100] <- 100
  DSQwhgr_high[DSQwhgr_high > 100] <- 100
  DSQsug_high[DSQsug_high > 100] <- 100
  DSQdairy_high[DSQdairy_high > 100] <- 100
  DSQfvl_high[DSQfvl_high > 100] <- 100
  DSQvlall_high[DSQvlall_high > 100] <- 100
  DSQfvlnf_high[DSQfvlnf_high > 100] <- 100
  DSQvlnf_high[DSQvlnf_high > 100] <- 100
  DSQfrt_high[DSQfrt_high > 100] <- 100
  DSQssb_high[DSQssb_high > 100] <- 100
  
  DSQfib_high <- exp(DSQfib_high)/(1+exp(DSQfib_high))
  DSQcalc_high <- exp(DSQcalc_high)/(1+exp(DSQcalc_high))
  DSQwhgr_high <- exp(DSQwhgr_high)/(1+exp(DSQwhgr_high))
  DSQsug_high <- exp(DSQsug_high)/(1+exp(DSQsug_high))
  DSQdairy_high <- exp(DSQdairy_high)/(1+exp(DSQdairy_high))
  DSQfvl_high <- exp(DSQfvl_high)/(1+exp(DSQfvl_high))
  DSQvlall_high <- exp(DSQvlall_high)/(1+exp(DSQvlall_high))
  DSQfvlnf_high <- exp(DSQfvlnf_high)/(1+exp(DSQfvlnf_high))
  DSQvlnf_high <- exp(DSQvlnf_high)/(1+exp(DSQvlnf_high))
  DSQfrt_high <- exp(DSQfrt_high)/(1+exp(DSQfrt_high))
  DSQssb_high <- exp(DSQssb_high)/(1+exp(DSQssb_high))
  
  
  out <- data.frame(DSQfib = DSQfib,
                    DSQcalc = DSQcalc,
                    DSQwhgr = DSQwhgr,
                    DSQsug = DSQsug,
                    DSQdairy = DSQdairy,
                    DSQfvl = DSQfvl,
                    DSQvlall = DSQvlall,
                    DSQfvlnf = DSQfvlnf,
                    DSQvlnf = DSQvlnf,
                    DSQfrt = DSQfrt,
                    DSQssb = DSQssb,
                    
                    DSQfib_low = DSQfib_low,
                    DSQcalc_low = DSQcalc_low,
                    DSQwhgr_low = DSQwhgr_low,
                    DSQsug_low = DSQsug_low,
                    DSQdairy_low = DSQdairy_low,
                    DSQfvl_low = DSQfvl_low,
                    DSQvlall_low = DSQvlall_low,
                    DSQfvlnf_low = DSQfvlnf_low,
                    DSQvlnf_low = DSQvlnf_low,
                    DSQfrt_low = DSQfrt_low,
                    DSQssb_low = DSQssb_low,
                    
                    DSQfib_high = DSQfib_high,
                    DSQcalc_high = DSQcalc_high,
                    DSQwhgr_high = DSQwhgr_high,
                    DSQsug_high = DSQsug_high,
                    DSQdairy_high = DSQdairy_high,
                    DSQfvl_high = DSQfvl_high,
                    DSQvlall_high = DSQvlall_high,
                    DSQfvlnf_high = DSQfvlnf_high,
                    DSQvlnf_high = DSQvlnf_high,
                    DSQfrt_high = DSQfrt_high,
                    DSQssb_high = DSQssb_high)
  
  return(out)
}
