#' Scoring DSQ
#'
#' This function calculates.
#' @param data The input data frame.
#' @param method The version of scoring algorithm. Choose from "current" or "earlier". The default is "current".
#' @param keep Whether to keep the variables in the input data frame. The default is TRUE.
#' @return The intake values for calcium, etc.
#' @examples
#' data_scored <- DSQ_scores(data,method="c");
#' @export
DSQ_scores <- function(data, method="current", keep=TRUE){

    if(method=="current" | method=="c"){
        df_scored <- Score_DSQ_Current(data, keep)
        return(df_scored)
    }

    if(method=="earlier" | method=="e"){
         df_scored <- Score_DSQ_Earlier(data, keep)
         return(df_scored)
    }
}

Score_DSQ_Current <- function(data, keep=TRUE){

    ################
    #### Gender ####
    ################

    data$gender <- NA
    data$gender[data$DSQ_xx2=='A'] <- 1
    data$gender[data$DSQ_xx2=='B'] <- 2

    #############
    #### Age ####
    #############

    ageinyr <- data$DSQ_xx1

    # age groups
    data$bcage <- rep(NA,nrow(data))
    data$bcage[ageinyr>=2 & ageinyr<=3] <- 1
    data$bcage[ageinyr>=4 & ageinyr<=5] <- 2
    data$bcage[ageinyr>=6 & ageinyr<=7] <- 3
    data$bcage[ageinyr>=8 & ageinyr<=9] <- 4
    data$bcage[ageinyr>=10 & ageinyr<=11] <- 5
    data$bcage[ageinyr>=12 & ageinyr<=13] <- 6
    data$bcage[ageinyr>=14 & ageinyr<=15] <- 7
    data$bcage[ageinyr>=16 & ageinyr<=17] <- 8
    data$bcage[ageinyr>=18 & ageinyr<=27] <- 9
    data$bcage[ageinyr>=26 & ageinyr<=35] <- 10
    data$bcage[ageinyr>=36 & ageinyr<=45] <- 11
    data$bcage[ageinyr>=46 & ageinyr<=60] <- 12
    data$bcage[ageinyr>=61 & ageinyr<=69] <- 13
    data$bcage[ageinyr>=70 & ageinyr<=99] <- 14

    # make age dummy variables
    data$kidgrp <- 0
    data$kidgrp[ageinyr>=2 & ageinyr<=11] <- 1

    data$teengrp <- 0
    data$teengrp[ageinyr>=12 & ageinyr<=17] <- 1

    ###################
    #### Frequency ####
    ###################

    # converting responses into times per day for food variables of interest

    foodrange <- function(var,maxv,data,newname){
        out <- rep(NA,nrow(data))

        # prevent missing value problems
        x <- as.character(data[,var])
        x[is.na(x)] <- ""
        x[!(x %in% LETTERS[1:9])] <- ""

        # code the normal way
        out[x=="A"] <- 0
        out[x=="B"] <- 0.033
        out[x=="C"] <- 0.083
        out[x=="D"] <- 0.143
        out[x=="E"] <- 0.286
        out[x=="F"] <- 0.5
        out[x=="G"] <- 0.786
        out[x=="H"] <- 1
        out[x=="I"] <- 2

        # top code outliers
        out[out>maxv] <- maxv
        data[,newname] <- out
        return(data)
    }

    bevrange <- function(var,maxv,data,newname){
        out <- rep(NA,nrow(data))

        # prevent missing value problems
        x <- as.character(data[,var])
        x[is.na(x)] <- ""
        x[!(x %in% LETTERS[1:11])] <- ""

        # code the normal way
        out[x=="A"] <- 0
        out[x=="B"] <- 0.033
        out[x=="C"] <- 0.083
        out[x=="D"] <- 0.143
        out[x=="E"] <- 0.286
        out[x=="F"] <- 0.5
        out[x=="G"] <- 0.786
        out[x=="H"] <- 1
        out[x=="I"] <- 2.5
        out[x=="J"] <- 4.5
        out[x=="K"] <- 6

        # top code outliers
        out[out>maxv] <- maxv
        data[,newname] <- out
        return(data)
    }

    # hccerxpd='number of times per day eat hot or cold cereal'
    data <- foodrange("DSQ_010",7,data,"hccerxpd")

    # milkxpd='number of times per day drink milk'
    data <- bevrange("DSQ_030",10,data,"milkxpd")

    # sodaxpd='number of times per day drink soda'
    data <- bevrange("DSQ_040",8,data,"sodaxpd")

    # frtjcxpd='number of times per day drink fruit juice'
    data <- bevrange("DSQ_050",8,data,"frtjcxpd")

    # swtctxpd='number of times per day drink sweet coffee/tea'
    data <- bevrange("DSQ_060",10, data,"swtctxpd")

    # energyxpd='number of times per day drink fruit/sports/energy drink'
    data <- bevrange("DSQ_070",7,data,"energyxpd")

    # fruitxpd='number of times per day eat fruit'
    data <- foodrange("DSQ_080",8,data,"fruitxpd")

    # saladxpd='number of times per day eat salad'
    data <- foodrange("DSQ_090",5,data,"saladxpd")

    # frfryxpd='number of times per day eat fried potatoes'
    data <- foodrange("DSQ_100",5,data,"frfryxpd")

    # othpotxpd='number of times per day eat other potatoes'
    data <- foodrange("DSQ_110",3,data,"othpotxpd")

    # beanxpd='number of times per day eat beans'
    data <- foodrange("DSQ_120",4,data,"beanxpd")

    # othvegxpd='number of times per day eat other vegetables'
    data <- foodrange("DSQ_130",5,data,"othvegxpd")

    # pizzaxpd='number of times per day eat pizza'
    data <- foodrange("DSQ_140",2,data,"pizzaxpd")

    # salsaxpd='number of times per day eat salsa'
    data <- foodrange("DSQ_150",3,data,"salsaxpd")

    # tomscxpd='number of times per day eat tomtato sauce'
    data <- foodrange("DSQ_160",2,data,"tomscxpd")

    # cheesexpd='number of times per day eat cheese'
    data <- foodrange("DSQ_190",6,data,"cheesexpd")

    # whgbrdxpd='number of times per day eat whole grain bread'
    data <- foodrange("DSQ_200",6,data,"whgbrdxpd")

    # brricexpd='number of times per day eat cooked whole grain (brown rice)'
    data <- foodrange("DSQ_210",4,data,"brricexpd")

    # candyxpd='number of times per day eat candy'
    data <- foodrange("DSQ_220",8,data,"candyxpd")

    # donutxpd='number of times per day eat pastries'
    data <- foodrange("DSQ_230",5,data,"donutxpd")

    # cakexpd='number of times per day eat cookies/cake'
    data <- foodrange("DSQ_240",7,data,"cakexpd")

    # icecrmxpd='number of times per day eat ice cream'
    data <- foodrange("DSQ_250",5,data,"icecrmxpd")

    # popcornxpd='number of times per day eat pop corn';
    data <- foodrange("DSQ_260",3,data,"popcornxpd")

    ###########################
    ####### Cereal Data #######
    ###########################

    # extract nutrition database by food code
    ntile <- calib.DSQ.cereal.ntile
    ntile <- sqldf::sqldf("SELECT DISTINCT food_code, sugnt, whgnt, calcnt, fibnt FROM ntile")
    ntile$food_code <- as.character(ntile$food_code)
    #length(unique(ntile$food_code))

    # join cereal data to nutrition database
    data$DSQ_020[is.na(data$DSQ_020)] <- ""
    cereal1 <- merge(data[,c("UNIQUEID","DSQ_020","hccerxpd")], ntile,
                     by.x = "DSQ_020", by.y = "food_code",
                     all.x = TRUE, all.y = FALSE)

    data$DSQ_xx3[is.na(data$DSQ_xx3)] <- ""
    cereal2 <- merge(data[,c("UNIQUEID","DSQ_xx3","hccerxpd")], ntile,
                     by.x = "DSQ_xx3", by.y = "food_code",
                     all.x = TRUE, all.y = FALSE)

    cereal_calc <- function(cereal_df){

        cereal_df$wg1f <- with(cereal_df, ifelse(whgnt==1, hccerxpd, 0))
        cereal_df$wg2f <- with(cereal_df, ifelse(whgnt==2, hccerxpd, 0))
        cereal_df$wg3f <- with(cereal_df, ifelse(whgnt==3, hccerxpd, 0))

        cereal_df$as1f <- with(cereal_df, ifelse(sugnt==1, hccerxpd, 0))
        cereal_df$as2f <- with(cereal_df, ifelse(sugnt==2, hccerxpd, 0))
        cereal_df$as3f <- with(cereal_df, ifelse(sugnt==3, hccerxpd, 0))

        cereal_df$cm1f <- with(cereal_df, ifelse(calcnt==1, hccerxpd, 0))
        cereal_df$cm2f <- with(cereal_df, ifelse(calcnt==2, hccerxpd, 0))
        cereal_df$cm3f <- with(cereal_df, ifelse(calcnt==3, hccerxpd, 0))

        cereal_df$fb1f <- with(cereal_df, ifelse(fibnt==1, hccerxpd, 0))
        cereal_df$fb2f <- with(cereal_df, ifelse(fibnt==2, hccerxpd, 0))
        cereal_df$fb3f <- with(cereal_df, ifelse(fibnt==3, hccerxpd, 0))

        return(cereal_df)
    }

    cereal1 <- cereal_calc(cereal1)
    cereal2 <- cereal_calc(cereal2)
    cereal_vars <- paste0(c("wg","as","cm","fb"),rep(1:3,4),"f")
    cereal_df <- merge(cereal1[,c("UNIQUEID","DSQ_020",cereal_vars)],
                       cereal2[,c("UNIQUEID","DSQ_xx3",cereal_vars)],
                       by="UNIQUEID")
    numcer <- ifelse(cereal_df$DSQ_020=="",0,ifelse(cereal_df$DSQ_xx3=="",1,2))

    cereal_df$wg1f <- ifelse(numcer==0, ifelse(data$DSQ_010=="A",0,NA), ifelse(numcer==1, cereal_df$wg1f.x,
                             0.75*cereal_df$wg1f.x + 0.25*cereal_df$wg1f.y))
    cereal_df$wg2f <- ifelse(numcer==0, ifelse(data$DSQ_010=="A",0,NA), ifelse(numcer==1, cereal_df$wg2f.x,
                             0.75*cereal_df$wg2f.x + 0.25*cereal_df$wg2f.y))
    cereal_df$wg3f <- ifelse(numcer==0, ifelse(data$DSQ_010=="A",0,NA), ifelse(numcer==1, cereal_df$wg3f.x,
                             0.75*cereal_df$wg3f.x + 0.25*cereal_df$wg3f.y))

    cereal_df$as1f <- ifelse(numcer==0, ifelse(data$DSQ_010=="A",0,NA), ifelse(numcer==1, cereal_df$as1f.x,
                             0.75*cereal_df$as1f.x + 0.25*cereal_df$as1f.y))
    cereal_df$as2f <- ifelse(numcer==0, ifelse(data$DSQ_010=="A",0,NA), ifelse(numcer==1, cereal_df$as2f.x,
                             0.75*cereal_df$as2f.x + 0.25*cereal_df$as2f.y))
    cereal_df$as3f <- ifelse(numcer==0, ifelse(data$DSQ_010=="A",0,NA), ifelse(numcer==1, cereal_df$as3f.x,
                             0.75*cereal_df$as3f.x + 0.25*cereal_df$as3f.y))

    cereal_df$cm1f <- ifelse(numcer==0, ifelse(data$DSQ_010=="A",0,NA), ifelse(numcer==1, cereal_df$cm1f.x,
                             0.75*cereal_df$cm1f.x + 0.25*cereal_df$cm1f.y))
    cereal_df$cm2f <- ifelse(numcer==0, ifelse(data$DSQ_010=="A",0,NA), ifelse(numcer==1, cereal_df$cm2f.x,
                             0.75*cereal_df$cm2f.x + 0.25*cereal_df$cm2f.y))
    cereal_df$cm3f <- ifelse(numcer==0, ifelse(data$DSQ_010=="A",0,NA), ifelse(numcer==1, cereal_df$cm3f.x,
                             0.75*cereal_df$cm3f.x + 0.25*cereal_df$cm3f.y))

    cereal_df$fb1f <- ifelse(numcer==0, ifelse(data$DSQ_010=="A",0,NA), ifelse(numcer==1, cereal_df$fb1f.x,
                             0.75*cereal_df$fb1f.x + 0.25*cereal_df$fb1f.y))
    cereal_df$fb2f <- ifelse(numcer==0, ifelse(data$DSQ_010=="A",0,NA), ifelse(numcer==1, cereal_df$fb2f.x,
                             0.75*cereal_df$fb2f.x + 0.25*cereal_df$fb2f.y))
    cereal_df$fb3f <- ifelse(numcer==0, ifelse(data$DSQ_010=="A",0,NA), ifelse(numcer==1, cereal_df$fb3f.x,
                             0.75*cereal_df$fb3f.x + 0.25*cereal_df$fb3f.y))

    # merge to main data
    data <- merge(data, cereal_df[,c("UNIQUEID",cereal_vars)], by="UNIQUEID")

    ######################################################
    #### Portion Size Adjustment by gender and agegrp ####
    ######################################################

    # join adjustment factors by age and gender
    psize <- calib.portion.size
    data <- merge(data, psize, by.x=c("bcage","gender"), by.y=c("agegrp","gender"),
                  all.x=TRUE, all.y=FALSE)

    data$gmilk <- with(data, milkxpd * gadj3)
    data$gsoda <- with(data, sodaxpd * gadj4)
    data$gfrtjc <- with(data, frtjcxpd * gadj5)
    data$gswtct <- with(data, swtctxpd * gadj6)
    data$genergy <- with(data, energyxpd * gadj7)
    data$gfruit <- with(data, fruitxpd * gadj8)
    data$gsalad <- with(data, saladxpd * gadj9)
    data$gfrfry <- with(data, frfryxpd * gadj10)
    data$gothpot <- with(data, othpotxpd * gadj11)
    data$gbean <- with(data, beanxpd * gadj12)
    data$gothveg <- with(data, othvegxpd * gadj13)
    data$gpizza <- with(data, pizzaxpd * gadj14)
    data$gsalsa <- with(data, salsaxpd * gadj15)
    data$gtomsc <- with(data, tomscxpd * gadj16)
    data$gcheese <- with(data, cheesexpd * gadj17)
    data$gwhgbrd <- with(data, whgbrdxpd * gadj18)
    data$gbrrice <- with(data, brricexpd * gadj19)
    data$gcandy <- with(data, candyxpd * gadj20)
    data$gdonut <- with(data, donutxpd * gadj21)
    data$gcake <- with(data, cakexpd * gadj22)
    data$gicecrm <- with(data, icecrmxpd * gadj23)
    data$gpopcorn <- with(data, popcornxpd * gadj24)

    # fiber
    data$gfb1f <- with(data, fb1f * gadj25)
    data$gfb2f <- with(data, fb2f * gadj26)
    data$gfb3f <- with(data, fb3f * gadj27)

    # calcium
    data$gcm1f <- with(data, cm1f * gadj28)
    data$gcm2f <- with(data, cm2f * gadj29)
    data$gcm3f <- with(data, cm3f * gadj30)

    # for whole grain
    data$gwg1f <- with(data, wg1f * gadj34)
    data$gwg2f <- with(data, wg2f * gadj35)
    data$gwg3f <- with(data, wg3f * gadj36)

    # ---- dadj ---
    # for dairy
    data$dmilk <- with(data, milkxpd * dadj3)
    data$dcheese <- with(data, cheesexpd * dadj17)
    data$dpizza <- with(data, pizzaxpd * dadj14)
    data$dicecrm <- with(data, icecrmxpd * dadj23)

    # ---- sadj ---
    # for sugar/ssb
    data$sas1f <- with(data, as1f * sadj31)
    data$sas2f <- with(data, as2f * sadj32)
    data$sas3f <- with(data, as3f * sadj33)
    data$sicecrm <- with(data, icecrmxpd * sadj23)
    data$scake <- with(data, cakexpd * sadj22)
    data$ssoda <- with(data, data$sodaxpd * sadj4)
    data$sswtct <- with(data, data$swtctxpd * sadj6)
    data$senergy <- with(data, energyxpd * sadj7)
    data$scandy <- with(data, candyxpd * sadj20)
    data$sdonut <- with(data, donutxpd * sadj21)

    # ---- fadj ---
    # for fruit
    data$ffrtjc <- with(data, frtjcxpd * fadj5)
    data$ffruit <- with(data, fruitxpd * fadj8)

    # ---- vadj ---
    # for veg
    data$vsalad <- with(data, saladxpd * vadj9)
    data$vfrfry <- with(data, frfryxpd * vadj10)
    data$vothpot <- with(data, othpotxpd * vadj11)
    data$vbean <- with(data, beanxpd * vadj12)
    data$vothveg <- with(data, othvegxpd * vadj13)
    data$vpizza <- with(data, pizzaxpd * vadj14)
    data$vsalsa <- with(data, salsaxpd * vadj15)
    data$vtomsc <- with(data, tomscxpd * vadj16)

    # ---- padj ---
    # for tot frt/veg
    data$pfrtjc <- with(data, frtjcxpd * padj5)
    data$pfruit <- with(data, fruitxpd * padj8)
    data$psalad <- with(data, saladxpd * padj9)
    data$pfrfry <- with(data, frfryxpd * padj10)
    data$pothpot <- with(data, othpotxpd * padj11)
    data$pbean <- with(data, beanxpd * padj12)
    data$pothveg <- with(data, othvegxpd * padj13)
    data$ppizza <- with(data, data$pizzaxpd * padj14)
    data$psalsa <- with(data, salsaxpd * padj15)
    data$ptomsc <- with(data, tomscxpd * padj16)

    ##############################################################
    #### Intercept and beta coefficient information by gender ####
    ##############################################################

    data <- merge(data, calib.equation.coeff, by="gender", all.x=TRUE, all.y=FALSE)

    #pivotal values
    DSQfib <- with(data, mfintercept +  (kidgrp * mfkidb) + (teengrp * mfteenb)  + (gfb1f * mfcer1b) + (gfb2f * mfcer2b) + (gfb3f * mfcer3b) + (gwhgbrd * mfwgbb) + (gbrrice * mfbrricb) +
        (gcheese * mfcheesb) +  (gpizza * mfpizzab) +  (gmilk * mfmilkb) +  (gicecrm * mficecrb) +  (gpopcorn * mfpcornb) +
        (gsoda * mfsodab) +  (genergy * mfspdrb) +  (gcake * mfcakeb) +  (gdonut * mfdonutb) +  (gswtct * mfswctb) +  (gcandy * mfcandyb) +
        (gfrtjc * mffjcb) +  (gfruit * mffruitb)  +  (gsalad * mfsaladb) + (gothpot * mfothptb) +  (gbean * mfbeanb) +
        (gothveg * mfothvgb) +  (gfrfry * mffrfrb) +  (gtomsc * mftomscb) +  (gsalsa * mfsalsab))

    DSQcalc <- with(data, mcintercept +  (kidgrp * mckidb) + (teengrp * mcteenb)  + (gcm1f * mccer1b) + (gcm2f * mccer2b) + (gcm3f * mccer3b) + (gwhgbrd * mcwgbb) + (gbrrice * mcbrricb) +
        (gcheese * mccheesb) +  (gpizza * mcpizzab) +  (gmilk * mcmilkb) +  (gicecrm * mcicecrb) +  (gpopcorn * mcpcornb) +
        (gsoda * mcsodab) +  (genergy * mcspdrb) +  (gcake * mccakeb) +  (gdonut * mcdonutb) +  (gswtct * mcswctb) +  (gcandy * mccandyb) +
        (gfrtjc * mcfjcb) +  (gfruit * mcfruitb)  +  (gsalad * mcsaladb) + (gothpot * mcothptb) +  (gbean * mcbeanb) +
        (gothveg * mcothvgb) +  (gfrfry * mcfrfrb) +  (gtomsc * mctomscb) +  (gsalsa * mcsalsab))

    DSQwhgr <- with(data, mgintercept +  (kidgrp * mgkidb) + (teengrp * mgteenb)  + (gwg1f * mgcer1b) + (gwg2f * mgcer2b) + (gwg3f * mgcer3b) + (gwhgbrd * mgwgbb) + (gbrrice * mgbrricb) +
        (gpopcorn * mgpcornb))

    DSQsug <- with(data, msintercept +  (kidgrp * mskidb) + (teengrp * msteenb)  + (sas1f * mscer1b) + (sas2f * mscer2b) + (sas3f * mscer3b) +
        (sicecrm * msicecrb) +  (ssoda * mssodab) +  (senergy * msspdrb) +  (scake * mscakeb) +  (sdonut * msdonutb) +  (sswtct * msswctb) +  (scandy * mscandyb))

    DSQdairy <- with(data, mdintercept +   (kidgrp * mdkidb) + (teengrp * mdteenb)  + (dcheese * mdcheesb) +  (dpizza * mdpizzab) +  (dmilk * mdmilkb) +  (dicecrm * mdicecrb))

    DSQfvl <- with(data, mpintercept +   (kidgrp * mpkidb) + (teengrp * mpteenb) +
        (pfrtjc * mpfjcb) +  (pfruit * mpfruitb)  +  (psalad * mpsaladb) + (pothpot * mpothptb) +  (pbean * mpbeanb) +
        (pothveg * mpothvgb) +  (pfrfry * mpfrfrb) +  (ptomsc * mptomscb) +  (psalsa * mpsalsab) +  (ppizza * mppizzab))

    DSQvlall <- with(data, mvintercept +   (kidgrp * mvkidb) + (teengrp * mvteenb) +
        (vsalad * mvsaladb) + (vothpot * mvothptb) +  (vbean * mvbeanb) +  (vpizza * mvpizzab) +
        (vothveg * mvothvgb) +  (vfrfry * mvfrfrb) +  (vtomsc * mvtomscb) +  (vsalsa * mvsalsab))

    DSQfvlnf <- with(data, mnintercept +   (kidgrp * mnkidb) + (teengrp * mnteenb) +
        (pfrtjc * mnfjcb) +  (pfruit * mnfruitb)  +  (psalad * mnsaladb) + (pothpot * mnothptb) +  (pbean * mnbeanb) +
        (pothveg * mnothvgb) +  (ptomsc * mntomscb) +  (psalsa * mnsalsab) + (ppizza * mnpizzab))

    DSQvlnf <- with(data, muintercept +   (kidgrp * mukidb) + (teengrp * muteenb) +
        (vsalad * musaladb) + (vothpot * muothptb) +  (vbean * mubeanb) +  (vpizza * mupizzab) +
        (vothveg * muothvgb) +  (vtomsc * mutomscb) +  (vsalsa * musalsab))

    DSQfrt <- with(data, mrintercept +   (kidgrp * mrkidb) + (teengrp * mrteenb)  + (ffrtjc * mrfjcb) +  (ffruit * mrfruitb))

    DSQssb <- with(data, mxintercept +  (kidgrp * mxkidb) + (teengrp * mxteenb) +  (ssoda * mxsodab) +  (senergy * mxspdrb) + (sswtct * mxswctb))

    #low values
    DSQfib_low <- with(data, lfintercept +  (kidgrp * lfkidb) + (teengrp * lfteenb)  + (gfb1f * lfcer1b) + (gfb2f * lfcer2b) + (gfb3f * lfcer3b) + (gwhgbrd * lfwgbb) + (gbrrice * lfbrricb) +
        (gcheese * lfcheesb) +  (gpizza * lfpizzab) +  (gmilk * lfmilkb) +  (gicecrm * lficecrb) +  (gpopcorn * lfpcornb) +
        (gsoda * lfsodab) +  (genergy * lfspdrb) +  (gcake * lfcakeb) +  (gdonut * lfdonutb) +  (gswtct * lfswctb) +  (gcandy * lfcandyb) +
        (gfrtjc * lffjcb) +  (gfruit * lffruitb)  +  (gsalad * lfsaladb) + (gothpot * lfothptb) +  (gbean * lfbeanb) +
        (gothveg * lfothvgb) +  (gfrfry * lffrfrb) +  (gtomsc * lftomscb) +  (gsalsa * lfsalsab))

    DSQcalc_low <- with(data, lcintercept +  (kidgrp * lckidb) + (teengrp * lcteenb)  + (gcm1f * lccer1b) + (gcm2f * lccer2b) + (gcm3f * lccer3b) + (gwhgbrd * lcwgbb) + (gbrrice * lcbrricb) +
        (gcheese * lccheesb) +  (gpizza * lcpizzab) +  (gmilk * lcmilkb) +  (gicecrm * lcicecrb) +  (gpopcorn * lcpcornb) +
        (gsoda * lcsodab) +  (genergy * lcspdrb) +  (gcake * lccakeb) +  (gdonut * lcdonutb) +  (gswtct * lcswctb) +  (gcandy * lccandyb) +
        (gfrtjc * lcfjcb) +  (gfruit * lcfruitb)  +  (gsalad * lcsaladb) + (gothpot * lcothptb) +  (gbean * lcbeanb) +
        (gothveg * lcothvgb) +  (gfrfry * lcfrfrb) +  (gtomsc * lctomscb) +  (gsalsa * lcsalsab))

    DSQwhgr_low <- with(data, lgintercept +  (kidgrp * lgkidb) + (teengrp * lgteenb)  + (gwg1f * lgcer1b) + (gwg2f * lgcer2b) + (gwg3f * lgcer3b) + (gwhgbrd * lgwgbb) + (gbrrice * lgbrricb) +
        (gpopcorn * lgpcornb))

    DSQsug_low <- with(data, lsintercept +  (kidgrp * lskidb) + (teengrp * lsteenb)  + (sas1f * lscer1b) + (sas2f * lscer2b) + (sas3f * lscer3b) +
        (sicecrm * lsicecrb) +  (ssoda * lssodab) +  (senergy * lsspdrb) +  (scake * lscakeb) +  (sdonut * lsdonutb) +  (sswtct * lsswctb) +  (scandy * lscandyb))

    DSQdairy_low <- with(data, ldintercept +   (kidgrp * ldkidb) + (teengrp * ldteenb)  + (dcheese * ldcheesb) +  (dpizza * ldpizzab) +  (dmilk * ldmilkb) +  (dicecrm * ldicecrb))

    DSQfvl_low <- with(data, lpintercept +   (kidgrp * lpkidb) + (teengrp * lpteenb) +
        (pfrtjc * lpfjcb) +  (pfruit * lpfruitb)  +  (psalad * lpsaladb) + (pothpot * lpothptb) +  (pbean * lpbeanb) +
        (pothveg * lpothvgb) +  (pfrfry * lpfrfrb) +  (ptomsc * lptomscb) +  (psalsa * lpsalsab) +  (ppizza * lppizzab))

    DSQvlall_low <- with(data, lvintercept +   (kidgrp * lvkidb) + (teengrp * lvteenb) +
        (vsalad * lvsaladb) + (vothpot * lvothptb) +  (vbean * lvbeanb) +  (vpizza * lvpizzab) +
        (vothveg * lvothvgb) +  (vfrfry * lvfrfrb) +  (vtomsc * lvtomscb) +  (vsalsa * lvsalsab))

    DSQfvlnf_low <- with(data, lnintercept +   (kidgrp * lnkidb) + (teengrp * lnteenb) +
        (pfrtjc * lnfjcb) +  (pfruit * lnfruitb)  +  (psalad * lnsaladb) + (pothpot * lnothptb) +  (pbean * lnbeanb) +
        (pothveg * lnothvgb) +  (ptomsc * lntomscb) +  (psalsa * lnsalsab) + (ppizza * lnpizzab))

    DSQvlnf_low <- with(data, luintercept +   (kidgrp * lukidb) + (teengrp * luteenb) +
        (vsalad * lusaladb) + (vothpot * luothptb) +  (vbean * lubeanb) +  (vpizza * lupizzab) +
        (vothveg * luothvgb) +  (vtomsc * lutomscb) +  (vsalsa * lusalsab))

    DSQfrt_low <- with(data, lrintercept +   (kidgrp * lrkidb) + (teengrp * lrteenb)  + (ffrtjc * lrfjcb) +  (ffruit * lrfruitb))

    DSQssb_low <- with(data, lxintercept +  (kidgrp * lxkidb) + (teengrp * lxteenb) +  (ssoda * lxsodab) +  (senergy * lxspdrb) + (sswtct * lxswctb))

    #high values
    DSQfib_high <- with(data, ufintercept +  (kidgrp * ufkidb) + (teengrp * ufteenb)  + (gfb1f * ufcer1b) + (gfb2f * ufcer2b) + (gfb3f * ufcer3b) + (gwhgbrd * ufwgbb) + (gbrrice * ufbrricb) +
        (gcheese * ufcheesb) +  (gpizza * ufpizzab) +  (gmilk * ufmilkb) +  (gicecrm * uficecrb) +  (gpopcorn * ufpcornb) +
        (gsoda * ufsodab) +  (genergy * ufspdrb) +  (gcake * ufcakeb) +  (gdonut * ufdonutb) +  (gswtct * ufswctb) +  (gcandy * ufcandyb) +
        (gfrtjc * uffjcb) +  (gfruit * uffruitb)  +  (gsalad * ufsaladb) + (gothpot * ufothptb) +  (gbean * ufbeanb) +
        (gothveg * ufothvgb) +  (gfrfry * uffrfrb) +  (gtomsc * uftomscb) +  (gsalsa * ufsalsab))

    DSQcalc_high <- with(data, ucintercept +  (kidgrp * uckidb) + (teengrp * ucteenb)  + (gcm1f * uccer1b) + (gcm2f * uccer2b) + (gcm3f * uccer3b) + (gwhgbrd * ucwgbb) + (gbrrice * ucbrricb) +
        (gcheese * uccheesb) +  (gpizza * ucpizzab) +  (gmilk * ucmilkb) +  (gicecrm * ucicecrb) +  (gpopcorn * ucpcornb) +
        (gsoda * ucsodab) +  (genergy * ucspdrb) +  (gcake * uccakeb) +  (gdonut * ucdonutb) +  (gswtct * ucswctb) +  (gcandy * uccandyb) +
        (gfrtjc * ucfjcb) +  (gfruit * ucfruitb)  +  (gsalad * ucsaladb) + (gothpot * ucothptb) +  (gbean * ucbeanb) +
        (gothveg * ucothvgb) +  (gfrfry * ucfrfrb) +  (gtomsc * uctomscb) +  (gsalsa * ucsalsab))

    DSQwhgr_high <- with(data, ugintercept +  (kidgrp * ugkidb) + (teengrp * ugteenb)  + (gwg1f * ugcer1b) + (gwg2f * ugcer2b) + (gwg3f * ugcer3b) + (gwhgbrd * ugwgbb) + (gbrrice * ugbrricb) +
        (gpopcorn * ugpcornb))

    DSQsug_high <- with(data, usintercept +  (kidgrp * uskidb) + (teengrp * usteenb)  + (sas1f * uscer1b) + (sas2f * uscer2b) + (sas3f * uscer3b) +
        (sicecrm * usicecrb) +  (ssoda * ussodab) +  (senergy * usspdrb) +  (scake * uscakeb) +  (sdonut * usdonutb) +  (sswtct * usswctb) +  (scandy * uscandyb))

    DSQdairy_high <- with(data, udintercept +   (kidgrp * udkidb) + (teengrp * udteenb)  + (dcheese * udcheesb) +  (dpizza * udpizzab) +  (dmilk * udmilkb) +  (dicecrm * udicecrb))

    DSQfvl_high <- with(data, upintercept +   (kidgrp * upkidb) + (teengrp * upteenb) +
        (pfrtjc * upfjcb) +  (pfruit * upfruitb)  +  (psalad * upsaladb) + (pothpot * upothptb) +  (pbean * upbeanb) +
        (pothveg * upothvgb) +  (pfrfry * upfrfrb) +  (ptomsc * uptomscb) +  (psalsa * upsalsab) +  (ppizza * uppizzab))

    DSQvlall_high <- with(data, uvintercept +   (kidgrp * uvkidb) + (teengrp * uvteenb) +
        (vsalad * uvsaladb) + (vothpot * uvothptb) +  (vbean * uvbeanb) +  (vpizza * uvpizzab) +
        (vothveg * uvothvgb) +  (vfrfry * uvfrfrb) +  (vtomsc * uvtomscb) +  (vsalsa * uvsalsab))

    DSQfvlnf_high <- with(data, unintercept +   (kidgrp * unkidb) + (teengrp * unteenb) +
        (pfrtjc * unfjcb) +  (pfruit * unfruitb)  +  (psalad * unsaladb) + (pothpot * unothptb) +  (pbean * unbeanb) +
        (pothveg * unothvgb) +  (ptomsc * untomscb) +  (psalsa * unsalsab) + (ppizza * unpizzab))

    DSQvlnf_high <- with(data, uuintercept +   (kidgrp * uukidb) + (teengrp * uuteenb) +
        (vsalad * uusaladb) + (vothpot * uuothptb) +  (vbean * uubeanb) +  (vpizza * uupizzab) +
        (vothveg * uuothvgb) +  (vtomsc * uutomscb) +  (vsalsa * uusalsab))

    DSQfrt_high <- with(data, urintercept +   (kidgrp * urkidb) + (teengrp * urteenb)  + (ffrtjc * urfjcb) +  (ffruit * urfruitb))

    DSQssb_high <- with(data, uxintercept +  (kidgrp * uxkidb) + (teengrp * uxteenb) +  (ssoda * uxsodab) +  (senergy * uxspdrb) + (sswtct * uxswctb))

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

    out <- data.frame(UNIQUEID = data$UNIQUEID,

                      DSQfib = DSQfib,
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

    # if keep, rbind

    return(out)
}

Score_DSQ_Earlier <- function(data){

    #trim <- function (x) gsub("^\\s+|\\s+$", "", x)
    # library(stringr) => str_trim()

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
    names(data)[names(data)=="Gender"] <- "gender"
    names(data)[names(data)=="Age"] <- "AGE"
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

