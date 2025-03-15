#' Scoring HEI from NDSR
#'
#' This function calculates.
#' @param data The input data frame.
#' @return The HEI scores, etc.
#' @examples
#' df_out <- HEI_scores(df_in);
#' @export
HEI_scores <- function(data){

    # /*********************************************************************/
    # /*                                                                   */
    # /*HEI2015 Calculator per Person NDSR 2013 or subsequent              */
    # /*HEI2015perpersonNDSR2013subsequent.sas                             */
    # /*                                                                   */
    # /*********************************************************************/
    # /*               VERSION 1.0         07/13/2017                      */
    # /*                                                                   */
    # /*                                                                   */
    # /*********************************************************************/

    # libname saslib "C:\SAS";
    # /* INCLUDE THE FORMATS */
    # %INCLUDE 'ndsformt.sas';
    #
    # proc sort data=saslib.fgscrecord; by cpartid dintake;
    # proc sort data=saslib.record; by cpartid dintake;
    #
    # *MERGE NDSR FILE 04 & FILE 09 DATA;
    # data record0409;
    # merge saslib.record saslib.fgscrecord;
    # by cpartid dintake;
    # run;

    # data hei0409; set record0409;
    hei0409 <- data.frame(
        Participant.ID = data$Participant.ID,
        Date.of.Intake = data$Date.of.Intake,
        # *ADEQUACY
        # ** TOTAL VEGETABLES COMPONENT;
        hei_totveg = ( data$rfgsciVEG0100 + data$rfgsciVEG0200 +
                           data$rfgsciVEG0300 + data$rfgsciVEG0400 +
                           data$rfgsciVEG0800 + data$rfgsciVEG0450 +
                           data$rfgsciVEG0700 + data$rfgsciVEG0600 +
                           data$rfgsciVEG0900 + data$rfgsciVEG0500)/2, #;

        # ** GREENS AND BEANS COMPONENT;
        hei_greensbeans = ( data$rfgsciVEG0100 + data$rfgsciVEG0700 )/2, #;

        # sHEI separate vegetable categories
        # VEG0100 - Dark-green Vegetables
        # VEG0200 - Deep-yellow Vegetables
        # VEG0300 - Tomato
        # VEG0400 - White Potatoes
        # VEG0450 - Other Starchy Vegetables
        # VEG0500 - Vegetable Juice
        # VEG0600 - Other Vegetables
        # VEG0700 - Legumes (cooked dried beans)
        # VEG0800 - Fried Potatoes
        # VEG0900 - Fried Vegetables
        greens    = data$rfgsciVEG0100/2,
        redorange = (data$rfgsciVEG0200 + data$rfgsciVEG0300)/2,
        starchy   = (data$rfgsciVEG0400 + data$rfgsciVEG0800 + data$rfgsciVEG0450)/2,
        beans     = data$rfgsciVEG0700/2,
        NDSRvlnf  = ( data$rfgsciVEG0100 + data$rfgsciVEG0200 +
                          data$rfgsciVEG0300 +
                          data$rfgsciVEG0700 + data$rfgsciVEG0600 +
                          data$rfgsciVEG0900 + data$rfgsciVEG0500)/2, # Total vegetable servings in cup equivalents including legumes and excluding French fries

        # ** TOTAL FRUITS COMPONENT;
        hei_totfruit = ( data$rfgsciFRU0100 + data$rfgsciFRU0200 +
                             data$rfgsciFRU0300 + data$rfgsciFRU0400 +
                             data$rfgsciFRU0500 + data$rfgsciFRU0600 +
                             data$rfgsciFRU0700 )/2, #;

        NDSRfvl = ( data$rfgsciVEG0100 + data$rfgsciVEG0200 +
                        data$rfgsciVEG0300 + data$rfgsciVEG0400 +
                        data$rfgsciVEG0800 + data$rfgsciVEG0450 +
                        data$rfgsciVEG0700 + data$rfgsciVEG0600 +
                        data$rfgsciVEG0900 + data$rfgsciVEG0500 +

                        data$rfgsciFRU0100 + data$rfgsciFRU0200 +
                        data$rfgsciFRU0300 + data$rfgsciFRU0400 +
                        data$rfgsciFRU0500 + data$rfgsciFRU0600 +
                        data$rfgsciFRU0700)/2, # Total fruit and vegetable servings in cup equivalents including legumes and French fries

        NDSRfvlnf = ( data$rfgsciVEG0100 + data$rfgsciVEG0200 +
                          data$rfgsciVEG0300 +
                          data$rfgsciVEG0700 + data$rfgsciVEG0600 +
                          data$rfgsciVEG0900 + data$rfgsciVEG0500 +

                          data$rfgsciFRU0100 + data$rfgsciFRU0200 +
                          data$rfgsciFRU0300 + data$rfgsciFRU0400 +
                          data$rfgsciFRU0500 + data$rfgsciFRU0600 +
                          data$rfgsciFRU0700)/2, # Total fruit and vegetable servings in cup equivalents including legumes and excluding French fries

        # ** WHOLE FRUITS COMPONENT;
        hei_wholefruit = ( data$rfgsciFRU0300 + data$rfgsciFRU0400 +
                               data$rfgsciFRU0500 + data$rfgsciFRU0600 +
                               data$rfgsciFRU0700 )/2, #;

        # ** WHOLE GRAINS COMPONENT;
        hei_wholegrains = data$riwholegrains, #;

        # ** DAIRY COMPONENT;
        hei_dairy = ( data$rfgsciDMF0100 + data$rfgsciDMR0100 +
                          data$rfgsciDML0100 + data$rfgsciDMN0100 +
                          data$rfgsciDMF0200 + data$rfgsciDMR0200 +
                          data$rfgsciDML0200 + data$rfgsciDML0300 +
                          data$rfgsciDML0400 + data$rfgsciDCF0100 +
                          data$rfgsciDCR0100 + data$rfgsciDCL0100 +
                          data$rfgsciDCN0100 + data$rfgsciDYF0100 +
                          data$rfgsciDYR0100 + data$rfgsciDYL0100 +
                          data$rfgsciDYF0200 + data$rfgsciDYR0200 +
                          data$rfgsciDYL0200 + data$rfgsciDYN0100 +
                          ((data$rfgsciDOT0100)/3) + data$rfgsciDOT0300 +
                          data$rfgsciDOT0400 + data$rfgsciDOT0500 + data$rfgsciDOT0600), #;

        # ** TOTAL PROTEIN FOODS COMPONENT;
        hei_totproteins = ( data$rfgsciMRF0100 + data$rfgsciMRL0100 +
                                data$rfgsciMRF0200 + data$rfgsciMRL0200 +
                                data$rfgsciMRF0300 + data$rfgsciMRL0300 +
                                data$rfgsciMRF0400 + data$rfgsciMRL0400 +
                                data$rfgsciMCF0200 + data$rfgsciMCL0200 +
                                data$rfgsciMRF0500 + data$rfgsciMPF0100 +
                                data$rfgsciMPL0100 + data$rfgsciMPF0200 +
                                data$rfgsciMFF0100 + data$rfgsciMFL0100 +
                                data$rfgsciMFF0200 + data$rfgsciMSL0100 +
                                data$rfgsciMSF0100 + data$rfgsciMCF0100 +
                                data$rfgsciMCL0100 + data$rfgsciMOF0100 +
                                data$rfgsciMOF0200 + data$rfgsciMOF0300 +
                                data$rfgsciMOF0400 + data$rfgsciMOF0500 +
                                data$rfgsciMOF0600 + data$rfgsciMOF0700 +
                                (data$rfgsciVEG0700*2) ), #;

        # ** SEAFOOD AND PLANT PROTEINS COMPONENT;
        hei_seafoodplantprot = ( data$rfgsciMFF0100 + data$rfgsciMFL0100 +
                                     data$rfgsciMFF0200 + data$rfgsciMSL0100 +
                                     data$rfgsciMSF0100 + data$rfgsciMOF0500 +
                                     data$rfgsciMOF0600 + data$rfgsciMOF0700 +
                                     (data$rfgsciVEG0700*2) ), #;

        #*MODERATION
        #** SODIUM COMPONENT;
        hei_sodium = data$rina/1000, #;

        #** REFINED GRAINS COMPONENT;
        hei_refinedgrains = data$rirefinedgrains, #;

        #** ADDED SUGARS COMPONENT;
        hei_addedsugars = data$rias_ts*4, #;

        #run;
        rikcal = data$rikcal,
        ripfa = data$ripfa,
        rimfa = data$rimfa,
        risfa = data$risfa,
        ripctsfa = data$ripctsfa,
        water = data$water,
        calcium = data$calcium,
        fiber = data$fiber,

        # DSQ's SSB added sugars (in tsp)
        NDSRssb = (data$rfgsciBVS0400 + # Sweetened Soft Drinks
                       # Coca Cola Classic: 12oz = 39g added sugars
                       # One teaspoon of granulated sugar equals 4 grams of sugar.
                       data$rfgsciBVS0300 + # Sweetened Fruit Drinks
                       data$rfgsciBVS0500 + # Sweetened Tea
                       data$rfgsciBVS0100 + # Sweetened Coffee
                       data$rfgsciBVS0200 + # Sweetened Coffee Substitutes
                       data$rfgsciBVS0600 + # Sweetened Water
                       data$rfgsciBVS0700 # Nondairy-based Sweetened Meal Replacement/Supplement - Includes meal replacement drinks, sports drinks.
        ) * 6.5
    )

    # data hei0409togroup;
    # 	set hei0409;
    # 	ripctsfa = ripctsfa*rikcal;
    #
    # proc means data=hei0409togroup noprint;
    #   by cpartid;
    #   var rikcal hei_totveg hei_greensbeans hei_totfruit hei_wholefruit
    #       hei_wholegrains hei_dairy hei_totproteins hei_seafoodplantprot
    # 		ripfa rimfa risfa hei_sodium hei_refinedgrains hei_addedsugars ripctsfa;
    #   output out=dailyhei0409 sum=;
    # run;
    #
    # data hei0409;
    # 	set dailyhei0409;
    # 	ripctsfa = ripctsfa/rikcal ;

    # Preserve a copy of sat fat calories before daily averaging
    hei0409togroup <- hei0409
    hei0409togroup$ripctsfa <- hei0409$ripctsfa * hei0409$rikcal

    dailyhei0409 <- aggregate(hei0409togroup[,c("rikcal", "hei_totveg", "hei_greensbeans",
                                                "hei_totfruit", "hei_wholefruit",
                                                "hei_wholegrains", "hei_dairy", "hei_totproteins",
                                                "hei_seafoodplantprot", "ripfa", "rimfa", "risfa",
                                                "hei_sodium", "hei_refinedgrains",
                                                "hei_addedsugars", "ripctsfa",
                                                "water", "calcium","fiber",
                                                "greens", "redorange", "starchy", "beans",
                                                "NDSRvlnf","NDSRfvl","NDSRfvlnf","NDSRssb")],
                              by=list(hei0409togroup$Participant.ID), FUN=mean)
    names(dailyhei0409)[1] <- "Participant.ID"
    #rm(hei0409togroup)

    hei0409 <- dailyhei0409
    hei0409$ripctsfa <- hei0409$ripctsfa/hei0409$rikcal
    #rm(dailyhei0409)

    attach(hei0409)
    # data hei0409;
    #    set hei0409;
    #
    # energy=rikcal/1000;
    energy <- rikcal/1000

    #### skipped as we did not have any Zeros

    # # if rikcal=0 then do;
    # if(rikcal==0){
    #
    #     HEIX1_TOTALVEG       = 0 #;
    #     HEIX2_GREEN_AND_BEAN = 0 #;
    #     HEIX3_TOTALFRUIT     = 0 #;
    #     HEIX4_WHOLEFRUIT     = 0 #;
    #     HEIX5_WHOLEGRAIN     = 0 #;
    #     HEIX6_TOTALDAIRY     = 0 #;
    #     HEIX7_TOTPROT        = 0 #;
    #     HEIX8_SEAPLANT_PROT  = 0 #;
    #     HEIX9_FATTYACID      = 0 #;
    #     HEIX10_SODIUM        = 0 #;
    #     HEIX11_REFINEDGRAIN  = 0 #;
    #     HEIX12_ADDEDSUGARS   = 0 #;
    #     HEIX13_SATFATS       = 0 #;
    #
    # #   end;
    # # else do;
    # } else {

    # *ADEQUACY
    # ** TOTAL VEGETABLES COMPONENT;
    # 	xhei_totveg=hei_totveg/energy;
    #
    # 	if xhei_totveg=0 then HEIX1_TOTALVEG=0;
    # 		else if xhei_totveg>=1.1 then HEIX1_TOTALVEG=5;
    # 		else HEIX1_TOTALVEG=5*(xhei_totveg/1.1);
    #
    xhei_totveg <- hei_totveg/energy

    HEIX1_TOTALVEG <- 5*(xhei_totveg/1.1)
    HEIX1_TOTALVEG[HEIX1_TOTALVEG>=5] <- 5

    # ** GREENS AND BEANS COMPONENT;
    # 	xhei_greensbeans=hei_greensbeans/energy;
    #
    # 	if xhei_greensbeans=0 then HEIX2_GREEN_AND_BEAN=0;
    # 		else if xhei_greensbeans>=0.2 then HEIX2_GREEN_AND_BEAN=5;
    # 		else HEIX2_GREEN_AND_BEAN=5*(xhei_greensbeans/0.2);
    #
    xhei_greensbeans <- hei_greensbeans/energy
    HEIX2_GREEN_AND_BEAN <- 5*(xhei_greensbeans/0.2)
    HEIX2_GREEN_AND_BEAN[HEIX2_GREEN_AND_BEAN>=5] <- 5

    # ** TOTAL FRUITS COMPONENT;
    # 	xhei_totfruit = hei_totfruit/energy;
    #
    # 	if xhei_totfruit=0 then HEIX3_TOTALFRUIT=0;
    # 		else if xhei_totfruit>=0.8 then HEIX3_TOTALFRUIT=5;
    # 		else HEIX3_TOTALFRUIT=5*(xhei_totfruit/0.8);
    #
    xhei_totfruit <- hei_totfruit/energy

    HEIX3_TOTALFRUIT <- 5*(xhei_totfruit/0.8)
    HEIX3_TOTALFRUIT[HEIX3_TOTALFRUIT>=5] <- 5

    # ** WHOLE FRUITS COMPONENT;
    # 	xhei_wholefruit=hei_wholefruit/energy;
    #
    # 	if xhei_wholefruit=0 then HEIX4_WHOLEFRUIT=0;
    # 		else if xhei_wholefruit>=0.4 then HEIX4_WHOLEFRUIT=5;
    # 		else HEIX4_WHOLEFRUIT=5*(xhei_wholefruit/0.4);
    #
    xhei_wholefruit <- hei_wholefruit/energy

    HEIX4_WHOLEFRUIT <- 5*(xhei_wholefruit/0.4)
    HEIX4_WHOLEFRUIT[HEIX4_WHOLEFRUIT>=5] <- 5

    # ** WHOLE GRAINS COMPONENT;
    # 	xhei_wholegrains=hei_wholegrains/energy;
    #
    # 	if xhei_wholegrains=0 then HEIX5_WHOLEGRAIN=0;
    # 		else if xhei_wholegrains>=1.5 then HEIX5_WHOLEGRAIN=10;
    # 		else HEIX5_WHOLEGRAIN=10*(xhei_wholegrains/1.5);
    #
    xhei_wholegrains <- hei_wholegrains/energy

    HEIX5_WHOLEGRAIN <- 10*(xhei_wholegrains/1.5)
    HEIX5_WHOLEGRAIN[HEIX5_WHOLEGRAIN>=10] <- 10

    # ** DAIRY COMPONENT;
    # 	xhei_dairy=hei_dairy/energy;
    #
    # 	if xhei_dairy=0 then HEIX6_TOTALDAIRY=0;
    # 		else if xhei_dairy>=1.3 then HEIX6_TOTALDAIRY=10;
    # 		else HEIX6_TOTALDAIRY=10*(xhei_dairy/1.3);
    #
    xhei_dairy <- hei_dairy/energy

    HEIX6_TOTALDAIRY <- 10*(xhei_dairy/1.3)
    HEIX6_TOTALDAIRY[HEIX6_TOTALDAIRY>=10] <- 10

    # ** TOTAL PROTEIN FOODS COMPONENT;
    # 	xhei_totproteins=hei_totproteins/energy;
    #
    # 	if xhei_totproteins=0 then HEIX7_TOTPROT=0;
    # 		else if xhei_totproteins>=2.5 then HEIX7_TOTPROT=5;
    # 		else HEIX7_TOTPROT=5*(xhei_totproteins/2.5);
    #
    xhei_totproteins <- hei_totproteins/energy

    HEIX7_TOTPROT <- 5*(xhei_totproteins/2.5)
    HEIX7_TOTPROT[HEIX7_TOTPROT>=5] <- 5

    # ** SEAFOOD AND PLANT PROTEINS COMPONENT;
    # 	xhei_seafoodplantprot=hei_seafoodplantprot/energy;
    #
    # 	if xhei_seafoodplantprot=0 then  HEIX8_SEAPLANT_PROT=0;
    # 		else if xhei_seafoodplantprot>=0.8 then  HEIX8_SEAPLANT_PROT=5;
    # 		else HEIX8_SEAPLANT_PROT=5*(xhei_seafoodplantprot/0.8);
    #
    xhei_seafoodplantprot <- hei_seafoodplantprot/energy

    HEIX8_SEAPLANT_PROT <- 5*(xhei_seafoodplantprot/0.8)
    HEIX8_SEAPLANT_PROT[HEIX8_SEAPLANT_PROT>=5] <- 5

    # ** FATTY ACIDS COMPONENT;
    # 	xhei_fatacid=(ripfa+rimfa)/risfa;
    #
    # 	FARMIN=1.2;
    # 	FARMAX=2.5;
    #  	if xhei_fatacid=<FARMIN then HEIX9_FATTYACID=0;
    # 		else if xhei_fatacid>FARMAX then HEIX9_FATTYACID=10;
    # 		else HEIX9_FATTYACID=10*((xhei_fatacid-FARMIN) / (FARMAX-FARMIN));
    #
    xhei_fatacid <- (ripfa+rimfa)/risfa

    FARMIN <- 1.2
    FARMAX <- 2.5
    HEIX9_FATTYACID <- 10*((xhei_fatacid-FARMIN) / (FARMAX-FARMIN))

    HEIX9_FATTYACID[HEIX9_FATTYACID<=0] <- 0
    HEIX9_FATTYACID[HEIX9_FATTYACID>=10] <- 10

    # *MODERATION
    # ** SODIUM COMPONENT;
    # 	xhei_sodium=hei_sodium/energy;
    #
    # 	SODMIN=1.1;
    # 	SODMAX=2.0;
    #  	if xhei_sodium>=SODMAX then HEIX10_SODIUM=0;
    # 		else if xhei_sodium=<SODMIN then HEIX10_SODIUM=10;
    # 		else HEIX10_SODIUM=10 - (10*((xhei_sodium-SODMIN) / (SODMAX-SODMIN)));
    #
    xhei_sodium <- hei_sodium/energy

    SODMIN <- 1.1
    SODMAX <- 2.0
    HEIX10_SODIUM <- 10 - (10*((xhei_sodium-SODMIN) / (SODMAX-SODMIN)))

    HEIX10_SODIUM[HEIX10_SODIUM<=0] <- 0
    HEIX10_SODIUM[HEIX10_SODIUM>=10] <- 10

    # ** REFINED GRAINS COMPONENT;
    # 	xhei_refinedgrains=hei_refinedgrains/energy;
    #
    # 	RGMIN=1.8;
    # 	RGMAX=4.3;
    #     if xhei_refinedgrains>=RGMAX then HEIX11_REFINEDGRAIN=0;
    # 		else if xhei_refinedgrains=<RGMIN then HEIX11_REFINEDGRAIN=10;
    # 		else HEIX11_REFINEDGRAIN=10 - (10*((xhei_refinedgrains-RGMIN) / (RGMAX-RGMIN)));
    #
    xhei_refinedgrains <- hei_refinedgrains/energy

    RGMIN <- 1.8
    RGMAX <- 4.3
    HEIX11_REFINEDGRAIN <- 10 - (10*((xhei_refinedgrains-RGMIN) / (RGMAX-RGMIN)))

    HEIX11_REFINEDGRAIN[HEIX11_REFINEDGRAIN<=0] <- 0
    HEIX11_REFINEDGRAIN[HEIX11_REFINEDGRAIN>=10] <- 10

    # ** ADDED SUGARS COMPONENT;
    # 	xhei_addedsugars=100*hei_addedsugars/rikcal;
    #
    # 	ADDSUGMIN=6.5;
    # 	ADDSUGMAX=26;
    # 	if xhei_addedsugars>=ADDSUGMAX then HEIX12_ADDEDSUGARS=0;
    # 		else if xhei_addedsugars<ADDSUGMIN then HEIX12_ADDEDSUGARS=10;
    # 		else HEIX12_ADDEDSUGARS=10 - (10*((xhei_addedsugars-ADDSUGMIN) / (ADDSUGMAX-ADDSUGMIN)));
    #
    xhei_addedsugars <- 100*hei_addedsugars/rikcal

    ADDSUGMIN <- 6.5
    ADDSUGMAX <- 26
    HEIX12_ADDEDSUGARS <- 10 - (10*((xhei_addedsugars-ADDSUGMIN) / (ADDSUGMAX-ADDSUGMIN)))

    HEIX12_ADDEDSUGARS[HEIX12_ADDEDSUGARS<=0] <- 0
    HEIX12_ADDEDSUGARS[HEIX12_ADDEDSUGARS>=10] <- 10

    # ** SATURATED FAT COMPONENT;
    # 	xhei_satfats=ripctsfa;
    #
    # 	SATFATSMIN=8;
    # 	SATFATSMAX=16;
    # 	if xhei_satfats>SATFATSMAX then HEIX13_SATFATS=0;
    # 		else if xhei_satfats<SATFATSMIN then HEIX13_SATFATS=10;
    # 		else HEIX13_SATFATS=10 - (10*((xhei_satfats-SATFATSMIN) / (SATFATSMAX-SATFATSMIN)));
    #
    xhei_satfats <- ripctsfa

    SATFATSMIN <- 8
    SATFATSMAX <- 16
    HEIX13_SATFATS <- 10 - (10*((xhei_satfats-SATFATSMIN) / (SATFATSMAX-SATFATSMIN)))

    HEIX13_SATFATS[HEIX13_SATFATS<=0] <- 0
    HEIX13_SATFATS[HEIX13_SATFATS>=10] <- 10

    # end; /* KCAL > 0 */
    #
    # run;

    # }

    # data hei0409;
    # set hei0409;
    #
    # label
    # hei_totveg='total vegetable servings in cup equivalents'
    # hei_greensbeans='greens and beans servings in cup equivalents'
    # hei_totfruit='total fruit servings in cup equivalents'
    # hei_wholefruit='whole fruit servings in cup equivalents'
    # hei_dairy='dairy servings in cup equivalents'
    # hei_wholegrains='whole grain servings in ounce equivalents'
    # hei_totproteins='total protein servings in ounce equivalents'
    # hei_seafoodplantprot='sea food and plant protein servings in ounce equivalents'
    # hei_refinedgrains='refined grains in ounce equivalents'
    # hei_sodium ='sodium intake in grams'
    # hei_addedsugars='kcal from added sugars'
    #
    # xhei_totfruit='total fruit servings in cup equivalents PER 1000 KCAL'
    # xhei_wholefruit='whole fruit servings in cup equivalents PER 1000 KCAL'
    # xhei_totveg='total vegetable servings in cup equivalents PER 1000 KCAL'
    # xhei_greensbeans='greens and beans servings in cup equivalents PER 1000 KCAL'
    # xhei_wholegrains='whole grain servings in ounce equivalents PER 1000 KCAL'
    # xhei_dairy='dairy servings in cup equivalents PER 1000 KCAL'
    # xhei_totproteins='protein servings in ounce equivalents PER 1000 KCAL'
    # xhei_seafoodplantprot='sea food and plant protein servings in ounce equivalents PER 1000 KCAL'
    # xhei_refinedgrains='refined grains in ounce equivalents PER 1000 KCAL'
    # xhei_sodium='sodium intake in grams PER 1000 KCAL'
    # xhei_fatacid='fatty acid ratio'
    # xhei_addedsugars='percent kcal from added sugars'
    # xhei_satfats='percent saturated fatty acids';
    #
    # label HEIX1_TOTALVEG='HEI-2015 COMPONENT 1 TOTAL VEGETABLES (0-5)'
    # HEIX2_GREEN_AND_BEAN='HEI-2015 COMPONENT 2 GREENS AND BEANS (0-5)'
    # HEIX3_TOTALFRUIT='HEI-2015 COMPONENT 3 TOTAL FRUIT (0-5)'
    # HEIX4_WHOLEFRUIT='HEI-2015 COMPONENT 4 WHOLE FRUIT (0-5)'
    # HEIX5_WHOLEGRAIN='HEI-2015 COMPONENT 5 WHOLE GRAINS (0-10)'
    # HEIX6_TOTALDAIRY='HEI-2015 COMPONENT 6 DAIRY (0-10)'
    # HEIX7_TOTPROT='HEI-2015 COMPONENT 7 TOTAL PROTEIN FOODS (0-5)'
    # HEIX8_SEAPLANT_PROT='HEI-2015 COMPONENT 8 SEAFOOD AND PLANT PROTEIN (0-5)'
    # HEIX9_FATTYACID='HEI-2015 COMPONENT 9 FATTY ACID RATIO (0-10)'
    # HEIX10_SODIUM='HEI-2010 COMPONENT 10 SODIUM (0-10)'
    # HEIX11_REFINEDGRAIN='HEI-2015 COMPONENT 11 REFINED GRAINS (0-10)'
    # HEIX12_ADDEDSUGARS='HEI-2015 COMPONENT 12 ADDED SUGARS (0-10)'
    # HEIX13_SATFATS='HEI-2015 COMPONENT 13 SATURATED FATS (0-10)';
    # run;
    #
    # * DELETE IRRELEVANT VARIABLES;
    # data hei0409; set hei0409;
    # drop FARMIN FARMAX RGMIN RGMAX  SODMIN SODMAX
    # ADDSUGMIN ADDSUGMAX SATFATSMIN SATFATSMAX;
    # run;
    #
    # proc print; title 'scores by individual ID number';
    # var cpartid rikcal
    # HEIX1_TOTALVEG HEIX2_GREEN_AND_BEAN HEIX3_TOTALFRUIT HEIX4_WHOLEFRUIT HEIX5_WHOLEGRAIN
    # HEIX6_TOTALDAIRY HEIX7_TOTPROT HEIX8_SEAPLANT_PROT HEIX9_FATTYACID HEIX10_SODIUM HEIX11_REFINEDGRAIN
    # HEIX12_ADDEDSUGARS HEIX13_SATFATS;
    # run;
    #
    # * KEEP HEI RELATED VARIABLES;
    # data keephei; set hei0409;
    # keep cpartid rikcal
    # HEIX1_TOTALVEG HEIX2_GREEN_AND_BEAN HEIX3_TOTALFRUIT HEIX4_WHOLEFRUIT HEIX5_WHOLEGRAIN
    # HEIX6_TOTALDAIRY HEIX7_TOTPROT HEIX8_SEAPLANT_PROT HEIX9_FATTYACID HEIX10_SODIUM HEIX11_REFINEDGRAIN
    # HEIX12_ADDEDSUGARS HEIX13_SATFATS hei_totveg hei_greensbeans hei_totfruit hei_wholefruit hei_dairy
    # hei_wholegrains hei_totproteins hei_seafoodplantprot hei_refinedgrains hei_sodium hei_addedsugars
    # xhei_totfruit xhei_wholefruit xhei_totveg xhei_greensbeans xhei_wholegrains xhei_dairy xhei_totproteins
    # xhei_seafoodplantprot xhei_refinedgrains xhei_fatacid xhei_sodium xhei_addedsugars xhei_satfats;
    # run;
    #
    # * CREATE PERMANENT DATASET WITH HEI SCORES FOR EACH ID;
    # data saslib.NDSRHEIBYID;
    # set keephei;
    # run;

    keephei <- data.frame(Participant.ID,

                          HEIX1_TOTALVEG,       # 'HEI-2015 COMPONENT 1 TOTAL VEGETABLES (0-5)'
                          HEIX2_GREEN_AND_BEAN, # 'HEI-2015 COMPONENT 2 GREENS AND BEANS (0-5)'
                          HEIX3_TOTALFRUIT,     # 'HEI-2015 COMPONENT 3 TOTAL FRUIT (0-5)'
                          HEIX4_WHOLEFRUIT,     # 'HEI-2015 COMPONENT 4 WHOLE FRUIT (0-5)'
                          HEIX5_WHOLEGRAIN,     # 'HEI-2015 COMPONENT 5 WHOLE GRAINS (0-10)'
                          HEIX6_TOTALDAIRY,     # 'HEI-2015 COMPONENT 6 DAIRY (0-10)'
                          HEIX7_TOTPROT,        # 'HEI-2015 COMPONENT 7 TOTAL PROTEIN FOODS (0-5)'
                          HEIX8_SEAPLANT_PROT,  # 'HEI-2015 COMPONENT 8 SEAFOOD AND PLANT PROTEIN (0-5)'
                          HEIX9_FATTYACID,      # 'HEI-2015 COMPONENT 9 FATTY ACID RATIO (0-10)'
                          HEIX10_SODIUM,        # 'HEI-2010 COMPONENT 10 SODIUM (0-10)'
                          HEIX11_REFINEDGRAIN,  # 'HEI-2015 COMPONENT 11 REFINED GRAINS (0-10)'
                          HEIX12_ADDEDSUGARS,   # 'HEI-2015 COMPONENT 12 ADDED SUGARS (0-10)'
                          HEIX13_SATFATS,       # 'HEI-2015 COMPONENT 13 SATURATED FATS (0-10)'
                          HEIX14_TOTAL = HEIX1_TOTALVEG + HEIX2_GREEN_AND_BEAN +
                              HEIX3_TOTALFRUIT + HEIX4_WHOLEFRUIT + HEIX5_WHOLEGRAIN +
                              HEIX6_TOTALDAIRY + HEIX7_TOTPROT + HEIX8_SEAPLANT_PROT +
                              HEIX9_FATTYACID + HEIX10_SODIUM + HEIX11_REFINEDGRAIN +
                              HEIX12_ADDEDSUGARS + HEIX13_SATFATS,

                          hei_totveg,           # 'total vegetable servings in cup equivalents'
                          hei_greensbeans,      # 'greens and beans servings in cup equivalents'
                          hei_totfruit,         # 'total fruit servings in cup equivalents'
                          hei_wholefruit,       # 'whole fruit servings in cup equivalents'
                          hei_dairy,            # 'dairy servings in cup equivalents'
                          hei_wholegrains,      # 'whole grain servings in ounce equivalents'
                          hei_totproteins,      # 'total protein servings in ounce equivalents'
                          hei_seafoodplantprot, # 'sea food and plant protein servings in ounce equivalents'
                          hei_refinedgrains,    # 'refined grains in ounce equivalents'
                          hei_sodium,           # 'sodium intake in grams'
                          hei_addedsugars,      # 'kcal from added sugars'

                          rikcal,   # total Energy (kilocalories) in kcal
                          ripfa,    # "Total.Polyunsaturated.Fatty.Acids..PUFA...g.", # ripfa (Column 30 Total Polyunsaturated Fatty Acids (PUFA) g)
                          rimfa,    # "Total.Monounsaturated.Fatty.Acids..MUFA...g.", # rimfa (Column 29 Total Monounsaturated Fatty Acids (MUFA) g)
                          risfa,    # "Total.Saturated.Fatty.Acids..SFA...g.",  # risfa (Column 28 Total Saturated Fatty Acids (SFA) g)
                          ripctsfa, # "X..Calories.from.SFA", # ripctsfa (% Calories from Sat. Fat)
                          water, calcium, fiber,
                          greens, redorange, starchy, beans,
                          NDSRvlnf, NDSRfvl, NDSRfvlnf, NDSRssb,

                          xhei_totfruit,         # 'total fruit servings in cup equivalents PER 1000 KCAL'
                          xhei_wholefruit,       # 'whole fruit servings in cup equivalents PER 1000 KCAL'
                          xhei_totveg,           # 'total vegetable servings in cup equivalents PER 1000 KCAL'
                          xhei_greensbeans,      # 'greens and beans servings in cup equivalents PER 1000 KCAL'
                          xhei_wholegrains,      # 'whole grain servings in ounce equivalents PER 1000 KCAL'
                          xhei_dairy,            # 'dairy servings in cup equivalents PER 1000 KCAL'
                          xhei_totproteins,      # 'protein servings in ounce equivalents PER 1000 KCAL'
                          xhei_seafoodplantprot, # 'sea food and plant protein servings in ounce equivalents PER 1000 KCAL'
                          xhei_refinedgrains,    # 'refined grains in ounce equivalents PER 1000 KCAL'
                          xhei_fatacid,          # 'fatty acid ratio'
                          xhei_sodium,           # 'sodium intake in grams PER 1000 KCAL'
                          xhei_addedsugars,      # 'percent kcal from added sugars'
                          xhei_satfats           # 'percent saturated fatty acids'
    )
    return(keephei)
}
