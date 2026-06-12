rm(list = ls())

###################################################
############ ESS data preparation  ################
###################################################

# This script loads ESS rounds 1-11 and merges them with party-position
# data from the Manifesto Project (MPD) to identify green-party voters.
# It also harmonizes education across countries (eisced2), derives
# occupational class using the Oesch class scheme (class16/class8),
# constructs a political attitudes index (polindex), and groups
# countries into regions. The combined, recoded dataset is saved at
# the end of the script for use in the analysis and appendix files.

# Packages
library(tidyverse)
library(haven)
library(labelled)
library(dplyr)
library(readxl)

# WD
setwd("C:/Users/Ingvild/OneDrive/EUI/Dissertation/Supplementary Material/Paper 1")

#### Load data + Education ####
#### Round 1 ####
df1 <- read_sav("01_Data/ESS/ESS round 1/ESS1e06_7.sav")
df_ref <- read_excel("01_Data/MPD/MPD_ESS_data_round_1.xlsx")
prtv_columns <- grep("^prtv", names(df1), value = TRUE)

df <- df1 %>%
  select(cntry, idno, all_of(prtv_columns))

df_prtv <- df %>%
  pivot_longer(cols = all_of(prtv_columns), 
               names_to = "variable", 
               values_to = "value") %>%
  drop_na(value)  

df_prtv <- df_prtv %>%
  left_join(df_ref %>% select(variable, value, pervote, per416, per501, per503, per504, per603, per604, per706, rile, welfare),
            by = c("variable", "value")) 

df1 <- df1 %>%
  left_join(df_prtv, by = c("cntry", "idno"))

# For each country-specific party-choice variable (prtvt..), recode the
# green party's value to 2 and all other parties to 1, then replace
# missing values with 0. Summing across these variables (greenvote)
# identifies green-party voters (1) vs. other voters (0). The same
# pattern is repeated for each ESS round below, with country-specific
# variable names and green-party codes.
df1 <- df1 %>%
  mutate(prtvtat	=	ifelse(	prtvtat	==	4	,	2,1),
         prtvtbe	=	ifelse(	prtvtbe	==	1	,	2,
                           ifelse(	prtvtbe	==	11	,	2, 1)),
         prtvtcz	=	ifelse(	prtvtcz	==	13	,	2,1),
         prtvtfi	=	ifelse(	prtvtfi	==	8	,	2,1),
         prtvtfr	=	ifelse(	prtvtfr	==	14	,	2,1),
         prtvde2	=	ifelse(	prtvde2	==	3	,	2,1),
         prtvtie	=	ifelse(	prtvtie	==	5	,	2,1),
         prtvtit	=	ifelse(	prtvtit	==	4	,	2,1),
         prtvtlu	=	ifelse(	prtvtlu	==	4	,	2,1),
         prtvtnl	=	ifelse(	prtvtnl	==	6	,	2,1),
         prtvtsi	=	ifelse(	prtvtsi	==	8	,	2,1),
         prtvtes	= ifelse (prtvtes	== 6, 2, 1),
         prtvtse	=	ifelse(	prtvtse	==	4	,	2,1),
         prtvtch	=	ifelse(	prtvtch	==	10,	2, 1),
         prtvtgb	=	ifelse(	prtvtgb	==	6	,	2,1),
         prtvtat	=	replace_na(	prtvtat	,	0),
         prtvtbe	=	replace_na(	prtvtbe	,	0),
         prtvtcz	=	replace_na(	prtvtcz	,	0),
         prtvtfi	=	replace_na(	prtvtfi	,	0),
         prtvtfr	=	replace_na(	prtvtfr	,	0),
         prtvde2	=	replace_na(	prtvde2	,	0),
         prtvtie	=	replace_na(	prtvtie	,	0),
         prtvtit	=	replace_na(	prtvtit	,	0),
         prtvtlu	=	replace_na(	prtvtlu	,	0),
         prtvtnl	=	replace_na(	prtvtnl	,	0),
         prtvtes = replace_na(prtvtes, 0),
         prtvtsi	=	replace_na(	prtvtsi	,	0),
         prtvtse	=	replace_na(	prtvtse	,	0),
         prtvtch	=	replace_na(	prtvtch	,	0),
         prtvtgb	=	replace_na(	prtvtgb	,	0),
         greenvote = prtvtat	+
           prtvtbe	+
           prtvtcz	+
           prtvtfi	+
           prtvtfr	+
           prtvde2	+
           prtvtie	+
           prtvtit	+
           prtvtlu	+
           prtvtnl	+
           prtvtsi	+
           prtvtes	+
           prtvtse	+
           prtvtch	+
           prtvtgb, 
         greenvote = na_if(greenvote, 0),
         greenvote = ifelse(greenvote == 2, 1,
                            ifelse(greenvote == 1, 0, NA)),
         # Harmonize education across countries: recode the ESS eisced
         # variable (and country-specific edlv.. variables, which use
         # different scales) into a common low (1) vs. high (2) education
         # measure, combined below into eisced2.
         eisced = ifelse(eisced == 55, NA, eisced),
         eisced = ifelse(eisced >= 1 & eisced <= 4, 1,
                         ifelse(eisced >= 5, 2, 0)),
         edlvfr = ifelse(edlvfr >= 0 & edlvfr <= 5, 1,
                         ifelse(edlvfr >= 6, 2, NA)),
         edlvgb = ifelse(edlvgb >= 0 & edlvgb <= 3, 1,
                         ifelse(edlvgb >= 4, 2, NA)),
         edlvgr = ifelse(edlvgr >= 0 & edlvgr <= 4, 1,
                         ifelse(edlvgr >= 5, 2, NA)),
         edlvie = ifelse(edlvie >= 0 & edlvie <= 4, 1,
                         ifelse(edlvie >= 5, 2, NA)),
         edlvpt = ifelse(edlvpt >= 0 & edlvpt <= 5, 1,
                         ifelse(edlvpt >= 6, 2, NA)),
         edlvse = ifelse(edlvse >= 0 & edlvse <= 9, 1,
                         ifelse(edlvse >= 10, 2, NA)), 
         edlvfr	=	replace_na(edlvfr	,	0),
         edlvgb = replace_na(edlvgb, 0),
         edlvgr = replace_na(edlvgr, 0),
         edlvie = replace_na(edlvie, 0),
         edlvpt = replace_na(edlvpt, 0),
         edlvse = replace_na(edlvse, 0),
         eisced2 = eisced + edlvfr + edlvgb + edlvgr + 
           edlvie + edlvpt + edlvse,
         eisced2 = na_if(eisced2, 0),
         anweight = pspwght*pweight)

#### Round 2 ####
df2 <- read_sav("01_Data/ESS/ESS round 2/ESS2e03_6.sav")
df_ref <- read_excel("01_Data/MPD/MPD_ESS_data_round_2.xlsx")
prtv_columns <- grep("^prtv", names(df2), value = TRUE)

df <- df2 %>%
  select(cntry, idno, all_of(prtv_columns)) 

df_prtv <- df %>%
  pivot_longer(cols = all_of(prtv_columns), 
               names_to = "variable", 
               values_to = "value") %>%
  drop_na(value)  

df_prtv <- df_prtv %>%
  left_join(df_ref %>% select(variable, value, pervote, per416, per501, per503, per504, per603, per604, per706, rile, welfare),
            by = c("variable", "value")) 

df2 <- df2 %>%
  left_join(df_prtv, by = c("cntry", "idno"))

df2 <- df2 %>%
  mutate(anweight = pspwght*pweight,
         prtvtat	=	ifelse(	prtvtat	==	4, 2, 1),
         prtvtabe = ifelse(	prtvtabe	==	1, 2,
                            ifelse(	prtvtabe	==	10,	2, 1)),
         prtvtcz	=	ifelse(	prtvtcz	==	13,	2, 1),
         prtvtfi	=	ifelse(	prtvtfi	==	8, 2 ,1),
         prtvtfr	=	ifelse(	prtvtfr	==	14,	2, 1),
         prtvade2 = ifelse(	prtvade2	==	3, 2, 1),
         prtvtis	=	ifelse(	prtvtis	==	7	,	2, 1),
         prtvtie	=	ifelse(	prtvtie	==	5	,	2, 1),
         prtvtlu	=	ifelse(	prtvtlu	==	4	,	2,1),
         prtvtanl =	ifelse(	prtvtanl ==	6	,	2, 1),
         prtvtaes = ifelse(	prtvtaes	==	6	,	2, 1),
         prtvtse	=	ifelse(	prtvtse	==	4	,	2,1),
         prtvtch	=	ifelse(	prtvtch	==	10	,	2,1),
         prtvtgb	=	ifelse(	prtvtgb	==	6	,	2,1),
         prtvtat	=	replace_na(	prtvtat	,	0),
         prtvtabe	=	replace_na(	prtvtabe	,	0),
         prtvtcz	=	replace_na(	prtvtcz	,	0),
         prtvtfi	=	replace_na(	prtvtfi	,	0),
         prtvtfr	=	replace_na(	prtvtfr	,	0),
         prtvade2	=	replace_na(	prtvade2	,	0),
         prtvtis	=	replace_na(	prtvtis	,	0),
         prtvtie	=	replace_na(	prtvtie	,	0),
         prtvtlu	=	replace_na(	prtvtlu	,	0),
         prtvtanl	=	replace_na(	prtvtanl	,	0),
         prtvtaes = replace_na(prtvtaes, 0),
         prtvtse	=	replace_na(	prtvtse	,	0),
         prtvtch	=	replace_na(	prtvtch	,	0),
         prtvtgb	=	replace_na(	prtvtgb	,	0),
         greenvote = prtvtat	+
           prtvtabe	+
           prtvtcz	+
           prtvtfi	+
           prtvtfr	+
           prtvade2	+
           prtvtis	+
           prtvtie	+
           prtvtlu	+
           prtvtanl	+
           prtvtaes	+
           prtvtse	+
           prtvtch	+
           prtvtgb, 
         greenvote = na_if(greenvote, 0),
         greenvote = ifelse(greenvote == 2, 1,
                            ifelse(greenvote == 1, 0, NA)))

df2$eisced  <- ifelse(df2$eisced == 55, NA, df2$eisced)
df2$eisced <- ifelse(df2$eisced >= 1 & df2$eisced <= 4, 1,
                     ifelse(df2$eisced >= 5, 2, 0))

df2$edlvfr <- ifelse(df2$edlvfr >= 0 & df2$edlvfr <= 5, 1,
                     ifelse(df2$edlvfr >= 6, 2, NA))
df2$edlvfr[is.na(df2$edlvfr)] <- 0

df2$edlvagb  <- ifelse(df2$edlvagb >= 0 & df2$edlvagb <= 3, 1,
                       ifelse(df2$edlvagb >= 4, 2, NA))
df2$edlvagb[is.na(df2$edlvagb)] <- 0

df2$edlvgr  <- ifelse(df2$edlvgr >= 0 & df2$edlvgr <= 4, 1,
                      ifelse(df2$edlvgr >= 5, 2, NA))
df2$edlvgr[is.na(df2$edlvgr)] <- 0

df2$edlvie  <- ifelse(df2$edlvie >= 0 & df2$edlvie <= 4, 1,
                      ifelse(df2$edlvie >= 5, 2, NA))
df2$edlvie[is.na(df2$edlvie)] <- 0

df2$edlvait  <- ifelse(df2$edlvait >= 0 & df2$edlvait <= 4, 1,
                       ifelse(df2$edlvait >= 5, 2, NA))
df2$edlvait[is.na(df2$edlvait)] <- 0

df2$edlvpt  <- ifelse(df2$edlvpt >= 0 & df2$edlvpt <= 5, 1,
                      ifelse(df2$edlvpt >= 6, 2, NA))
df2$edlvpt[is.na(df2$edlvpt)] <- 0

df2$edlvse  <- ifelse(df2$edlvse >= 0 & df2$edlvse <= 9, 1,
                      ifelse(df2$edlvse >= 10, 2, NA))
df2$edlvse[is.na(df2$edlvse)] <- 0

df2$edlvua  <- ifelse(df2$edlvua >= 0 & df2$edlvua <= 3, 1,
                      ifelse(df2$edlvua >= 4, 2, NA))
df2$edlvua[is.na(df2$edlvua)] <- 0

df2$eisced2 <- (df2$eisced + df2$edlvfr + df2$edlvagb + df2$edlvgr + df2$edlvie + 
                  df2$edlvait + df2$edlvpt + df2$edlvse + df2$edlvua)
df2$eisced2 <- ifelse(df2$eisced2 == 0, NA, df2$eisced2)

#### Round 3 ####
df3 <- read_sav("01_Data/ESS/ESS round 3/ESS3e03_7.sav")
df_ref <- read_excel("01_Data/MPD/MPD_ESS_data_round_3.xlsx")
prtv_columns <- grep("^prtv", names(df3), value = TRUE)

df <- df3 %>%
  select(cntry, idno, all_of(prtv_columns))

df_prtv <- df %>%
  pivot_longer(cols = all_of(prtv_columns), 
               names_to = "variable", 
               values_to = "value") %>%
  drop_na(value)  

df_prtv <- df_prtv %>%
  left_join(df_ref %>% select(variable, value, pervote, per416, per501, per503, per504, per603, per604, per706, rile, welfare),
            by = c("variable", "value")) 

df3 <- df3 %>%
  left_join(df_prtv, by = c("cntry", "idno"))

df3 <- df3 %>%
  mutate(anweight = pspwght*pweight,
         prtvtaat	=	ifelse(	prtvtaat	==	5	,	2,1),
         prtvtabe	=	ifelse(	prtvtabe	==	1	,	2,
                            ifelse(	prtvtabe	==	10	,	2,1)),
         prtvtcy	=	ifelse(	prtvtcy	==	5	,	2,1),
         prtvtadk	=	ifelse(	prtvtadk	==	5	,	2,1),
         prtvtfi	=	ifelse(	prtvtfi	==	8	,	2,1),
         prtvtafr	=	ifelse(	prtvtafr	==	14	,	2,1),
         prtvbde2	=	ifelse(	prtvbde2	==	3	,	2,1),
         prtvtie	=	ifelse(	prtvtie	==	5	,	2,1),
         prtvtbnl	=	ifelse(	prtvtbnl	==	6	,	2,1),
         prtvtro	=	ifelse(	prtvtro	==	3	,	2,1),
         prtvtaes	=	ifelse(	prtvtaes	==	6	,	2,1),
         prtvtse	=	ifelse(	prtvtse	==	4	,	2,1),
         prtvtach	=	ifelse(	prtvtach	==	10	,	2, 1),
         prtvtagb	=	ifelse(	prtvtagb	==	6	,	2,1),
         prtvtaat	=	replace_na(	prtvtaat	,	0),
         prtvtabe	=	replace_na(	prtvtabe	,	0),
         prtvtbg	=	replace_na(	prtvtbg	,	0),
         prtvtcy	=	replace_na(	prtvtcy	,	0),
         prtvtadk	=	replace_na(	prtvtadk	,	0),
         prtvtfi	=	replace_na(	prtvtfi	,	0),
         prtvtafr	=	replace_na(	prtvtafr	,	0),
         prtvbde2	=	replace_na(	prtvbde2	,	0),
         prtvtie	=	replace_na(	prtvtie	,	0),
         prtvtbnl	=	replace_na(	prtvtbnl	,	0),
         prtvtro	=	replace_na(	prtvtro	,	0),
         prtvtaes	=	replace_na(	prtvtaes	,	0),
         prtvtse	=	replace_na(	prtvtse	,	0),
         prtvtach	=	replace_na(	prtvtach	,	0),
         prtvtagb	=	replace_na(	prtvtagb	,	0),
         greenvote = prtvtaat	+
           prtvtabe	+
           prtvtcy	+
           prtvtadk	+
           prtvtfi	+
           prtvtafr	+
           prtvbde2	+
           prtvtie	+
           prtvtbnl	+
           prtvtro	+
           prtvtaes +
           prtvtse	+
           prtvtach	+
           prtvtagb,
         greenvote = na_if(greenvote, 0),
         greenvote = ifelse(greenvote == 2, 1,
                            ifelse(greenvote == 1, 0, NA)))

df3$eisced  <- ifelse(df3$eisced == 55, NA, df3$eisced)
df3$eisced2 <- ifelse(df3$eisced >= 1 & df3$eisced <= 4, 1,
                      ifelse(df3$eisced >= 5, 2, 0))

df3$edlvbg <- ifelse(df3$edlvbg >= 0 & df3$edlvbg <= 4, 1,
                     ifelse(df3$edlvbg >= 5, 2, NA))
df3$edlvbg[is.na(df3$edlvbg)] <- 0

df3$edlvcy <- ifelse(df3$edlvcy >= 0 & df3$edlvcy <= 4, 1,
                     ifelse(df3$edlvcy >= 5, 2, NA))
df3$edlvcy[is.na(df3$edlvcy)] <- 0

df3$edlvgb  <- ifelse(df3$edlvgb >= 0 & df3$edlvgb <= 3, 1,
                      ifelse(df3$edlvgb >= 4, 2, NA))
df3$edlvgb[is.na(df3$edlvgb)] <- 0

df3$edlvaie  <- ifelse(df3$edlvaie >= 0 & df3$edlvaie <= 4, 1,
                       ifelse(df3$edlvaie >= 5, 2, NA))
df3$edlvaie[is.na(df3$edlvaie)] <- 0

df3$edlvapt  <- ifelse(df3$edlvapt >= 0 & df3$edlvapt <= 5, 1,
                       ifelse(df3$edlvapt >= 6, 2, NA))
df3$edlvapt[is.na(df3$edlvapt)] <- 0

df3$edlvase  <- ifelse(df3$edlvase >= 0 & df3$edlvase <= 9, 1,
                       ifelse(df3$edlvase >= 10, 2, NA))
df3$edlvase[is.na(df3$edlvase)] <- 0

df3$edlvua  <- ifelse(df3$edlvua >= 0 & df3$edlvua <= 3, 1,
                      ifelse(df3$edlvua >= 4, 2, NA))
df3$edlvua[is.na(df3$edlvua)] <- 0

df3$eisced2 <- (df3$eisced2 + df3$edlvbg + df3$edlvcy + df3$edlvgb + 
                  df3$edlvaie + df3$edlvapt + df3$edlvase + df3$edlvua)
df3$eisced2 <- ifelse(df3$eisced2 == 0, NA, df3$eisced2)


#### Round 4 ####
df4 <- read_sav("01_Data/ESS/ESS round 4/ESS4e04_6.sav")
df_ref <- read_excel("01_Data/MPD/MPD_ESS_data_round_4.xlsx")
prtv_columns <- grep("^prtv", names(df4), value = TRUE)

df <- df4 %>%
  select(cntry, idno, all_of(prtv_columns)) 

df_prtv <- df %>%
  pivot_longer(cols = all_of(prtv_columns), 
               names_to = "variable", 
               values_to = "value") %>%
  drop_na(value)  

df_prtv <- df_prtv %>%
  left_join(df_ref %>% select(variable, value, pervote, per416, per501, per503, per504, per603, per604, per706, rile, welfare),
            by = c("variable", "value")) 

df4 <- df4 %>%
  left_join(df_prtv, by = c("cntry", "idno"))

df4 <- df4 %>%
  mutate(anweight = pspwght*pweight,
         prtvtaat = ifelse(prtvtaat == 5, 2, 1),
         prtvtbbe	=	ifelse(	prtvtbbe	==	1	,	2,
                            ifelse(	prtvtbbe	==	10	,	2,1)),
         prtvtcy	=	ifelse(	prtvtcy	==	5	,	2,1),
         prtvtacz	=	ifelse(	prtvtacz	==	3	,	2,1),
         prtvtbdk	=	ifelse(	prtvtbdk	==	4	,	2,1),
         prtvtbee	=	ifelse(	prtvtbee	==	6	,	2,1),
         prtvtafi	=	ifelse(	prtvtafi	==	7	,	2,1),
         prtvtbfr	=	ifelse(	prtvtbfr	==	12	,	2,1),
         prtvbde2	=	ifelse(	prtvbde2	==	3	,	2,1),
         prtvtie	=	ifelse(	prtvtie	==	5	,	2,1),
         prtvtcnl	=	ifelse(	prtvtcnl	==	6	,	2,
                            ifelse(	prtvtcnl	==	12	,	2, 1)),
         prtvtse	=	ifelse(	prtvtse	==	4	,	2,1),
         prtvtbch	=	ifelse(	prtvtbch	==	8	,	2,
                            ifelse(	prtvtbch	==	9	,	2,1)),
         prtvtgb	=	ifelse(	prtvtgb	==	6	,	2,1),
         prtvtaat = replace_na(prtvtaat, 0),
         prtvtbbe	=	replace_na(	prtvtbbe	,	0),
         prtvtcy	=	replace_na(	prtvtcy	,	0),
         prtvtacz	=	replace_na(	prtvtacz	,	0),
         prtvtbdk	=	replace_na(	prtvtbdk	,	0),
         prtvtbee	=	replace_na(	prtvtbee	,	0),
         prtvtafi	=	replace_na(	prtvtafi	,	0),
         prtvtbfr	=	replace_na(	prtvtbfr	,	0),
         prtvbde2	=	replace_na(	prtvbde2	,	0),
         prtvtie	=	replace_na(	prtvtie	,	0),
         prtvtcnl	=	replace_na(	prtvtcnl	,	0),
         prtvtse	=	replace_na(	prtvtse	,	0),
         prtvtbch	=	replace_na(	prtvtbch	,	0),
         prtvtgb	=	replace_na(	prtvtgb	,	0),
         greenvote =  prtvtaat +
           prtvtbbe	+
           prtvtcy	+
           prtvtacz	+
           prtvtbdk	+
           prtvtbee	+
           prtvtafi	+
           prtvtbfr	+
           prtvbde2	+
           prtvtie	+
           prtvtcnl	+
           prtvtse	+
           prtvtbch	+
           prtvtgb,
         greenvote = na_if(greenvote, 0),
         greenvote = ifelse(greenvote == 2, 1,
                            ifelse(greenvote == 1, 0, NA)))

df4$eisced  <- ifelse(df4$eisced == 55, NA, df4$eisced)
df4$eisced2 <- ifelse(df4$eisced >= 1 & df4$eisced <= 4, 1,
                      ifelse(df4$eisced >= 5, 2, 0))

df4$edlvat <- ifelse(df4$edlvat >= 0 & df4$edlvat <= 3, 1,
                     ifelse(df4$edlvat >= 4, 2, NA))
df4$edlvat[is.na(df4$edlvat)] <- 0

df4$edlvacy <- ifelse(df4$edlvacy >= 0 & df4$edlvacy <= 3, 1,
                      ifelse(df4$edlvacy >= 4, 2, NA))
df4$edlvacy[is.na(df4$edlvacy)] <- 0

df4$edlvgb  <- ifelse(df4$edlvgb >= 0 & df4$edlvgb <= 3, 1,
                      ifelse(df4$edlvgb >= 4, 2, NA))
df4$edlvgb[is.na(df4$edlvgb)] <- 0

df4$edlvagr  <- ifelse(df4$edlvagr >= 0 & df4$edlvagr <= 3, 1,
                       ifelse(df4$edlvagr >= 4, 2, NA))
df4$edlvagr[is.na(df4$edlvagr)] <- 0

df4$edlvbie  <- ifelse(df4$edlvbie >= 0 & df4$edlvbie <= 4, 1,
                       ifelse(df4$edlvbie >= 5, 2, NA))
df4$edlvbie[is.na(df4$edlvbie)] <- 0

df4$edlvail  <- ifelse(df4$edlvail >= 0 & df4$edlvail <= 5, 1,
                       ifelse(df4$edlvail >= 6, 2, NA))
df4$edlvail[is.na(df4$edlvail)] <- 0

df4$edlvlt  <- ifelse(df4$edlvlt >= 0 & df4$edlvlt <= 7, 1,
                      ifelse(df4$edlvlt >= 8, 2, NA))
df4$edlvlt[is.na(df4$edlvlt)] <- 0

df4$edlvase  <- ifelse(df4$edlvase >= 0 & df4$edlvase <= 9, 1,
                       ifelse(df4$edlvase >= 10, 2, NA))
df4$edlvase[is.na(df4$edlvase)] <- 0

df4$edlvtr  <- ifelse(df4$edlvtr >= 0 & df4$edlvtr <= 3, 1,
                      ifelse(df4$edlvtr >= 4, 2, NA))
df4$edlvtr[is.na(df4$edlvtr)] <- 0

df4$eisced2 <- (df4$eisced2 + df4$edlvat + df4$edlvacy + df4$edlvgb + df4$edlvagr +
                  df4$edlvbie + df4$edlvail + df4$edlvlt + df4$edlvase + df4$edlvtr)
df4$eisced2 <- ifelse(df4$eisced2 == 0, NA, df4$eisced2)


#### Round 5 ####
df5 <- read_sav("01_Data/ESS/ESS round 5/ESS5e03_5.sav")
df_ref <- read_excel("01_Data/MPD/MPD_ESS_data_round_5.xlsx")
prtv_columns <- grep("^prtv", names(df5), value = TRUE)

df <- df5 %>%
  select(cntry, idno, all_of(prtv_columns)) 

df_prtv <- df %>%
  pivot_longer(cols = all_of(prtv_columns), 
               names_to = "variable", 
               values_to = "value") %>%
  drop_na(value)  

df_prtv <- df_prtv %>%
  left_join(df_ref %>% select(variable, value, pervote, per416, per501, per503, per504, per603, per604, per706, rile, welfare),
            by = c("variable", "value")) 

df5 <- df5 %>%
  left_join(df_prtv, by = c("cntry", "idno"))

df5 <- remove_var_label(df5)
df5 <- remove_val_labels(df5)
df5 <- df5 %>% 
  mutate(anweight = pspwght*pweight,
         prtvtcbe	=	ifelse(	prtvtcbe	==	1, 2,
                            ifelse(	prtvtcbe	==	10	,	2,1)),
         prtvtbbg = ifelse( prtvtbbg == 11, 2, 1),
         prtvtcy	=	ifelse(	prtvtcy	==	5	,	2,1),
         prtvtbcz	=	ifelse(	prtvtbcz	==	3	,	2,1),
         prtvtbdk	=	ifelse(	prtvtbdk	==	4	,	2,1),
         prtvtcee	=	ifelse(	prtvtcee	==	6	,	2,1),
         prtvtbfi	=	ifelse(	prtvtbfi	==	13	,	2,1),
         prtvtbfr	=	ifelse(	prtvtbfr	==	12	,	2,1),
         prtvcde2	=	ifelse(	prtvcde2	==	3	,	2,1),
         prtvtcgr = ifelse(prtvtcgr == 6, 2, 1),
         prtvtchu	=	ifelse(	prtvtchu	==	13	,	2,1),
         prtvtaie	=	ifelse(	prtvtaie	==	3	,	2,1),
         prtvtdnl	=	ifelse(	prtvtdnl	==	7	,	2,
                            ifelse(	prtvtdnl	==	10	,	2,1)),
         prtvtase	=	ifelse(	prtvtase	==	4	,	2,1),
         prtvtcch	=	ifelse(	prtvtcch	==	8	,	2,
                            ifelse(	prtvtcch	==	9	,	2,1)),
         prtvtgb	=	ifelse(	prtvtgb	==	6	,	2,1),
         prtvtcbe	=	replace_na(	prtvtcbe	,	0),
         prtvtbbg = replace_na(prtvtbbg, 0),
         prtvtcy	=	replace_na(	prtvtcy	,	0),
         prtvtbcz	=	replace_na(	prtvtbcz	,	0),
         prtvtbdk	=	replace_na(	prtvtbdk	,	0),
         prtvtcee	=	replace_na(	prtvtcee	,	0),
         prtvtbfi	=	replace_na(	prtvtbfi	,	0),
         prtvtbfr	=	replace_na(	prtvtbfr	,	0),
         prtvcde2	=	replace_na(	prtvcde2	,	0),
         prtvtcgr = replace_na(prtvtcgr, 0),
         prtvtchu	=	replace_na(	prtvtchu	,	0),
         prtvtaie	=	replace_na(	prtvtaie	,	0),
         prtvtdnl	=	replace_na(	prtvtdnl	,	0),
         prtvtase	=	replace_na(	prtvtase	,	0),
         prtvtcch	=	replace_na(	prtvtcch	,	0),
         prtvtgb	=	replace_na(	prtvtgb	,	0),
         greenvote = prtvtcbe	+ prtvtbbg +
           prtvtcy	+
           prtvtbcz	+
           prtvtbdk	+
           prtvtcee	+
           prtvtbfi	+
           prtvtbfr	+
           prtvcde2	+
           prtvtcgr +
           prtvtchu	+
           prtvtaie	+
           prtvtdnl	+
           prtvtase	+
           prtvtcch	+
           prtvtgb,
         greenvote = na_if(greenvote, 0),
         greenvote = ifelse(greenvote == 2, 1,
                            ifelse(greenvote == 1, 0, NA)))

df5$eisced  <- ifelse(df5$eisced == 55, NA, df5$eisced)
df5$eisced2 <- ifelse(df5$eisced >= 1 & df5$eisced <= 4, 1,
                      ifelse(df5$eisced >= 5, 2, 0))
df5$eisced2 <- ifelse(df5$eisced2 == 0, NA, df5$eisced2)


#### Round 6 ####
df6 <- read_sav("01_Data/ESS/ESS round 6/ESS6e02_6.sav")
df_ref <- read_excel("01_Data/MPD/MPD_ESS_data_round_6.xlsx")
prtv_columns <- grep("^prtv", names(df6), value = TRUE)

table(df6$prtvtal)

df <- df6 %>%
  select(cntry, idno, all_of(prtv_columns)) 

df_prtv <- df %>%
  pivot_longer(cols = all_of(prtv_columns), 
               names_to = "variable", 
               values_to = "value") %>%
  drop_na(value)  

df_prtv <- df_prtv %>%
  left_join(df_ref %>% select(variable, value, pervote, per416, per501, per503, per504, per603, per604, per706, rile, welfare),
            by = c("variable", "value"))  

df6 <- df6 %>%
  left_join(df_prtv, by = c("cntry", "idno"))

df6 <- remove_var_label(df6)
df6 <- remove_val_labels(df6)
df6 <- df6 %>% 
  mutate(anweight = pspwght*pweight,
         prtvtcbe	=	ifelse(	prtvtcbe	==	1	,	2,
                            ifelse(	prtvtcbe	==	10	,	2,1)),
         prtvtacy	=	ifelse(	prtvtacy	==	5	,	2,1),
         prtvtcdk	=	ifelse(	prtvtcdk	==	4	,	2,1),
         prtvtdee	=	ifelse(	prtvtdee	==	6	,	2,1),
         prtvtcfi	=	ifelse(	prtvtcfi	==	12	,	2,1),
         prtvtcfr	=	ifelse(	prtvtcfr	==	12	,	2,1),
         prtvdde2	=	ifelse(	prtvdde2	==	3	,	2,1),
         prtvtdhu	=	ifelse(	prtvtdhu	==	5	,	2,1),
         prtvtais	=	ifelse(	prtvtais	==	4	,	2,1),
         prtvtaie	=	ifelse(	prtvtaie	==	3	,	2,1),
         prtvtenl	=	ifelse(	prtvtenl	==	7	,	2,
                            ifelse(	prtvtenl	==	10	,	2,1)),
         prtvtbse	=	ifelse(	prtvtbse	==	4	,	2,1),
         prtvtdch	=	ifelse(	prtvtdch	==	5	,	2,
                            ifelse(	prtvtdch	==	6	,	2,1)),
         prtvtgb	=	ifelse(	prtvtgb	==	6	,	2,1),
         prtvtcbe	=	replace_na(	prtvtcbe	,	0),
         prtvtacy	=	replace_na(	prtvtacy	,	0),
         prtvtcdk	=	replace_na(	prtvtcdk	,	0),
         prtvtdee	=	replace_na(	prtvtdee	,	0),
         prtvtcfi	=	replace_na(	prtvtcfi	,	0),
         prtvtcfr	=	replace_na(	prtvtcfr	,	0),
         prtvdde2	=	replace_na(	prtvdde2	,	0),
         prtvtdhu	=	replace_na(	prtvtdhu	,	0),
         prtvtais	=	replace_na(	prtvtais	,	0),
         prtvtaie	=	replace_na(	prtvtaie	,	0),
         prtvtenl	=	replace_na(	prtvtenl	,	0),
         prtvtbse	=	replace_na(	prtvtbse	,	0),
         prtvtdch	=	replace_na(	prtvtdch	,	0),
         prtvtgb	=	replace_na(	prtvtgb	,	0),
         greenvote = prtvtcbe	+
           prtvtacy	+
           prtvtcdk	+
           prtvtdee	+
           prtvtcfi	+
           prtvtcfr	+
           prtvdde2	+
           prtvtdhu	+
           prtvtais	+
           prtvtaie	+
           prtvtenl	+
           prtvtbse	+
           prtvtdch	+
           prtvtgb, 
         greenvote = na_if(greenvote, 0),
         greenvote = ifelse(greenvote == 2, 1,
                            ifelse(greenvote == 1, 0, NA)))

df6$eisced  <- ifelse(df6$eisced == 55, NA, df6$eisced)
df6$eisced2 <- ifelse(df6$eisced >= 1 & df6$eisced <= 4, 1,
                      ifelse(df6$eisced >= 5, 2, 0))
df6$eisced2 <- ifelse(df6$eisced2 == 0, NA, df6$eisced2)

#### Round 7 ####
df7 <- read_sav("01_Data/ESS/ESS round 7/ESS7e02_3.sav")
df_ref <- read_excel("01_Data/MPD/MPD_ESS_data_round_7.xlsx")
prtv_columns <- grep("^prtv", names(df7), value = TRUE)

df <- df7 %>%
  select(cntry, idno, all_of(prtv_columns)) 

df_prtv <- df %>%
  pivot_longer(cols = all_of(prtv_columns), 
               names_to = "variable", 
               values_to = "value") %>%
  drop_na(value)  

df_prtv <- df_prtv %>%
  left_join(df_ref %>% select(variable, value, pervote, per416, per501, per503, per504, per603, per604, per706, rile, welfare),
            by = c("variable", "value")) 

df7 <- df7 %>%
  left_join(df_prtv, by = c("cntry", "idno"))

df7 <- remove_var_label(df7)
df7 <- remove_val_labels(df7)
df7 <- df7 %>% 
  mutate(anweight = pspwght*pweight,
         prtvtbat	=	ifelse(	prtvtbat	==	5	,	2,1),
         prtvtcbe	=	ifelse(	prtvtcbe	==	1	,	2,
                            ifelse(	prtvtcbe	==	10	,	2,1)),
         prtvtcdk	=	ifelse(	prtvtcdk	==	4	,	2,1),
         prtvteee	=	ifelse(	prtvteee	==	5	,	2,1),
         prtvtcfi	=	ifelse(	prtvtcfi	==	12	,	2,1),
         prtvtcfr	=	ifelse(	prtvtcfr	==	12	,	2,1),
         prtvede2	=	ifelse(	prtvede2	==	4	,	2,1),
         prtvtehu	=	ifelse(	prtvtehu	==	3	,	2,1),
         prtvtaie	=	ifelse(	prtvtaie	==	3	,	2,1),
         prtvtfnl	=	ifelse(	prtvtfnl	==	8	,	2,
                            ifelse(	prtvtfnl	==	10	,	2,1)),
         prtvtbno	=	ifelse(	prtvtbno	==	10	,	2,1),
         prtvtbse	=	ifelse(	prtvtbse	==	4	,	2,1),
         prtvtech	=	ifelse(	prtvtech	==	5	,	2,
                            ifelse(	prtvtech	==	6	,	2,
                                    ifelse( prtvtech == 15, 2, 1))),
         prtvtbgb	=	ifelse(	prtvtbgb	==	6	,	2,1),
         prtvtbat	=	replace_na(	prtvtbat	,	0),
         prtvtcbe	=	replace_na(	prtvtcbe	,	0),
         prtvtcdk	=	replace_na(	prtvtcdk	,	0),
         prtvteee	=	replace_na(	prtvteee	,	0),
         prtvtcfi	=	replace_na(	prtvtcfi	,	0),
         prtvtcfr	=	replace_na(	prtvtcfr	,	0),
         prtvede2	=	replace_na(	prtvede2	,	0),
         prtvtehu	=	replace_na(	prtvtehu	,	0),
         prtvtaie	=	replace_na(	prtvtaie	,	0),
         prtvtfnl	=	replace_na(	prtvtfnl	,	0),
         prtvtbno	=	replace_na(	prtvtbno	,	0),
         prtvtbse	=	replace_na(	prtvtbse	,	0),
         prtvtech	=	replace_na(	prtvtech	,	0),
         prtvtbgb	=	replace_na(	prtvtbgb	,	0),
         greenvote =  prtvtbat	+
           prtvtcbe	+
           prtvtcdk	+
           prtvteee	+
           prtvtcfi	+
           prtvtcfr	+
           prtvede2	+
           prtvtehu	+
           prtvtaie	+
           prtvtfnl	+
           prtvtbno	+
           prtvtbse	+
           prtvtech	+
           prtvtbgb,
         greenvote = na_if(greenvote, 0),
         greenvote = ifelse(greenvote == 2, 1,
                            ifelse(greenvote == 1, 0, NA)))

df7$eisced  <- ifelse(df7$eisced == 55, NA, df7$eisced)
df7$eisced2 <- ifelse(df7$eisced >= 1 & df7$eisced <= 4, 1,
                      ifelse(df7$eisced >= 5, 2, 0))
df7$eisced2 <- ifelse(df7$eisced2 == 0, NA, df7$eisced2)


#### Round 8 ####
df8 <- read_sav("01_Data/ESS/ESS round 8/ESS8e02_3.sav")
df_ref <- read_excel("01_Data/MPD/MPD_ESS_data_round_8.xlsx")
prtv_columns <- grep("^prtv", names(df8), value = TRUE)

df <- df8 %>%
  select(cntry, idno, all_of(prtv_columns)) 

df_prtv <- df %>%
  pivot_longer(cols = all_of(prtv_columns), 
               names_to = "variable", 
               values_to = "value") %>%
  drop_na(value)  

df_prtv <- df_prtv %>%
  left_join(df_ref %>% select(variable, value, pervote, per416, per501, per503, per504, per603, per604, per706, rile, welfare),
            by = c("variable", "value"))  

df8 <- df8 %>%
  left_join(df_prtv, by = c("cntry", "idno"))

df8 <- remove_var_label(df8)
df8 <- remove_val_labels(df8)
df8 <- df8 %>% 
  mutate(anweight = pspwght*pweight,
         prtvtbat	=	ifelse(	prtvtbat	==	5	,	2,1),	
         prtvtcbe	=	ifelse(	prtvtcbe	==	1	,	2,	
                            ifelse(	prtvtcbe	==	10	,	2,1)),	
         prtvtfee	=	ifelse(	prtvtfee	==	5	,	2,1),	
         prtvtdfi	=	ifelse(	prtvtdfi	==	10	,	2,1),	
         prtvtcfr	=	ifelse(	prtvtcfr	==	12	,	2,1),	
         prtvede2	=	ifelse(	prtvede2	==	4	,	2,1),	
         prtvtehu	=	ifelse(	prtvtehu	==	3	,	2,1),	
         prtvtbis	=	ifelse(	prtvtbis	==	10	,	2, 1),	
         prtvtbie	=	ifelse(	prtvtbie	==	4	,	2,1),	
         prtvblt1	=	ifelse(	prtvblt1	==	12	,	2,1),	
         prtvtfnl	=	ifelse(	prtvtfnl	==	8	,	2, 
                            ifelse(	prtvtfnl	==	8	,	2,1)),	
         prtvtbno	=	ifelse(	prtvtbno	==	10	,	2,1),	
         prtvtcpt	=	ifelse(	prtvtcpt	==	15	,	2,1),
         prtvtbse	=	ifelse(	prtvtbse	==	4	,	2,1),	
         prtvtfch	=	ifelse(	prtvtfch	==	5	,	2,	
                            ifelse(	prtvtfch	==	6	,	2,1)),	
         prtvtbgb	=	ifelse(	prtvtbgb	==	6	,	2, 
                            ifelse(	prtvtbgb	==	15	,	2,1)),	
         prtvtbat	=	replace_na(	prtvtbat	,	0),
         prtvtcbe	=	replace_na(	prtvtcbe	,	0),
         prtvtfee	=	replace_na(	prtvtfee	,	0),
         prtvtdfi	=	replace_na(	prtvtdfi	,	0),
         prtvtcfr	=	replace_na(	prtvtcfr	,	0),
         prtvede2	=	replace_na(	prtvede2	,	0),
         prtvtehu	=	replace_na(	prtvtehu	,	0),
         prtvtbis	=	replace_na(	prtvtbis	,	0),
         prtvtbie	=	replace_na(	prtvtbie	,	0),
         prtvblt1	=	replace_na(	prtvblt1	,	0),
         prtvtfnl	=	replace_na(	prtvtfnl	,	0),
         prtvtbno	=	replace_na(	prtvtbno	,	0),
         prtvtcpt	=	replace_na(	prtvtcpt	,	0),
         prtvtbse	=	replace_na(	prtvtbse	,	0),
         prtvtfch	=	replace_na(	prtvtfch	,	0),
         prtvtbgb	=	replace_na(	prtvtbgb	,	0),
         greenvote = prtvtbat	+
           prtvtcbe	+
           prtvtfee	+
           prtvtdfi	+
           prtvtcfr	+
           prtvede2	+
           prtvtehu	+
           prtvtbis	+
           prtvtbie	+
           prtvblt1	+
           prtvtfnl	+
           prtvtbno	+
           prtvtcpt	+
           prtvtbse	+
           prtvtfch	+
           prtvtbgb,
         greenvote = na_if(greenvote, 0),
         greenvote = ifelse(greenvote == 2, 1,
                            ifelse(greenvote == 1, 0, NA)))

df8$eisced  <- ifelse(df8$eisced == 55, NA, df8$eisced)
df8$eisced2 <- ifelse(df8$eisced >= 1 & df8$eisced <= 4, 1,
                      ifelse(df8$eisced >= 5, 2, 0))
df8$eisced2 <- ifelse(df8$eisced2 == 0, NA, df8$eisced2)

# Reverse-code three climate policy support items (inctxff, sbsrnen,
# banhhap, only available in round 8) and average them into polindex,
# a climate policy support index.
df8 <- df8 %>%
  mutate(
    inctxff = inctxff * (-1) + 6,
    sbsrnen = sbsrnen * (-1) + 6,
    banhhap = banhhap * (-1) + 6,
    valid_count = rowSums(!is.na(cbind(inctxff, sbsrnen, banhhap))),  # Count number of non-NA values
    polindex = (inctxff + sbsrnen + banhhap) / 3)


##### Round 9  ####
df9 <- read_sav("01_Data/ESS/ESS round 9/ESS9e03_2.sav")
df_ref <- read_excel("01_Data/MPD/MPD_ESS_data_round_9.xlsx")
prtv_columns <- grep("^prtv", names(df9), value = TRUE)

df <- df9 %>%
  select(cntry, idno, all_of(prtv_columns)) 

df_prtv <- df %>%
  pivot_longer(cols = all_of(prtv_columns), 
               names_to = "variable", 
               values_to = "value") %>%
  drop_na(value)  

df_prtv <- df_prtv %>%
  left_join(df_ref %>% select(variable, value, pervote, per416, per501, per503, per504, per603, per604, per706, rile, welfare),
            by = c("variable", "value")) 

df9 <- df9 %>%
  left_join(df_prtv, by = c("cntry", "idno"))

df9 <- remove_var_label(df9)
df9 <- remove_val_labels(df9)
df9 <- df9 %>% 
  mutate(anweight = pspwght*pweight,
         prtvtcat	=	ifelse(	prtvtcat	==	5	,	2, 1),
         prtvtdbe	=	ifelse(	prtvtdbe	==	1	,	2,1),
         ifelse(	prtvtdbe	==	10	,	2,1),
         prtvtbcy	=	ifelse(	prtvtbcy	==	6	,	2,1),
         prtvtddk	=	ifelse(	prtvtddk	==	4	,	2, 1),
         prtvtgee	=	ifelse(	prtvtgee	==	5	,	2,1),
         prtvtdfi	=	ifelse(	prtvtdfi	==	10	,	2,1),
         prtvtdfr	=	ifelse(	prtvtdfr	==	6	,	2,1),
         prtvede2	=	ifelse(	prtvede2	==	4	,	2,1),
         prtvtfhu	=	ifelse(	prtvtfhu	==	6	,	2, 1),
         prtvtcis	=	ifelse(	prtvtcis	==	3	,	2,1),
         prtvtcie	=	ifelse(	prtvtcie	==	4	,	2,1),
         prtvblt1	=	ifelse(	prtvblt1	==	12	,	2,1),
         prtvtgnl	=	ifelse(	prtvtgnl	==	8	,	2,
                            ifelse(	prtvtgnl	==	10	,	2,1)),
         prtvtbno	=	ifelse(	prtvtbno	==	10	,	2,1),
         prtvtcpt	=	ifelse(	prtvtcpt	==	15	,	2,1),
         prtvtcse	=	ifelse(	prtvtcse	==	4	,	2,1),
         prtvtgch	=	ifelse(	prtvtgch	==	5	,	2,
                            ifelse(	prtvtgch	==	6	,	2, 1)),
         prtvtcgb	=	ifelse(	prtvtcgb	==	6	,	2,
                            ifelse(	prtvtcgb	==	15	,	2,1)),
         prtvtcat	=	replace_na(	prtvtcat	,	0),
         prtvtdbe	=	replace_na(	prtvtdbe	,	0),
         prtvtbcy	=	replace_na(	prtvtbcy	,	0),
         prtvtddk	=	replace_na(	prtvtddk	,	0),
         prtvtgee	=	replace_na(	prtvtgee	,	0),
         prtvtdfi	=	replace_na(	prtvtdfi	,	0),
         prtvtdfr	=	replace_na(	prtvtdfr	,	0),
         prtvede2	=	replace_na(	prtvede2	,	0),
         prtvtfhu	=	replace_na(	prtvtfhu	,	0),
         prtvtcis	=	replace_na(	prtvtcis	,	0),
         prtvtcie	=	replace_na(	prtvtcie	,	0),
         prtvblt1	=	replace_na(	prtvblt1	,	0),
         prtvtgnl	=	replace_na(	prtvtgnl	,	0),
         prtvtbno	=	replace_na(	prtvtbno	,	0),
         prtvtcpt	=	replace_na(	prtvtcpt	,	0),
         prtvtcse	=	replace_na(	prtvtcse	,	0),
         prtvtgch	=	replace_na(	prtvtgch	,	0),
         prtvtcgb	=	replace_na(	prtvtcgb	,	0),
         greenvote = prtvtcat	+
           prtvtdbe	+
           prtvtbcy	+
           prtvtddk	+
           prtvtgee	+
           prtvtdfi	+
           prtvtdfr	+
           prtvede2	+
           prtvtfhu	+
           prtvtcis	+
           prtvtcie	+
           prtvblt1	+
           prtvtgnl	+
           prtvtbno	+
           prtvtcpt	+
           prtvtcse	+
           prtvtgch	+
           prtvtcgb,
         greenvote = na_if(greenvote, 0),
         greenvote = ifelse(greenvote == 2, 1,
                            ifelse(greenvote == 1, 0, NA)))

df9$eisced  <- ifelse(df9$eisced == 55, NA, df9$eisced)
df9$eisced2 <- ifelse(df9$eisced >= 1 & df9$eisced <= 4, 1,
                      ifelse(df9$eisced >= 5, 2, 0))
df9$eisced2 <- ifelse(df9$eisced2 == 0, NA, df9$eisced2)

##### Round 10 ####
df10 <- read_sav("01_Data/ESS/ESS round 10/ESS10.sav")
df10_sc <- read_sav("01_Data/ESS/ESS round 10/ESS10SC.sav")
df10 <- bind_rows(df10, df10_sc)
df10 <- remove_var_label(df10)
df10 <- remove_val_labels(df10)
df_ref <- read_excel("01_Data/MPD/MPD_ESS_data_round_10.xlsx")
prtv_columns <- grep("^prtv", names(df10), value = TRUE)

df <- df10 %>%
  select(cntry, idno, all_of(prtv_columns)) 

df_prtv <- df %>%
  pivot_longer(cols = all_of(prtv_columns), 
               names_to = "variable", 
               values_to = "value") %>%
  drop_na(value)  

df_prtv <- df_prtv %>%
  left_join(df_ref %>% select(variable, value, pervote, per416, per501, per503, per504, per603, per604, per706, rile, welfare),
            by = c("variable", "value")) 

df10 <- df10 %>%
  left_join(df_prtv, by = c("cntry", "idno"))

df10 <- df10 %>% 
  mutate(anweight = pweight*dweight,
         prtvtcat	=	ifelse(	prtvtcat	==	5	,	2,1),
         prtvtebe = ifelse( prtvtebe == 1, 2,
                            ifelse( prtvtebe == 9, 2, 1)),
         prtvtccy = ifelse( prtvtccy == 11, 2, 1),
         prtvthee	=	ifelse(	prtvthee	==	5	,	2,1),
         prtvtefi	=	ifelse(	prtvtefi	==	16	,	2,1),
         prtvtefr	=	ifelse( prtvtefr	==	6	,	2,1),
         prtvfde2 = ifelse(prtvfde2 == 4, 2, 1),
         prtvtghu	=	ifelse( prtvtghu	==	6	,	2,1),
         prtvtdie = ifelse( prtvtdie == 6, 2, 1),
         prtvtdis = ifelse( prtvtdis == 11, 2, 1),
         prtvclt1	=	ifelse(	prtvclt1	==	14	,	2,1),
         prtvthnl = ifelse( prtvthnl == 8, 2,
                            ifelse( prtvthnl == 10, 2, 1)),
         prtvtbno = ifelse( prtvtbno == 10, 2, 1),
         prtvtdpt =  ifelse(prtvtdpt == 11, 2, 1),
         prtvtdse = ifelse(prtvtdse == 4, 2, 1),
         prtvthch = ifelse(prtvthch == 4, 2,
                           ifelse(prtvthch == 6, 2, 1)),
         prtvtdgb = ifelse( prtvtdgb == 6, 2,
                            ifelse( prtvtdgb == 16, 2, 1)),
         prtvtcat =	replace_na(	prtvtcat,	0),
         prtvtebe = replace_na( prtvtebe,	0),
         prtvthch = replace_na( prtvthch,	0),
         prtvtccy = replace_na( prtvtccy,	0),
         prtvthee	=	replace_na(	prtvthee,	0),
         prtvtefi	=	replace_na(	prtvtefi,	0),
         prtvtefr	=	replace_na( prtvtefr,	0),
         prtvtdgb = replace_na( prtvtdgb,	0),
         prtvtghu	=	replace_na( prtvtghu,	0),
         prtvtdie = replace_na( prtvtdie,	0),
         prtvtdis = replace_na( prtvtdis,	0),
         prtvclt1	=	replace_na(	prtvclt1,	0),
         prtvthnl = replace_na( prtvthnl,	0),
         prtvtbno = replace_na( prtvtbno,	0),
         prtvtdpt = replace_na( prtvtdpt,	0),
         prtvfde2 = replace_na(	prtvfde2,	0),
         prtvtdse = replace_na(	prtvtdse,	0),
         greenvote = prtvtcat + 
           prtvtebe +
           prtvthch + 
           prtvtccy +
           prtvthee	+
           prtvtefi	+
           prtvtefr	+
           prtvtdgb +
           prtvtghu	+
           prtvtdie +
           prtvtdis +
           prtvclt1 +
           prtvthnl +
           prtvtbno +
           prtvtdpt +
           prtvfde2 +
           prtvtdse, 
         greenvote = na_if(greenvote, 0),
         greenvote = ifelse(greenvote == 2, 1,
                            ifelse(greenvote == 1, 0, NA))) %>%
  select(-domain) %>%
  drop_na(gndr, agea)

df10$eisced  <- ifelse(df10$eisced == 55, NA, df10$eisced)
df10$eisced2 <- ifelse(df10$eisced >= 1 & df10$eisced <= 4, 1,
                       ifelse(df10$eisced >= 5, 2, 0))
df10$eisced2 <- ifelse(df10$eisced2 == 0, NA, df10$eisced2)

##### Round 11 ####
df11 <- read_sav("01_Data/ESS/ESS round 11/ESS11.sav")
df11 <- remove_var_label(df11)
df11 <- remove_val_labels(df11)
df_ref <- read_excel("01_Data/MPD/MPD_ESS_data_round_11.xlsx")
prtv_columns <- grep("^prtv", names(df11), value = TRUE)

df <- df11 %>%
  select(cntry, idno, all_of(prtv_columns)) 

df_prtv <- df %>%
  pivot_longer(cols = all_of(prtv_columns), 
               names_to = "variable", 
               values_to = "value") %>%
  drop_na(value)  

df_prtv <- df_prtv %>%
  left_join(df_ref %>% select(variable, value, pervote, per416, per501, per503, per504, per603, per604, per706, rile, welfare),
            by = c("variable", "value")) 
df11 <- df11 %>%
  left_join(df_prtv, by = c("cntry", "idno"))

df11 <- df11 %>% 
  mutate(
    anweight = pweight * dweight,
    prtvtdat = ifelse(prtvtdat == 5, 2, 1),
    prtvtebe = ifelse(prtvtebe == 1, 2,
                      ifelse(prtvtebe == 9, 2, 1)),
    prtvtccy = ifelse(prtvtccy == 11, 2, 1),
    prtvtffi = ifelse(prtvtffi == 6, 2, 1),
    prtvgde2 = ifelse(prtvgde2 == 4, 2, 1),
    prtvteie = ifelse(prtvteie == 6, 2, 1),
    prtvteis = ifelse(prtvteis == 11, 2, 1),
    prtvclt1 = ifelse(prtvclt1 == 14, 2, 1),
    prtvtinl = ifelse(prtvtinl == 8, 2,
                      ifelse(prtvtinl == 10, 2, 1)),
    prtvtcno = ifelse(prtvtcno == 9, 2, 1),
    prtvtept = ifelse(prtvtept == 13, 2, 1),
    prtvtdse = ifelse(prtvtdse == 4, 2, 1),
    prtvthch = ifelse(prtvthch == 4, 2,
                      ifelse(prtvthch == 6, 2, 1)),
    prtvtdgb = ifelse(prtvtdgb == 6, 2, 
                      ifelse(prtvtdgb == 16, 2, 1)),
    prtvtdat = replace_na(prtvtdat, 0),
    prtvtebe = replace_na(prtvtebe, 0),
    prtvthch = replace_na(prtvthch, 0),
    prtvtccy = replace_na(prtvtccy, 0),
    prtvgde2 = replace_na(prtvgde2, 0),
    prtvtffi = replace_na(prtvtffi, 0),
    prtvtdgb = replace_na(prtvtdgb, 0),
    prtvteie = replace_na(prtvteie, 0),
    prtvteis = replace_na(prtvteis, 0),
    prtvclt1 = replace_na(prtvclt1, 0),
    prtvtinl = replace_na(prtvtinl, 0),
    prtvtcno = replace_na(prtvtcno, 0),
    prtvtept = replace_na(prtvtept, 0),
    prtvtdse = replace_na(prtvtdse, 0),
    greenvote = prtvtdat + 
      prtvtebe +
      prtvthch + 
      prtvtccy +
      prtvgde2 +
      prtvtffi +
      prtvtdgb +
      prtvteie +
      prtvteis +
      prtvclt1 +
      prtvtinl +
      prtvtcno +
      prtvtept +
      prtvtdse, 
    greenvote = na_if(greenvote, 0),
    greenvote = ifelse(greenvote == 2, 1,
                       ifelse(greenvote == 1, 0, NA)),
    impenv = impenva) %>%
  select(-domain) %>%
  drop_na(gndr, agea)

df11$eisced  <- ifelse(df11$eisced == 55, NA, df11$eisced)
df11$eisced2 <- ifelse(df11$eisced >= 1 & df11$eisced <= 4, 1,
                       ifelse(df11$eisced >= 5, 2, 0))
df11$eisced2 <- ifelse(df11$eisced2 == 0, NA, df11$eisced2)

##### Combined ####
df <- bind_rows(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11) 

df <- df %>%
  filter(cntry != "IL" & cntry != "RU" & cntry != "TR") %>%
  select(cntry, essround, anweight, pspwght, yrbrn,
         impenv, wrclmch, ccrdprs, greenvote, 
         gndr, agea, eisced2, eisced, domicil, 
         iscoco, isco08, emplno, emplrel, 
         ipadvnt,
         lrscale, polintr, 
         eneffap, rdcenr, 
         clmchng, inctxff, sbsrnen, banhhap,
         ecohenv, pervote, per416, per501, per503, per504, per603, per604, per706, rile, welfare, polindex) %>% 
  mutate(gndr = gndr - 1,
         eisced2 = eisced2 - 1,
         impenv = impenv * (-1) + 7,
         polintr = polintr * (-1) + 5,
         # Group countries into broad European regions for regional analysis
         region = case_when(
           cntry %in% c("AT", "BE", "DE", "CH", "LU", "FR", "GB", "NL", "IE") ~ "West",
           cntry %in% c("CZ", "EE", "HU", "AL", "BG", "HR", "XK", "LV", "LT", "PL", "SI", "ME", "MK", "RO", "RS", "SK", "UA") ~ "East",
           cntry %in% c("ES", "IT", "PT", "CY", "GR") ~ "South",
           cntry %in% c("FI", "IS", "DK", "NO", "SE") ~ "North",
           TRUE ~ NA_character_)) %>%
  drop_na(gndr, agea) 

df <- remove_var_label(df)
df <- remove_val_labels(df)



#### Re-code ####
## Oesch class scheme: classifies respondents into 16 (class16) and
## 8 (class8) occupational classes based on ISCO occupation codes
## (iscoco for rounds 1-5, isco08 for rounds 6-11) and self-employment
## status (selfem_mainjob).
df <- df %>%
  mutate(emplrel = replace_na(emplrel, 9),
         emplrel = ifelse(cntry == "FR" & essround == 1, NA, 
                          ifelse(cntry == "FR" & essround == 2, NA, emplrel)),
         emplno = replace_na(emplno, 0),
         emplno = ifelse(emplno == 0, 0,
                         ifelse(emplno >= 1 & emplno <= 9, 1,
                                ifelse(emplno >=  10, 2, NA))),
         selfem_mainjob = ifelse(emplrel == 1, 1, 
                                 ifelse(emplrel == 9, 1, 
                                        ifelse(emplrel == 2 & emplno == 0, 2,
                                               ifelse(emplrel == 3, 2,
                                                      ifelse(emplrel == 2 & emplno == 1, 3,
                                                             ifelse(emplrel == 2 & emplno == 2, 4, NA)))))))
# Oesch class 1 to 5
df1 <- df %>% 
  filter(essround <= 5) %>% 
  mutate(isco_mainjob = iscoco,
         class16 = ifelse(selfem_mainjob==4, 1, 0),
         class16 = ifelse(selfem_mainjob==2 | selfem_mainjob==3 & isco_mainjob >= 2000 & isco_mainjob <= 2229, 2,
                          ifelse(selfem_mainjob==2 | selfem_mainjob==3 & isco_mainjob >= 2300 & isco_mainjob <= 2470, 2, class16)),
         class16 = ifelse(selfem_mainjob==3 & isco_mainjob >= 1000 & isco_mainjob <= 1999, 3,
                          ifelse(selfem_mainjob==3 & isco_mainjob >= 3000 & isco_mainjob <= 9333, 3, 
                                 ifelse(selfem_mainjob==3 & isco_mainjob == 2230, 3, class16))),
         class16 = ifelse(selfem_mainjob==2 & isco_mainjob >= 1000 & isco_mainjob <= 1999, 4,
                          ifelse(selfem_mainjob==2 & isco_mainjob >= 3000 & isco_mainjob <= 9333, 4,
                                 ifelse(selfem_mainjob==2 & isco_mainjob == 2230, 4, class16))), 
         class16 = ifelse(selfem_mainjob==1 & isco_mainjob >= 2100 & isco_mainjob <= 2213, 5, class16),
         class16 = ifelse(selfem_mainjob==1 & isco_mainjob >= 3100 & isco_mainjob <= 3152, 6,
                          ifelse(selfem_mainjob==1 & isco_mainjob >= 3210 & isco_mainjob <= 3213, 6,
                                 ifelse(selfem_mainjob==1 & isco_mainjob == 3434, 6, class16))),
         class16 = ifelse(selfem_mainjob==1 & isco_mainjob >= 6000 & isco_mainjob <= 7442, 7,
                          ifelse(selfem_mainjob==1 & isco_mainjob >= 8310 & isco_mainjob <= 8312, 7,
                                 ifelse(selfem_mainjob==1 & isco_mainjob >= 8324 & isco_mainjob <= 8330, 7,
                                        ifelse(selfem_mainjob==1 & isco_mainjob >= 8332 & isco_mainjob <= 8340, 7, class16)))), 
         class16 = ifelse(selfem_mainjob==1 & isco_mainjob >= 8000 & isco_mainjob <= 8300, 8,
                          ifelse(selfem_mainjob==1 & isco_mainjob >= 8320 & isco_mainjob <= 8321, 8,
                                 ifelse(selfem_mainjob==1 & isco_mainjob == 8331, 8,
                                        ifelse(selfem_mainjob==1 & isco_mainjob >= 9153 & isco_mainjob <= 9333, 8, class16)))),
         class16 = ifelse(selfem_mainjob==1 & isco_mainjob >= 1000 & isco_mainjob <= 1239, 9,
                          ifelse(selfem_mainjob==1 & isco_mainjob >= 2400 & isco_mainjob <= 2429, 9, 
                                 ifelse(selfem_mainjob==1 & isco_mainjob == 2441, 9,
                                        ifelse(selfem_mainjob==1 & isco_mainjob == 2470, 9, class16)))),
         class16 = ifelse(selfem_mainjob==1 & isco_mainjob >= 1300 & isco_mainjob <= 1319, 10,
                          ifelse(selfem_mainjob==1 & isco_mainjob >= 3400 & isco_mainjob <= 3433, 10,
                                 ifelse(selfem_mainjob==1 & isco_mainjob >= 3440 & isco_mainjob <= 3450, 10, class16))),
         class16 = ifelse(selfem_mainjob==1 & isco_mainjob >= 4000 & isco_mainjob <= 4112, 11,
                          ifelse(selfem_mainjob==1 & isco_mainjob >= 4114 & isco_mainjob <= 4210, 11,
                                 ifelse(selfem_mainjob==1 & isco_mainjob >= 4212 & isco_mainjob <= 4222,11, class16))),
         class16 = ifelse(selfem_mainjob==1 & isco_mainjob == 4113, 12,
                          ifelse(selfem_mainjob==1 & isco_mainjob == 4211, 12,
                                 ifelse(selfem_mainjob==1 & isco_mainjob == 4223, 12, class16))),
         class16 = ifelse(selfem_mainjob==1 & isco_mainjob >= 2220 &  isco_mainjob <= 2229, 13,
                          ifelse(selfem_mainjob==1 & isco_mainjob >= 2300 &  isco_mainjob <= 2320, 13,
                                 ifelse(selfem_mainjob==1 & isco_mainjob >= 2340 &  isco_mainjob <= 2359, 13,
                                        ifelse(selfem_mainjob==1 & isco_mainjob >= 2430 &  isco_mainjob <= 2440, 13,
                                               ifelse(selfem_mainjob==1 & isco_mainjob >= 2442 &  isco_mainjob <= 2443, 13,
                                                      ifelse(selfem_mainjob==1 & isco_mainjob == 2445, 13,
                                                             ifelse(selfem_mainjob==1 & isco_mainjob == 2451, 13,
                                                                    ifelse(selfem_mainjob==1 & isco_mainjob == 2460, 13, class16)))))))),
         class16 = ifelse(selfem_mainjob==1 & isco_mainjob == 2230, 14,
                          ifelse(selfem_mainjob==1 & isco_mainjob >= 2330 & isco_mainjob <= 2332, 14,
                                 ifelse(selfem_mainjob==1 & isco_mainjob == 2444, 14,
                                        ifelse(selfem_mainjob==1 & isco_mainjob >= 2446 & isco_mainjob <= 2450, 14,
                                               ifelse(selfem_mainjob==1 & isco_mainjob >= 2452 & isco_mainjob <= 2455, 14,
                                                      ifelse(selfem_mainjob==1 & isco_mainjob == 3200, 14,
                                                             ifelse(selfem_mainjob==1 & isco_mainjob >= 3220 & isco_mainjob <= 3224, 14,
                                                                    ifelse(selfem_mainjob==1 & isco_mainjob == 3226, 14,
                                                                           ifelse(selfem_mainjob==1 & isco_mainjob >= 3229 & isco_mainjob <= 3340, 14,
                                                                                  ifelse(selfem_mainjob==1 & isco_mainjob >= 3460 & isco_mainjob <= 3472, 14,
                                                                                         ifelse(selfem_mainjob==1 & isco_mainjob == 3480, 14, class16))))))))))),
         class16 = ifelse(selfem_mainjob==1 & isco_mainjob == 3225, 15,
                          ifelse(selfem_mainjob==1 & isco_mainjob >= 3227 & isco_mainjob <= 3228, 15,
                                 ifelse(selfem_mainjob==1 & isco_mainjob >= 3473 & isco_mainjob <= 3475, 15,
                                        ifelse(selfem_mainjob==1 & isco_mainjob >= 5000 & isco_mainjob <= 5113, 15,
                                               ifelse(selfem_mainjob==1 & isco_mainjob == 5122, 15,
                                                      ifelse(selfem_mainjob==1 & isco_mainjob >= 5131 & isco_mainjob <= 5132, 15,
                                                             ifelse(selfem_mainjob==1 & isco_mainjob >= 5140 & isco_mainjob <= 5141, 15,
                                                                    ifelse(selfem_mainjob==1 & isco_mainjob == 5143, 15,
                                                                           ifelse(selfem_mainjob==1 & isco_mainjob >= 5160 & isco_mainjob <= 5220, 15,
                                                                                  ifelse(selfem_mainjob==1 & isco_mainjob == 8323, 15, class16)))))))))),
         class16 = ifelse(selfem_mainjob==1 & isco_mainjob >= 5120 & isco_mainjob <= 5121, 16,
                          ifelse(selfem_mainjob==1 & isco_mainjob >= 5123 & isco_mainjob <= 5130, 16,
                                 ifelse(selfem_mainjob==1 & isco_mainjob >= 5133 & isco_mainjob <= 5139, 16,
                                        ifelse(selfem_mainjob==1 & isco_mainjob == 5142, 16,
                                               ifelse(selfem_mainjob==1 & isco_mainjob == 5149, 16,
                                                      ifelse(selfem_mainjob==1 & isco_mainjob == 5230, 16,
                                                             ifelse(selfem_mainjob==1 & isco_mainjob == 8322, 16,
                                                                    ifelse(selfem_mainjob==1 & isco_mainjob >= 9100 &  isco_mainjob <= 9152, 16, class16)))))))),
         class16 = ifelse(class16 == 0, NA, class16),
         class8  = ifelse(class16 <= 2, 1,
                          ifelse(class16 == 3 | class16 == 4, 2,
                                 ifelse(class16 == 5 | class16 == 6, 3,
                                        ifelse(class16 == 7 | class16 == 8, 4,
                                               ifelse(class16 == 9 | class16 == 10, 5,
                                                      ifelse(class16 == 11 | class16 == 12, 6,
                                                             ifelse(class16 == 13 | class16 == 14, 7,
                                                                    ifelse(class16 >= 15, 8, 0)))))))),
         class5  = ifelse(class16 == 1 | class16 == 2 | class16 == 5 | class16 == 9 | class16 == 13, 1,
                          ifelse(class16 == 6 | class16 == 10 | class16 == 14, 2,
                                 ifelse(class16 == 3 | class16 == 4, 3,
                                        ifelse(class16 == 7 | class16 == 11 | class16 == 15, 4,
                                               ifelse(class16 == 8 | class16 == 12 | class16 == 16, 5, 0))))),
         class16 = ifelse(class16 == 1, "1 Large employers",
                          ifelse(class16 == 2, "2 Self-employed professionals",
                                 ifelse(class16 == 3, "3 Small business owners with employees",
                                        ifelse(class16 == 4, "4 Small business owners without employees",
                                               ifelse(class16 == 5, "5 Technical experts",
                                                      ifelse(class16 == 6, "6 Technicians",
                                                             ifelse(class16 == 7, "7 Skilled manual" ,
                                                                    ifelse(class16 == 8, "0 Low-skilled manual",
                                                                           ifelse(class16 == 9, "9 Higher-grade managers and administrators",
                                                                                  ifelse(class16 == 10, "10 Lower-grade managers and administrators",
                                                                                         ifelse(class16 == 11, "11 Skilled clerks",
                                                                                                ifelse(class16 == 12, "12 Unskilled clerks",
                                                                                                       ifelse(class16 == 13, "13 Socio-cultural professionals",
                                                                                                              ifelse(class16 == 14, "14 Socio-cultural semi-professionals",
                                                                                                                     ifelse(class16 == 15, "15 Skilled service",
                                                                                                                            ifelse(class16 == 16, "16 Low-skilled service", NA)))))))))))))))),
         class5  = ifelse(class5 == 1, "1 Higher-grade service class",
                          ifelse(class5 == 2, "2 Lower-grade service class",
                                 ifelse(class5 == 3, "3 Small business owners",
                                        ifelse(class5 == 4, "4 Skilled workers",
                                               ifelse(class5 == 5, "5 Unskilled workers", NA))))),
         self_w =      ifelse(gndr == 2 & class8 == 1, 1, 0),
         small_w =     ifelse(gndr == 2 & class8 == 2, 1, 0),
         tech_w =      ifelse(gndr == 2 & class8 == 3, 1, 0),
         prod_w =      ifelse(gndr == 2 & class8 == 4, 1, 0),
         manager_w =   ifelse(gndr == 2 & class8 == 5, 1, 0),
         clerks_w =    ifelse(gndr == 2 & class8 == 6, 1, 0),
         sociocult_w = ifelse(gndr == 2 & class8 == 7, 1, 0),
         service_w =   ifelse(gndr == 2 & class8 == 8, 1, 0),
         self_m =      ifelse(gndr == 1 & class8 == 1, 1, 0),
         small_m =     ifelse(gndr == 1 & class8 == 2, 1, 0),
         tech_m =      ifelse(gndr == 1 & class8 == 3, 1, 0),
         prod_m =      ifelse(gndr == 1 & class8 == 4, 1, 0),
         manager_m =   ifelse(gndr == 1 & class8 == 5, 1, 0),
         clerks_m =    ifelse(gndr == 1 & class8 == 6, 1, 0),
         sociocult_m = ifelse(gndr == 1 & class8 == 7, 1, 0),
         service_m =   ifelse(gndr == 1 & class8 == 8, 1, 0),
         self =      ifelse(class8 == 1, 1, 0),
         small =     ifelse(class8 == 2, 1, 0),
         tech =      ifelse(class8 == 3, 1, 0),
         prod =      ifelse(class8 == 4, 1, 0),
         manager =   ifelse(class8 == 5, 1, 0),
         clerks =    ifelse(class8 == 6, 1, 0),
         sociocult = ifelse(class8 == 7, 1, 0),
         service =   ifelse(class8 == 8, 1, 0))

# Oesch class 6 to 11 
df2 <- df %>% 
  filter(essround >= 6) %>%
  mutate(isco_mainjob = isco08,
         class16 = ifelse(selfem_mainjob==4, 1, 0),
         class16 = ifelse(selfem_mainjob==2 |selfem_mainjob==3 & isco_mainjob >= 2000 & isco_mainjob <= 2162, 2, 
                          ifelse(selfem_mainjob==2 |selfem_mainjob==3 & isco_mainjob >= 2164 & isco_mainjob <= 2165, 2, 
                                 ifelse(selfem_mainjob==2 |selfem_mainjob==3 & isco_mainjob >= 2200 & isco_mainjob <= 2212, 2, 
                                        ifelse(selfem_mainjob==2 | selfem_mainjob==3 & isco_mainjob == 2250, 2,
                                               ifelse(selfem_mainjob==2 | selfem_mainjob==3 & isco_mainjob >= 2261 & isco_mainjob <= 2262, 2,
                                                      ifelse(selfem_mainjob==2 | selfem_mainjob==3 & isco_mainjob >= 2300 & isco_mainjob <= 2330, 2,
                                                             ifelse(selfem_mainjob==2 | selfem_mainjob==3 & isco_mainjob >= 2350 & isco_mainjob <= 2352, 2,
                                                                    ifelse(selfem_mainjob==2 | selfem_mainjob==3 & isco_mainjob >= 2359 & isco_mainjob <= 2432, 2,
                                                                           ifelse(selfem_mainjob==2 | selfem_mainjob==3 & isco_mainjob >= 2500 & isco_mainjob <= 2619, 2,
                                                                                  ifelse(selfem_mainjob==2 | selfem_mainjob==3 & isco_mainjob == 2621, 2,
                                                                                         ifelse(selfem_mainjob==2 | selfem_mainjob==3 & isco_mainjob >= 2630 & isco_mainjob <= 2634, 2,
                                                                                                ifelse(selfem_mainjob==2 | selfem_mainjob==3 & isco_mainjob >= 2636 & isco_mainjob <= 2640, 2,
                                                                                                       ifelse(selfem_mainjob==2 | selfem_mainjob==3 & isco_mainjob >= 2642 & isco_mainjob <= 2643, 2, class16))))))))))))),
         class16 = ifelse(selfem_mainjob==3 & isco_mainjob >= 1000 & isco_mainjob <= 1439, 3,
                          ifelse(selfem_mainjob==3 & isco_mainjob == 2163, 3,
                                 ifelse(selfem_mainjob==3 & isco_mainjob == 2166, 3,
                                        ifelse(selfem_mainjob==3 & isco_mainjob >= 2220 & isco_mainjob <= 2240, 3,
                                               ifelse(selfem_mainjob==3 & isco_mainjob == 2260, 3,
                                                      ifelse(selfem_mainjob==3 & isco_mainjob >= 2263 & isco_mainjob <= 2269, 3,
                                                             ifelse(selfem_mainjob==3 & isco_mainjob >= 2340 & isco_mainjob <= 2342, 3,
                                                                    ifelse(selfem_mainjob==3 & isco_mainjob >= 2353 & isco_mainjob <= 2356, 3,
                                                                           ifelse(selfem_mainjob==3 & isco_mainjob >= 2433 & isco_mainjob <= 243, 3,
                                                                                  ifelse(selfem_mainjob==3 & isco_mainjob == 2620, 3,
                                                                                         ifelse(selfem_mainjob==3 & isco_mainjob == 2622, 3,
                                                                                                ifelse(selfem_mainjob==3 & isco_mainjob == 2635, 3,
                                                                                                       ifelse(selfem_mainjob==3 & isco_mainjob == 2641, 3,
                                                                                                              ifelse(selfem_mainjob==3 & isco_mainjob >= 2650 & isco_mainjob <= 2659, 3,
                                                                                                                     ifelse(selfem_mainjob==3 & isco_mainjob >= 3000 & isco_mainjob <= 9629, 3, class16))))))))))))))),
         class16 = ifelse(selfem_mainjob==2 & isco_mainjob >= 1000 & isco_mainjob <= 1439, 4,
                          ifelse(selfem_mainjob==2 & isco_mainjob == 2163, 4,
                                 ifelse(selfem_mainjob==2 & isco_mainjob == 2166, 4,
                                        ifelse(selfem_mainjob==2 & isco_mainjob >= 2220 & isco_mainjob <= 2240, 4,
                                               ifelse(selfem_mainjob==2 & isco_mainjob == 2260, 4,
                                                      ifelse(selfem_mainjob==2 & isco_mainjob >= 2263 & isco_mainjob <= 2269, 4,
                                                             ifelse(selfem_mainjob==2 & isco_mainjob >= 2340 & isco_mainjob <= 2342, 4,
                                                                    ifelse(selfem_mainjob==2 & isco_mainjob >= 2353 & isco_mainjob <= 2356, 4,
                                                                           ifelse(selfem_mainjob==2 & isco_mainjob >= 2433 & isco_mainjob <= 2434, 4,
                                                                                  ifelse(selfem_mainjob==2 & isco_mainjob == 2620, 4,
                                                                                         ifelse(selfem_mainjob==2 & isco_mainjob == 2622, 4,
                                                                                                ifelse(selfem_mainjob==2 & isco_mainjob == 2635, 4,
                                                                                                       ifelse(selfem_mainjob==2 & isco_mainjob == 2641, 4,
                                                                                                              ifelse(selfem_mainjob==2 & isco_mainjob >= 2650 & isco_mainjob <= 2659, 4,
                                                                                                                     ifelse(selfem_mainjob==2 & isco_mainjob >= 3000 & isco_mainjob <= 9629, 4, class16))))))))))))))),
         class16 = ifelse(selfem_mainjob==1 & isco_mainjob >= 2100 & isco_mainjob <= 2162, 5,
                          ifelse(selfem_mainjob==1 & isco_mainjob >= 2164 & isco_mainjob <= 2165, 5,
                                 ifelse(selfem_mainjob==1 & isco_mainjob >= 2500 & isco_mainjob <= 2529, 5, class16))),
         class16 = ifelse(selfem_mainjob==1 & isco_mainjob >= 3100 & isco_mainjob <= 3155, 6,
                          ifelse(selfem_mainjob==1 & isco_mainjob >= 3210 & isco_mainjob <= 3214, 6,
                                 ifelse(selfem_mainjob==1 & isco_mainjob == 3252, 6,
                                        ifelse(selfem_mainjob==1 & isco_mainjob >= 3500 & isco_mainjob <= 3522, 6, class16)))),
         class16 = ifelse(selfem_mainjob==1 & isco_mainjob >= 6000 & isco_mainjob <= 7549, 7,
                          ifelse(selfem_mainjob==1 & isco_mainjob >= 8310 & isco_mainjob <= 8312, 7,
                                 ifelse(selfem_mainjob==1 & isco_mainjob == 8330, 7,
                                        ifelse(selfem_mainjob==1 & isco_mainjob >= 8332 & isco_mainjob <= 8340, 7,
                                               ifelse(selfem_mainjob==1 & isco_mainjob >= 8342 & isco_mainjob <= 8344, 7, class16))))),
         class16 = ifelse(selfem_mainjob==1 & isco_mainjob >= 8000 & isco_mainjob <= 8300, 8,
                          ifelse(selfem_mainjob==1 & isco_mainjob >= 8320 & isco_mainjob <= 8321, 8,
                                 ifelse(selfem_mainjob==1 & isco_mainjob == 8341, 8,
                                        ifelse(selfem_mainjob==1 & isco_mainjob == 8350, 8,
                                               ifelse(selfem_mainjob==1 & isco_mainjob >= 9200 & isco_mainjob <= 9334, 8,
                                                      ifelse(selfem_mainjob==1 & isco_mainjob >= 9600 & isco_mainjob <= 9620, 8,
                                                             ifelse(selfem_mainjob==1 & isco_mainjob >= 9622 & isco_mainjob <= 9629, 8, class16))))))),
         class16 = ifelse(selfem_mainjob==1 & isco_mainjob >= 1000 & isco_mainjob <= 1300, 9,
                          ifelse(selfem_mainjob==1 & isco_mainjob >= 1320 & isco_mainjob <= 1349, 9,
                                 ifelse(selfem_mainjob==1 & isco_mainjob >= 2400 & isco_mainjob <= 2432, 9,
                                        ifelse(selfem_mainjob==1 & isco_mainjob >= 2610 & isco_mainjob <= 2619, 9,
                                               ifelse(selfem_mainjob==1 & isco_mainjob == 2631, 9,
                                                      ifelse(selfem_mainjob==1 & isco_mainjob >= 100 & isco_mainjob <= 110, 9, class16)))))),
         class16 = ifelse(selfem_mainjob==1 & isco_mainjob >= 1310 & isco_mainjob <= 1312, 10,
                          ifelse(selfem_mainjob==1 & isco_mainjob >= 1400 & isco_mainjob <= 1439, 10,
                                 ifelse(selfem_mainjob==1 & isco_mainjob >= 2433 & isco_mainjob <= 2434, 10,
                                        ifelse(selfem_mainjob==1 & isco_mainjob >= 3300 & isco_mainjob <= 3339, 10,
                                               ifelse(selfem_mainjob==1 & isco_mainjob == 3343, 10,
                                                      ifelse(selfem_mainjob==1 & isco_mainjob >= 3350 & isco_mainjob <= 3359, 10,
                                                             ifelse(selfem_mainjob==1 & isco_mainjob == 3411, 10,
                                                                    ifelse(selfem_mainjob==1 & isco_mainjob == 5221, 10,
                                                                           ifelse(selfem_mainjob==1 & isco_mainjob >= 200 & isco_mainjob <= 210, 10, class16))))))))),
         class16 = ifelse(selfem_mainjob==1 & isco_mainjob >= 3340 & isco_mainjob <= 3342, 11,
                          ifelse(selfem_mainjob==1 & isco_mainjob == 3344, 11,
                                 ifelse(selfem_mainjob==1 & isco_mainjob >= 4000 & isco_mainjob <= 4131, 11,
                                        ifelse(selfem_mainjob==1 & isco_mainjob >= 4200 & isco_mainjob <= 4221, 11,
                                               ifelse(selfem_mainjob==1 & isco_mainjob >= 4224 & isco_mainjob <= 4413, 11,
                                                      ifelse(selfem_mainjob==1 & isco_mainjob >= 4415 & isco_mainjob <= 4419, 11, class16)))))),
         class16 = ifelse(selfem_mainjob==1 & isco_mainjob == 4132, 12,
                          ifelse(selfem_mainjob==1 & isco_mainjob == 4222, 12,
                                 ifelse(selfem_mainjob==1 & isco_mainjob == 4223, 12,
                                        ifelse(selfem_mainjob==1 & isco_mainjob == 5230, 12,
                                               ifelse(selfem_mainjob==1 & isco_mainjob == 9621, 12, class16))))),
         class16 = ifelse(selfem_mainjob==1 & isco_mainjob >= 2200 &  isco_mainjob <= 2212, 13,
                          ifelse(selfem_mainjob==1 & isco_mainjob == 2250, 13,
                                 ifelse(selfem_mainjob==1 & isco_mainjob >= 2261 &  isco_mainjob <= 2262, 13,
                                        ifelse(selfem_mainjob==1 & isco_mainjob >= 2300 &  isco_mainjob <= 2330, 13,
                                               ifelse(selfem_mainjob==1 & isco_mainjob >= 2350 &  isco_mainjob <= 2352, 13,
                                                      ifelse(selfem_mainjob==1 & isco_mainjob == 2359, 13,
                                                             ifelse(selfem_mainjob==1 & isco_mainjob == 2600, 13,
                                                                    ifelse(selfem_mainjob==1 & isco_mainjob == 2621, 13,
                                                                           ifelse(selfem_mainjob==1 & isco_mainjob == 2630, 13,
                                                                                  ifelse(selfem_mainjob==1 & isco_mainjob >= 2632 &  isco_mainjob <= 2634, 13,
                                                                                         ifelse(selfem_mainjob==1 & isco_mainjob >= 2636 &  isco_mainjob <= 2640, 13,
                                                                                                ifelse(selfem_mainjob==1 & isco_mainjob >= 2642 &  isco_mainjob <= 2643, 13, class16)))))))))))),
         class16 = ifelse(selfem_mainjob==1 & isco_mainjob == 2163, 14,
                          ifelse(selfem_mainjob==1 & isco_mainjob == 2166, 14,
                                 ifelse(selfem_mainjob==1 & isco_mainjob >= 2220 & isco_mainjob <= 2240, 14,
                                        ifelse(selfem_mainjob==1 & isco_mainjob == 2260, 14,
                                               ifelse(selfem_mainjob==1 & isco_mainjob >= 2263 & isco_mainjob <= 2269, 14,
                                                      ifelse(selfem_mainjob==1 & isco_mainjob >= 2340 & isco_mainjob <= 2342, 14,
                                                             ifelse(selfem_mainjob==1 & isco_mainjob >= 2353 & isco_mainjob <= 2356, 14,
                                                                    ifelse(selfem_mainjob==1 & isco_mainjob == 2620, 14,
                                                                           ifelse(selfem_mainjob==1 & isco_mainjob == 2622, 14,
                                                                                  ifelse(selfem_mainjob==1 & isco_mainjob == 2635, 14,
                                                                                         ifelse(selfem_mainjob==1 & isco_mainjob == 2641, 14,
                                                                                                ifelse(selfem_mainjob==1 & isco_mainjob >= 2650 & isco_mainjob <= 2659, 14,
                                                                                                       ifelse(selfem_mainjob==1 & isco_mainjob == 3200, 14,
                                                                                                              ifelse(selfem_mainjob==1 & isco_mainjob >= 3220 & isco_mainjob <= 3230, 14,
                                                                                                                     ifelse(selfem_mainjob==1 & isco_mainjob == 3250, 14,
                                                                                                                            ifelse(selfem_mainjob==1 & isco_mainjob >= 3253 & isco_mainjob <= 3257, 14,
                                                                                                                                   ifelse(selfem_mainjob==1 & isco_mainjob == 3259, 14,
                                                                                                                                          ifelse(selfem_mainjob==1 & isco_mainjob >= 3400 & isco_mainjob <= 3410, 14,
                                                                                                                                                 ifelse(selfem_mainjob==1 & isco_mainjob >= 3412 & isco_mainjob <= 3413, 14,
                                                                                                                                                        ifelse(selfem_mainjob==1 & isco_mainjob >= 3430 & isco_mainjob <= 3433, 14,
                                                                                                                                                               ifelse(selfem_mainjob==1 & isco_mainjob == 3435, 14,
                                                                                                                                                                      ifelse(selfem_mainjob==1 & isco_mainjob == 4414, 14, class16)))))))))))))))))))))),
         class16 = ifelse(selfem_mainjob==1 & isco_mainjob == 3240, 15,
                          ifelse(selfem_mainjob==1 & isco_mainjob == 3251, 15,
                                 ifelse(selfem_mainjob==1 & isco_mainjob == 3258, 15,
                                        ifelse(selfem_mainjob==1 & isco_mainjob >= 3420 & isco_mainjob <= 3423, 15,
                                               ifelse(selfem_mainjob==1 & isco_mainjob == 3434, 15,
                                                      ifelse(selfem_mainjob==1 & isco_mainjob >= 5000 & isco_mainjob <= 5120, 15,
                                                             ifelse(selfem_mainjob==1 & isco_mainjob >= 5140 & isco_mainjob <= 5142, 15,
                                                                    ifelse(selfem_mainjob==1 & isco_mainjob == 5163, 15,
                                                                           ifelse(selfem_mainjob==1 & isco_mainjob == 5165, 15,
                                                                                  ifelse(selfem_mainjob==1 & isco_mainjob == 5200, 15,
                                                                                         ifelse(selfem_mainjob==1 & isco_mainjob == 5220, 15,
                                                                                                ifelse(selfem_mainjob==1 & isco_mainjob >= 5222 & isco_mainjob <= 5223, 15,
                                                                                                       ifelse(selfem_mainjob==1 & isco_mainjob >= 5241 & isco_mainjob <= 5242, 15,
                                                                                                              ifelse(selfem_mainjob==1 & isco_mainjob >= 5300 & isco_mainjob <= 5321, 15,
                                                                                                                     ifelse(selfem_mainjob==1 & isco_mainjob >= 5400 & isco_mainjob <= 5413, 15,
                                                                                                                            ifelse(selfem_mainjob==1 & isco_mainjob == 5419, 15,
                                                                                                                                   ifelse(selfem_mainjob==1 & isco_mainjob == 8331, 15, class16))))))))))))))))),
         class16 = ifelse(selfem_mainjob==1 & isco_mainjob >= 5130 & isco_mainjob <= 5132, 16,
                          ifelse(selfem_mainjob==1 & isco_mainjob >= 5150 & isco_mainjob <= 5162, 16,
                                 ifelse(selfem_mainjob==1 & isco_mainjob == 5164, 16,
                                        ifelse(selfem_mainjob==1 & isco_mainjob == 5169, 16,
                                               ifelse(selfem_mainjob==1 & isco_mainjob >= 5210 & isco_mainjob <= 5212, 16,
                                                      ifelse(selfem_mainjob==1 & isco_mainjob == 5240, 16,
                                                             ifelse(selfem_mainjob==1 & isco_mainjob >= 5243 & isco_mainjob <= 5249, 16,
                                                                    ifelse(selfem_mainjob==1 & isco_mainjob >= 5322 & isco_mainjob <= 5329, 16,
                                                                           ifelse(selfem_mainjob==1 & isco_mainjob == 5414, 16,
                                                                                  ifelse(selfem_mainjob==1 & isco_mainjob == 8322, 16,
                                                                                         ifelse(selfem_mainjob==1 & isco_mainjob >= 9100 & isco_mainjob <= 9129, 16,
                                                                                                ifelse(selfem_mainjob==1 & isco_mainjob >= 9400 & isco_mainjob <= 9520, 16, class16)))))))))))),
         class16 = ifelse(class16 == 0, NA, class16),
         class8  = ifelse(class16 <= 2, 1,
                          ifelse(class16 == 3 | class16 == 4, 2,
                                 ifelse(class16 == 5 | class16 == 6, 3,
                                        ifelse(class16 == 7 | class16 == 8, 4,
                                               ifelse(class16 == 9 | class16 == 10, 5,
                                                      ifelse(class16 == 11 | class16 == 12, 6,
                                                             ifelse(class16 == 13 | class16 == 14, 7,
                                                                    ifelse(class16 >= 15, 8, 0)))))))),
         class5  = ifelse(class16 == 1 | class16 == 2 | class16 == 5 | class16 == 9 | class16 == 13, 1,
                          ifelse(class16 == 6 | class16 == 10 | class16 == 14, 2,
                                 ifelse(class16 == 3 | class16 == 4, 3,
                                        ifelse(class16 == 7 | class16 == 11 | class16 == 15, 4,
                                               ifelse(class16 == 8 | class16 == 12 | class16 == 16, 5, 0))))),
         class16 = ifelse(class16 == 1, "1 Large employers",
                          ifelse(class16 == 2, "2 Self-employed professionals",
                                 ifelse(class16 == 3, "3 Small business owners with employees",
                                        ifelse(class16 == 4, "4 Small business owners without employees",
                                               ifelse(class16 == 5, "5 Technical experts",
                                                      ifelse(class16 == 6, "6 Technicians",
                                                             ifelse(class16 == 7, "7 Skilled manual" ,
                                                                    ifelse(class16 == 8, "0 Low-skilled manual",
                                                                           ifelse(class16 == 9, "9 Higher-grade managers and administrators",
                                                                                  ifelse(class16 == 10, "10 Lower-grade managers and administrators",
                                                                                         ifelse(class16 == 11, "11 Skilled clerks",
                                                                                                ifelse(class16 == 12, "12 Unskilled clerks",
                                                                                                       ifelse(class16 == 13, "13 Socio-cultural professionals",
                                                                                                              ifelse(class16 == 14, "14 Socio-cultural semi-professionals",
                                                                                                                     ifelse(class16 == 15, "15 Skilled service",
                                                                                                                            ifelse(class16 == 16, "16 Low-skilled service", NA)))))))))))))))),
         class5  = ifelse(class5 == 1, "1 Higher-grade service class",
                          ifelse(class5 == 2, "2 Lower-grade service class",
                                 ifelse(class5 == 3, "3 Small business owners",
                                        ifelse(class5 == 4, "4 Skilled workers",
                                               ifelse(class5 == 5, "5 Unskilled workers", NA))))),
         self_w =      ifelse(gndr == 2 & class8 == 1, 1, 0),
         small_w =     ifelse(gndr == 2 & class8 == 2, 1, 0),
         tech_w =      ifelse(gndr == 2 & class8 == 3, 1, 0),
         prod_w =      ifelse(gndr == 2 & class8 == 4, 1, 0),
         manager_w =   ifelse(gndr == 2 & class8 == 5, 1, 0),
         clerks_w =    ifelse(gndr == 2 & class8 == 6, 1, 0),
         sociocult_w = ifelse(gndr == 2 & class8 == 7, 1, 0),
         service_w =   ifelse(gndr == 2 & class8 == 8, 1, 0),
         self_m =      ifelse(gndr == 1 & class8 == 1, 1, 0),
         small_m =     ifelse(gndr == 1 & class8 == 2, 1, 0),
         tech_m =      ifelse(gndr == 1 & class8 == 3, 1, 0),
         prod_m =      ifelse(gndr == 1 & class8 == 4, 1, 0),
         manager_m =   ifelse(gndr == 1 & class8 == 5, 1, 0),
         clerks_m =    ifelse(gndr == 1 & class8 == 6, 1, 0),
         sociocult_m = ifelse(gndr == 1 & class8 == 7, 1, 0),
         service_m =   ifelse(gndr == 1 & class8 == 8, 1, 0),
         self =      ifelse(class8 == 1, 1, 0),
         small =     ifelse(class8 == 2, 1, 0),
         tech =      ifelse(class8 == 3, 1, 0),
         prod =      ifelse(class8 == 4, 1, 0),
         manager =   ifelse(class8 == 5, 1, 0),
         clerks =    ifelse(class8 == 6, 1, 0),
         sociocult = ifelse(class8 == 7, 1, 0),
         service =   ifelse(class8 == 8, 1, 0))
df <- bind_rows(df1, df2)

df$class8_lab <- factor(df$class8, levels = c(1:8), labels = c("1 Self-employed professionals and large employers", 
                                                               "2 Small business owners",
                                                               "3 Technical (semi-)professionals",
                                                               "4 Production workers", 
                                                               "5 (Associate) managers",
                                                               "6 Clerks",
                                                               "7 Socio-cultural (semi-)professionals", 
                                                               "8 Service workers"))
# Interactions 
df <- df %>%
  mutate(agea_40 = ifelse(agea > 40, 1, 0),
         w_o = ifelse(gndr == 1 & agea_40 == 1, 1, 0),
         w_y = ifelse(gndr == 1 & agea_40 == 0, 1, 0),
         w_h = ifelse(gndr == 1 & eisced2 == 1, 1, 0),
         w_l = ifelse(gndr == 1 & eisced2 == 0, 1, 0),
         m_o = ifelse(gndr == 0 & agea_40 == 1, 1, 0),
         m_y = ifelse(gndr == 0 & agea_40 == 0, 1, 0),
         m_h = ifelse(gndr == 0 & eisced2 == 1, 1, 0),
         m_l = ifelse(gndr == 0 & eisced2 == 0, 1, 0),
         w_h_o = ifelse(gndr == 1 & eisced2 == 1 & agea_40 == 2, 1, 0), 
         w_h_y = ifelse(gndr == 1 & eisced2 == 1 & agea_40 == 1, 1, 0), 
         w_l_o = ifelse(gndr == 1 & eisced2 == 0 & agea_40 == 2, 1, 0), 
         w_l_y = ifelse(gndr == 1 & eisced2 == 0 & agea_40 == 1, 1, 0), 
         m_h_o = ifelse(gndr == 0 & eisced2 == 1 & agea_40 == 2, 1, 0), 
         m_h_y = ifelse(gndr == 0 & eisced2 == 1 & agea_40 == 1, 1, 0), 
         m_l_o = ifelse(gndr == 0 & eisced2 == 0 & agea_40 == 2, 1, 0), 
         m_l_y = ifelse(gndr == 0 & eisced2 == 0 & agea_40 == 1, 1, 0),
         city_w =      ifelse(gndr == 1 & domicil == 1, 1, 0),
         sub_w =       ifelse(gndr == 1 & domicil == 2, 1, 0),
         town_w =      ifelse(gndr == 1 & domicil == 3, 1, 0),
         vill_w =      ifelse(gndr == 1 & domicil == 4, 1, 0),
         count_w =     ifelse(gndr == 1 & domicil == 5, 1, 0),
         city_m =      ifelse(gndr == 0 & domicil == 1, 1, 0),
         sub_m =       ifelse(gndr == 0 & domicil == 2, 1, 0),
         town_m =      ifelse(gndr == 0 & domicil == 3, 1, 0),
         vill_m =      ifelse(gndr == 0 & domicil == 4, 1, 0),
         count_m =     ifelse(gndr == 0 & domicil == 5, 1, 0),
         city =      ifelse(domicil == 1, 1, 0),
         sub =       ifelse(domicil == 2, 1, 0),
         town =      ifelse(domicil == 3, 1, 0),
         vill =      ifelse(domicil == 4, 1, 0),
         count =     ifelse(domicil == 5, 1, 0),
         yrbrn_cohort = ifelse(yrbrn >= 1997, 1,
                               ifelse(yrbrn <= 1996 & yrbrn >= 1981, 2,
                                      ifelse(yrbrn <= 1980 & yrbrn >= 1965, 3,
                                             ifelse(yrbrn <= 1964 & yrbrn >= 1946, 4,
                                                    ifelse(yrbrn <= 1945 & yrbrn >= 1928, 5,
                                                           ifelse(yrbrn <= 1927, 6, NA))))))) %>%
  mutate(w_so = ifelse(gndr == 2 & class8 == "7 Socio-cultural (semi-)professionals", 1, 0),
         w_not_so = ifelse(gndr == 2 & class8 != "7 Socio-cultural (semi-)professionals", 1, 0),
         m_so = ifelse(gndr == 1 & class8 == "7 Socio-cultural (semi-)professionals", 1, 0),
         m_not_so = ifelse(gndr == 1 & class8 != "7 Socio-cultural (semi-)professionals", 1, 0))

# Recoded items round 8 and 1
df <- df %>%
  mutate(
    ecohenv = ecohenv*(-1) + 6,
    clmchng = ifelse(clmchng <= 2, 1,
                     ifelse(clmchng >= 3, 0, NA)))

# Final region grouping used in the analysis (overwrites the earlier
# region variable, relabeling "East" as "Central/Eastern")
df <- df %>%
  mutate(region = case_when(
    cntry %in% c("AT", "BE", "DE", "CH", "LU", "FR", "GB", "NL", "IE") ~ "West",
    cntry %in% c("CZ", "EE", "HU", "AL", "BG", "HR", "XK", "LV", "LT", "PL", "SI", "ME", "MK", "RO", "RS", "SK", "UA") ~ "Central/Eastern",
    cntry %in% c("ES", "IT", "PT", "CY", "GR") ~ "South",
    cntry %in% c("FI", "IS", "DK", "NO", "SE") ~ "North",
    TRUE ~ "Other"
  ))



### Save ####
save(df, file = "01_Data/Ess1-11.R")



