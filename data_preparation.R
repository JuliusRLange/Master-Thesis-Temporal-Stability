### This script contains the code for the data cleanup as well as some preliminary
### exploration steps.

# -------------open libraries---------------

library(tidyverse)
library(reshape2)
library(lubridate)

library("ggplot2")
theme_set(theme_bw())
library("sf")

library("rnaturalearth")
library("rnaturalearthdata")

library("worrms")

# ---------------establish world map for overview maps----------------

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

# -------------data import--------------

##DOME data

# set path for working directory
setwd("D:/Studium/Master/Masterarbeit/Thema 2 Stabilität/Daten und Datenverarbeitung")

# import raw data
zoobenthos<-read.csv("zoobenthos.csv")

# ----------------first overview-----------------------

names(zoobenthos)                         # prameters included in the data set
unique(zoobenthos$CNTRY)                  # countries conducting samples
sort(unique(zoobenthos$MYEAR))            # years of sampling

unique(zoobenthos$MPROG)                  #monitoring programs:

# NATL = National Monitoring Programme
# COMB = HELCOM Cooperative Monitoring in the Baltic Marine Environment (COMBINE)
# JMP = OSPAR Joint Monitoring Programme
# BMP = HELCOM Baltic Monitoring Programme
# CEMP = OSPAR Coordinated Environmental Monitoring Programme 
# PROJ = Project Data
# WFD = Water Framework Directive 

unique(zoobenthos$Final_MUNIT)            # units used

# check which units were used in which years
unique(zoobenthos$MYEAR[zoobenthos$Final_MUNIT=="g/m2"])
unique(zoobenthos$MYEAR[zoobenthos$Final_MUNIT=="nr/m3"])
unique(zoobenthos$MYEAR[zoobenthos$Final_MUNIT=="nr/m2"])
unique(zoobenthos$MYEAR[zoobenthos$Final_MUNIT=="mg/m2"])

unique(zoobenthos$SMTYP)                  # sampler types:

# VV = Van Veen Grab
# SM = Smith-McIntyre Grab
# BC = Box Corer 
# EK = Ekman Grab
# HOS = Hose
# HP = Haps Corer
# HC = Hand Corer
# GUG = Gunther Grab
# PT = Petersen Grab
# KK = Kieler Kinderwagen Dredge 
# KA = Kajak Corer

# list all combinations of sampler type and unit along with number of taxa, 
# number of samples and sampling area
test<-zoobenthos %>%
  group_by(SMTYP,Final_MUNIT,MUNIT) %>% 
  summarise(S = length(unique(AphiaID_accepted)), # number of sampled taxa
            N = length(Final_value), # number of sampling values
            A=length(unique(SAREA))) # number of different sampling areas

# list sampled taxa with unit of sampling, minimum-, maximum- and mean values
test<-zoobenthos %>%
  group_by(AphiaID_accepted,Final_MUNIT) %>% 
  summarise(min = min(Final_value),
            max = max(Final_value),
            mean = mean(Final_value))


#----------------data cleanup-------------------

zoobenthos$Final_value[zoobenthos$Final_MUNIT=="mg/m2"]<-zoobenthos$Final_value[zoobenthos$Final_MUNIT=="mg/m2"]/1000
zoobenthos$Final_MUNIT[zoobenthos$Final_MUNIT=="mg/m2"]<-"g/m2"

# test if conversion was successful
test<-zoobenthos %>%
  group_by(SMTYP,Final_MUNIT,PARAM) %>% 
  summarise(S = length(unique(AphiaID_accepted)),
            y = length(unique(MYEAR)),
            N = length(Final_value),
            A=length(unique(SAREA)))

zoobenthos<-zoobenthos[zoobenthos$PARAM=="BMWETWT",] # remove samples measuring 
                                                     # abundance
unique(zoobenthos$STATN)                             # list of station names
unique(zoobenthos$STNNO)                             # list of station id numbers

zoobenthos$StationID<-zoobenthos$STATN               # column for corrected station
                                                     # names
unique(sort(zoobenthos$StationID))                   # list all StationIDs

zoobenthos$checked_name <- zoobenthos$WoRMS_accepted_name  # column for corrected
                                                           # taxon names
zoobenthos$checked_ID <- zoobenthos$AphiaID_accepted       # column for corrected
                                                           # taxon IDs

# extract year and month of sampling from sampling date and put them in their
# own columns

zoobenthos <- zoobenthos %>%
  mutate(
    year = year(DATE),
    month = month(DATE),
    )  

test <- zoobenthos %>%
  group_by(MYEAR, DATE, year, month, StationID) %>%
  summarise()

test <- subset(test, MYEAR != year) # check if there are cases where MYEAR is
                                    # different from the year of sampling

# -----------------sampling methods--------------------

# list combinations of sampler type and sampler area, along with numbers of years
# combination was used in, countries using combination and stations sampled
test <- zoobenthos %>%
  group_by(SMTYP, SAREA) %>%
  summarise(y = length(unique(year)),
            cntr = length(unique(CNTRY)),
            stat = length(unique(StationID)))

# grab types to be used in final data set
grab_type <- c("SM", "VV")

# sampler areas to be used in final data set
test <- subset(zoobenthos, SMTYP %in% grab_type &
                 SAREA >= 930.000 & SAREA <= 1260.000)

test <- test %>%
  group_by(CNTRY) %>%
  summarise(y = length(unique(year)),
            cntr = length(unique(CNTRY)),
            stat = length(unique(StationID)))

# ------------------manually merge station duplicates------------------------


zoobenthos$StationID[zoobenthos$StationID=="107_LAG9"]<-"107"
zoobenthos$StationID[zoobenthos$StationID=="102A_LAG29"]<-"102A"
zoobenthos$StationID[zoobenthos$StationID=="111_LAG11"]<-"111"
zoobenthos$StationID[zoobenthos$StationID=="119_LAG25"]<-"119"
zoobenthos$StationID[zoobenthos$StationID=="120_LAG20"]<-"120"
zoobenthos$StationID[zoobenthos$StationID=="121_LAG1"]<-"121"
zoobenthos$StationID[zoobenthos$StationID=="121A_LAG15"]<-"121A"
zoobenthos$StationID[zoobenthos$StationID=="135_LAG21"]<-"135"
zoobenthos$StationID[zoobenthos$StationID=="137A_LAG23"]<-"137A"
zoobenthos$StationID[zoobenthos$StationID=="142_LAG17"]<-"142"
zoobenthos$StationID[zoobenthos$StationID=="163_LAG27"]<-"163"
zoobenthos$StationID[zoobenthos$StationID=="165_LAG32"]<-"165"
zoobenthos$StationID[zoobenthos$StationID=="167_LAG35"]<-"167"
zoobenthos$StationID[zoobenthos$StationID=="170_LAG33"]<-"170"
zoobenthos$StationID[zoobenthos$StationID=="3125 LAUSVIKEN"]<-"3125"
zoobenthos$StationID[zoobenthos$StationID=="3127 NÄR"]<-"3127"
zoobenthos$StationID[zoobenthos$StationID=="3129 ESE NÄR"]<-"3129_77J1"
zoobenthos$StationID[zoobenthos$StationID=="6001 KÄFTUDDEN"]<-"6001"
zoobenthos$StationID[zoobenthos$StationID=="6004 ASENSKALLEN"]<-"6004"
zoobenthos$StationID[zoobenthos$StationID=="6009 ÖSTERBÅDARNA"]<-"6009"
zoobenthos$StationID[zoobenthos$StationID=="6010 FURHOLMARNA"]<-"6010"
zoobenthos$StationID[zoobenthos$StationID=="6011 TRUTKLUBBEN"]<-"6011"
zoobenthos$StationID[zoobenthos$StationID=="6017 S SUNDSBÅDARNA"]<-"6017"
zoobenthos$StationID[zoobenthos$StationID=="6018 N GRÅSKÄR"]<-"6018 GRÅSKÄR"
zoobenthos$StationID[zoobenthos$StationID=="6019 S NYGRUND"]<-"6019"
zoobenthos$StationID[zoobenthos$StationID=="6020 Y HÅLLSFJÄRDEN"]<-"6020"
zoobenthos$StationID[zoobenthos$StationID=="6022 LILLBERGET"]<-"6022"
zoobenthos$StationID[zoobenthos$StationID=="6024 V VÄSTRA RÖKO"]<-"6024"
zoobenthos$StationID[zoobenthos$StationID=="6025 SKVALLRAN"]<-"6025"
zoobenthos$StationID[zoobenthos$StationID=="Als"]<-"ALS"
zoobenthos$StationID[zoobenthos$StationID=="ANHOLT_77R3"]<-"ANHOLT E"
zoobenthos$StationID[zoobenthos$StationID=="ASKERÖFJ"]<-"ASKERÖFJORDEN"
zoobenthos$StationID[zoobenthos$StationID=="ASKIMSFJORDEN 1"]<-"ASKIMSFJORDEN_1"
zoobenthos$StationID[zoobenthos$StationID=="ASKIMSFJORDEN 3"]<-"ASKIMSFJORDEN_3"
zoobenthos$StationID[zoobenthos$StationID=="ASKIMSFJORDEN 5"]<-"ASKIMSFJORDEN_5"
zoobenthos$StationID[zoobenthos$StationID=="BF Basnæs Nor"]<-"BF Basnaes Nor"
zoobenthos$StationID[zoobenthos$StationID=="BF Karrebæksminde Bugt"]<-"BF Karrebaeksminde Bugt"
zoobenthos$StationID[zoobenthos$StationID=="BF Korsør Nor"]<-"BF Korsoer Nor"
zoobenthos$StationID[zoobenthos$StationID=="BF Præstø Fjord"]<-"BF Praestoe Fjord"
zoobenthos$StationID[zoobenthos$StationID=="BF_NIVaa"]<-"BF_NIVÅ"
zoobenthos$StationID[zoobenthos$StationID=="BMPK1_K"]<-"BMPK1"
zoobenthos$StationID[zoobenthos$StationID=="BMPK10_K"]<-"BMPK10"
zoobenthos$StationID[zoobenthos$StationID=="BMPK11_K"]<-"BMPK11"
zoobenthos$StationID[zoobenthos$StationID=="BMPK12_K"]<-"BMPK12"
zoobenthos$StationID[zoobenthos$StationID=="BMPK13_K"]<-"BMPK13"
zoobenthos$StationID[zoobenthos$StationID=="BMPL1_L"]<-"BMPL1"
zoobenthos$StationID[zoobenthos$StationID=="DANA FJORD 6"]<-"DANA FJORD_6"
zoobenthos$StationID[zoobenthos$StationID=="DANA FJORD 7"]<-"DANA FJORD_7"
zoobenthos$StationID[zoobenthos$StationID=="DANA FJORD 8"]<-"DANA FJORD_8"
zoobenthos$StationID[zoobenthos$StationID=="DANA FJORD 9"]<-"DANA FJORD_9"
zoobenthos$StationID[zoobenthos$StationID=="F9 / A13"]<-"F9"
zoobenthos$StationID[zoobenthos$StationID=="F 9"]<-"F9"
zoobenthos$StationID[zoobenthos$StationID=="FK2"]<-"FK 2"
zoobenthos$StationID[zoobenthos$StationID=="FLADEN_77R6"]<-"FLADEN"
zoobenthos$StationID[zoobenthos$StationID=="GÖTEBORG YTTRE KUST 9"]<-"GÖTEBORG YTTRE KUST_9"
zoobenthos$StationID[zoobenthos$StationID=="GÖTEBORG YTTRE KUST 8"]<-"GÖTEBORG YTTRE KUST_8"
zoobenthos$StationID[zoobenthos$StationID=="GÖTEBORG YTTRE KUST 7"]<-"GÖTEBORG YTTRE KUST_7"
zoobenthos$StationID[zoobenthos$StationID=="GÖTEBORG YTTRE KUST 6"]<-"GÖTEBORG YTTRE KUST_6"
zoobenthos$StationID[zoobenthos$StationID=="GÖTEBORG YTTRE KUST 5"]<-"GÖTEBORG YTTRE KUST_5"
zoobenthos$StationID[zoobenthos$StationID=="GÖTEBORG YTTRE KUST 4"]<-"GÖTEBORG YTTRE KUST_4"
zoobenthos$StationID[zoobenthos$StationID=="GÖTEBORG YTTRE KUST 3"]<-"GÖTEBORG YTTRE KUST_3"
zoobenthos$StationID[zoobenthos$StationID=="GÖTEBORG YTTRE KUST 2"]<-"GÖTEBORG YTTRE KUST_2"
zoobenthos$StationID[zoobenthos$StationID=="GÖTEBORG YTTRE KUST 1"]<-"GÖTEBORG YTTRE KUST_1"
zoobenthos$StationID[zoobenthos$StationID=="Habitatomr nr 96"]<-"Habitatområde nr. 96"
zoobenthos$StationID[zoobenthos$StationID=="HALSE ASKERÖFJORD 1"]<-"HALSE ASKERÖFJORD_1"
zoobenthos$StationID[zoobenthos$StationID=="HAVSTENSFJORD 1"]<-"HAVSTENSFJORD_1"
zoobenthos$StationID[zoobenthos$StationID=="HAVSTENSFJORD 2"]<-"HAVSTENSFJORD_2"
zoobenthos$StationID[zoobenthos$StationID=="HAVSTENSFJORD 3"]<-"HAVSTENSFJORD_3"
zoobenthos$StationID[zoobenthos$StationID=="HAVSTENSFJORD 4"]<-"HAVSTENSFJORD_4"
zoobenthos$StationID[zoobenthos$StationID=="HAVSTENSFJORD 5"]<-"HAVSTENSFJORD_5"
zoobenthos$StationID[zoobenthos$StationID=="HAVSTENSFJORD 6"]<-"HAVSTENSFJORD_6"
zoobenthos$StationID[zoobenthos$StationID=="HAVSTENSFJORD 7"]<-"HAVSTENSFJORD_7"
zoobenthos$StationID[zoobenthos$StationID=="HAVSTENSFJORD 8"]<-"HAVSTENSFJORD_8"
zoobenthos$StationID[zoobenthos$StationID=="HAVSTENSFJORD 9"]<-"HAVSTENSFJORD_9"

zoobenthos$StationID[zoobenthos$StationID=="HK  7"]<-"HK 7"
zoobenthos$StationID[zoobenthos$StationID=="I:1"]<-"I_1"
zoobenthos$StationID[zoobenthos$StationID=="I:2"]<-"I_2"
zoobenthos$StationID[zoobenthos$StationID=="IrishSea_LarneLoughMid_se"]<-"IrishSea_LarneLoughMid_se01"
zoobenthos$StationID[zoobenthos$StationID=="Ivb 1"]<-"IVB 1"
zoobenthos$StationID[zoobenthos$StationID=="Ka1"]<-"KA1"
zoobenthos$StationID[zoobenthos$StationID=="Ka2"]<-"KA2"
zoobenthos$StationID[zoobenthos$StationID=="Ka3"]<-"KA3"
zoobenthos$StationID[zoobenthos$StationID=="Ka4"]<-"KA4"
zoobenthos$StationID[zoobenthos$StationID=="KATTEGATT UTSJÖ 1"]<-"KATTEGATT UTSJÖ_1"
zoobenthos$StationID[zoobenthos$StationID=="KATTEGATT UTSJÖ 3"]<-"KATTEGATT UTSJÖ_3"
zoobenthos$StationID[zoobenthos$StationID=="KF3_KÅLLA-/GÅSEFJÄRDEN"]<-"KF3_Kålla-/Gåsefjärden"
zoobenthos$StationID[zoobenthos$StationID=="KLÄDESHOLMEN 1"]<-"KLÄDESHOLMEN_1"
zoobenthos$StationID[zoobenthos$StationID=="KLÄDESHOLMEN 3"]<-"KLÄDESHOLMEN_3"
zoobenthos$StationID[zoobenthos$StationID=="Koljöfjord"]<-"KOLJÖFJORD_2"
zoobenthos$StationID[zoobenthos$StationID=="KUNGSHAMN S 1"]<-"KUNGSHAMN_S_1"
zoobenthos$StationID[zoobenthos$StationID=="KUNGSHAMN S 3"]<-"KUNGSHAMN_S_3"
zoobenthos$StationID[zoobenthos$StationID=="KUNGSHAMN S 5"]<-"KUNGSHAMN_S_5"
zoobenthos$StationID[zoobenthos$StationID=="Læsø Syd"]<-"Laesoe Syd"
zoobenthos$StationID[zoobenthos$StationID=="LD2_Lindödjupet"]<-"LD2_LINDÖDJUPET"
zoobenthos$StationID[zoobenthos$StationID=="Løgstør omr"]<-"Loegstoer omr"
zoobenthos$StationID[zoobenthos$StationID=="Ma1"]<-"MA1"
zoobenthos$StationID[zoobenthos$StationID=="MF Havnø"]<-"MF Havnoe"
zoobenthos$StationID[zoobenthos$StationID=="MinchMalin_BannEstuary_se01"]<-"MinchMalin_BannEstuary_wa01"
zoobenthos$StationID[zoobenthos$StationID=="MinchMalin_FoyleFaughanE_se"]<-"MinchMalin_FoyleFaughanE_se01"
zoobenthos$StationID[zoobenthos$StationID=="S KOSTERFJORDEN 3"]<-"S KOSTERFJORD"
zoobenthos$StationID[zoobenthos$StationID=="SALTKÄLLEFJORDEN"]<-"SALTKÄLLEFJORDEN_1"
zoobenthos$StationID[zoobenthos$StationID=="SÖDRA_ÖRESUND_2"]<-"SÖDRA ÖRESUND_2"
zoobenthos$StationID[zoobenthos$StationID=="SÖDRA_ÖRESUND_3"]<-"SÖDRA ÖRESUND_3"
zoobenthos$StationID[zoobenthos$StationID=="SÖDRA_ÖRESUND_4"]<-"SÖDRA ÖRESUND_4"
zoobenthos$StationID[zoobenthos$StationID=="SÖDRA_ÖRESUND_5"]<-"SÖDRA ÖRESUND_5"
zoobenthos$StationID[zoobenthos$StationID=="Thyborøn habitat"]<-"Thyboroen habitat"

# finland

zoobenthos$StationID[zoobenthos$StationID=="SR1a"]<-"SR1A"
zoobenthos$StationID[zoobenthos$StationID=="XXVI"]<-"XLIV"

# germany

zoobenthos$StationID[zoobenthos$StationID=="OMBMPN3"]<-"BMPN3"
zoobenthos$StationID[zoobenthos$StationID=="OMBMPN1"]<-"BMPN1"

# poland

zoobenthos$StationID[zoobenthos$StationID=="BMPK10_K"]<-"BMPK10"
zoobenthos$StationID[zoobenthos$StationID=="BMPK11_K"]<-"BMPK11"
zoobenthos$StationID[zoobenthos$StationID=="BMPK12_K"]<-"BMPK12"
zoobenthos$StationID[zoobenthos$StationID=="BMPK13_K"]<-"BMPK13"
zoobenthos$StationID[zoobenthos$StationID=="BMPK14_K"]<-"BMPK14"
zoobenthos$StationID[zoobenthos$StationID=="BMPK1_K"]<-"BMPK1"
zoobenthos$StationID[zoobenthos$StationID=="B13BMPK14_K"]<-"BMPK14"
zoobenthos$StationID[zoobenthos$StationID=="K13"]<-"BMPK13"
zoobenthos$StationID[zoobenthos$StationID=="K12"]<-"BMPK12"
zoobenthos$StationID[zoobenthos$StationID=="K10"]<-"BMPK10"
zoobenthos$StationID[zoobenthos$StationID=="K11"]<-"BMPK11"
zoobenthos$StationID[zoobenthos$StationID=="BMPL1_L"]<-"BMPL1"
zoobenthos$StationID[zoobenthos$StationID=="OM1P"]<-"BMPK10"

# latvia

zoobenthos$StationID[zoobenthos$StationID=="167B"]<-"167"
zoobenthos$StationID[zoobenthos$StationID=="163B"]<-"163"
zoobenthos$StationID[zoobenthos$StationID=="101A"]<-"165"
zoobenthos$StationID[zoobenthos$StationID=="BMPG1"]<-"121"

# lithuania

zoobenthos$StationID[zoobenthos$StationID=="20A"]<-"20"

# sweden

zoobenthos$StationID[zoobenthos$StationID=="LD2_LINDÖDJUPET"]<-"LD2_Lindödjupet"
zoobenthos$StationID[zoobenthos$StationID=="KF3_KÅLLA-/GÅSEFJÄRDEN"]<-"KF3_Kålla-/Gåsefjärden"
zoobenthos$StationID[zoobenthos$StationID=="P 204"]<-"P204"
zoobenthos$StationID[zoobenthos$StationID=="P 206"]<-"P206"
zoobenthos$StationID[zoobenthos$StationID=="SR 1A"]<-"SR1A"
zoobenthos$StationID[zoobenthos$StationID=="Sk33"]<-"SK33"
zoobenthos$StationID[zoobenthos$StationID=="Sk32"]<-"SK32"
zoobenthos$StationID[zoobenthos$StationID=="Sk35"]<-"SK35"
zoobenthos$StationID[zoobenthos$StationID=="Sk36"]<-"SK36"
zoobenthos$StationID[zoobenthos$StationID=="DANA FJORD_6"]<-"DANA FJORD 6"
zoobenthos$StationID[zoobenthos$StationID=="DANA FJORD_7"]<-"DANA FJORD 7"
zoobenthos$StationID[zoobenthos$StationID=="DANA FJORD_8"]<-"DANA FJORD 8"
zoobenthos$StationID[zoobenthos$StationID=="DANA FJORD_9"]<-"DANA FJORD 9"
zoobenthos$StationID[zoobenthos$StationID=="Ka4"]<-"KA4"
zoobenthos$StationID[zoobenthos$StationID=="Danafjord"]<-"KA4"
zoobenthos$StationID[zoobenthos$StationID=="ASKIMSFJORDEN_1"]<-"ASKIMSFJORDEN 1"
zoobenthos$StationID[zoobenthos$StationID=="ASKIMSFJORDEN_3"]<-"ASKIMSFJORDEN 3"
zoobenthos$StationID[zoobenthos$StationID=="ASKIMSFJORDEN_5"]<-"ASKIMSFJORDEN 5"
zoobenthos$StationID[zoobenthos$StationID=="AsKIMSFJORDEN 1"]<-"ASKIMSFJORDEN 1"
zoobenthos$StationID[zoobenthos$StationID=="AsKIMSFJORDEN 3"]<-"ASKIMSFJORDEN 3"
zoobenthos$StationID[zoobenthos$StationID=="AsKIMSFJORDEN 5"]<-"ASKIMSFJORDEN 5"
zoobenthos$StationID[zoobenthos$StationID=="GÖTEBORG YTTRE KUST_1"]<-"GÖTEBORG YTTRE KUST 1"
zoobenthos$StationID[zoobenthos$StationID=="GÖTEBORG YTTRE KUST_2"]<-"GÖTEBORG YTTRE KUST 2"
zoobenthos$StationID[zoobenthos$StationID=="GÖTEBORG YTTRE KUST_3"]<-"GÖTEBORG YTTRE KUST 3"
zoobenthos$StationID[zoobenthos$StationID=="GÖTEBORG YTTRE KUST_4"]<-"GÖTEBORG YTTRE KUST 4"
zoobenthos$StationID[zoobenthos$StationID=="GÖTEBORG YTTRE KUST_5"]<-"GÖTEBORG YTTRE KUST 5"
zoobenthos$StationID[zoobenthos$StationID=="GÖTEBORG YTTRE KUST_6"]<-"GÖTEBORG YTTRE KUST 6"
zoobenthos$StationID[zoobenthos$StationID=="GÖTEBORG YTTRE KUST_7"]<-"GÖTEBORG YTTRE KUST 7"
zoobenthos$StationID[zoobenthos$StationID=="GÖTEBORG YTTRE KUST_8"]<-"GÖTEBORG YTTRE KUST 8"
zoobenthos$StationID[zoobenthos$StationID=="GÖTEBORG YTTRE KUST_9"]<-"GÖTEBORG YTTRE KUST 9"
zoobenthos$StationID[zoobenthos$StationID=="KATTEGATT UTSJÖ_1"]<-"KATTEGATT UTSJÖ 1"
zoobenthos$StationID[zoobenthos$StationID=="KATTEGATT UTSJÖ_3"]<-"KATTEGATT UTSJÖ 3"
zoobenthos$StationID[zoobenthos$StationID=="ONSALA N YTTRE KUST_12"]<-"ONSALA KUST_5"
zoobenthos$StationID[zoobenthos$StationID=="ONSALA N YTTRE KUST_11"]<-"ONSALA KUST_4"
zoobenthos$StationID[zoobenthos$StationID=="Ka2"]<-"KA2"
zoobenthos$StationID[zoobenthos$StationID=="FLADEN_77R6"]<-"KA2"
zoobenthos$StationID[zoobenthos$StationID=="FLADEN"]<-"ONSALA KUST_5"
zoobenthos$StationID[zoobenthos$StationID=="Ka3"]<-"KA3"
zoobenthos$StationID[zoobenthos$StationID=="ANHOLT_77R3"]<-"KA3"
zoobenthos$StationID[zoobenthos$StationID=="ANHOLT E"]<-"KA3"
zoobenthos$StationID[zoobenthos$StationID=="LAHOLMSBUKTEN_YTTRE_6"]<-"KA6"
zoobenthos$StationID[zoobenthos$StationID=="N ÖRESUND_6"]<-"NORRA ÖRESUND_2"
zoobenthos$StationID[zoobenthos$StationID=="N ÖRESUND_3"]<-"NORRA ÖRESUND_3"
zoobenthos$StationID[zoobenthos$StationID=="N ÖRESUND_5"]<-"OVF 1_3"
zoobenthos$StationID[zoobenthos$StationID=="N ÖRESUND_4"]<-"NORRA ÖRESUND_4"
zoobenthos$StationID[zoobenthos$StationID=="N ÖRESUND_1"]<-"NORRA ÖRESUND_1"
zoobenthos$StationID[zoobenthos$StationID=="SÖDRA_ÖRESUND_2"]<-"SÖDRA ÖRESUND_2"
zoobenthos$StationID[zoobenthos$StationID=="SÖDRA_ÖRESUND_3"]<-"SÖDRA ÖRESUND_3"
zoobenthos$StationID[zoobenthos$StationID=="SÖDRA_ÖRESUND_4"]<-"SÖDRA ÖRESUND_4"
zoobenthos$StationID[zoobenthos$StationID=="SÖDRA_ÖRESUND_5"]<-"SÖDRA ÖRESUND_5"
zoobenthos$StationID[zoobenthos$StationID=="OVF 4_9"]<-"SÖDRA ÖRESUND_1"
zoobenthos$StationID[zoobenthos$StationID=="SK 4"]<-"SK4"
zoobenthos$StationID[zoobenthos$StationID=="SK 6"]<-"SK6"
zoobenthos$StationID[zoobenthos$StationID=="I:1"]<-"I_1"
zoobenthos$StationID[zoobenthos$StationID=="I:2"]<-"I_2"
zoobenthos$StationID[zoobenthos$StationID=="MA1"]<-"KF2_Kålla-/Gåsefjärden"
zoobenthos$StationID[zoobenthos$StationID=="TORHAMN 4"]<-"KF4_KÅLLA-/GÅSEFJÄRDEN"
zoobenthos$StationID[zoobenthos$StationID=="Torhamn 4"]<-"KF4_KÅLLA-/GÅSEFJÄRDEN"
zoobenthos$StationID[zoobenthos$StationID=="H128"]<-"H 128"
zoobenthos$StationID[zoobenthos$StationID=="3129_77J1"]<-"3129 ESE NÄR"
zoobenthos$StationID[zoobenthos$StationID=="3127"]<-"3127 NÄR"
zoobenthos$StationID[zoobenthos$StationID=="3125"]<-"3125 LAUSVIKEN"
zoobenthos$StationID[zoobenthos$StationID=="Ivb 1"]<-"IVB 1"
zoobenthos$StationID[zoobenthos$StationID=="HAE 8B"]<-"HAE 8"
zoobenthos$StationID[zoobenthos$StationID=="HK  7"]<-"HK 7"
zoobenthos$StationID[zoobenthos$StationID=="HK 15B"]<-"HK 15"
zoobenthos$StationID[zoobenthos$StationID=="6001"]<-"6001 KÄFTUDDEN"
zoobenthos$StationID[zoobenthos$StationID=="6004"]<-"6004 ASENSKALLEN"
zoobenthos$StationID[zoobenthos$StationID=="6009"]<-"6009 ÖSTERBÅDARNA"
zoobenthos$StationID[zoobenthos$StationID=="6010"]<-"6010 FURHOLMARNA"
zoobenthos$StationID[zoobenthos$StationID=="6011"]<-"6011 TRUTKLUBBEN"
zoobenthos$StationID[zoobenthos$StationID=="6017"]<-"6017 S SUNDSBÅDARNA"
zoobenthos$StationID[zoobenthos$StationID=="6019"]<-"6019 S NYGRUND"
zoobenthos$StationID[zoobenthos$StationID=="6020"]<-"6020 Y HÅLLSFJÄRDEN"
zoobenthos$StationID[zoobenthos$StationID=="6022"]<-"6022 LILLBERGET"
zoobenthos$StationID[zoobenthos$StationID=="6025"]<-"6025 SKVALLRAN"
zoobenthos$StationID[zoobenthos$StationID=="6018 N GRÅSKÄR"]<-"6018 GRÅSKÄR"
zoobenthos$StationID[zoobenthos$StationID=="SB 17b"]<-"SB 17"
zoobenthos$StationID[zoobenthos$StationID=="SB 81"]<-"SB 8"
zoobenthos$StationID[zoobenthos$StationID=="C3_South"]<-"C 3"
zoobenthos$StationID[zoobenthos$StationID=="C3_SOUTH"]<-"C 3"
zoobenthos$StationID[zoobenthos$StationID=="BPC11"]<-"SR1A"
zoobenthos$StationID[zoobenthos$StationID=="F9 / A13"]<-"F9"
zoobenthos$StationID[zoobenthos$StationID=="B1-BOTTENVIKEN"]<-"B 1"
zoobenthos$StationID[zoobenthos$StationID=="FK2"]<-"FK 2"

# -----------------overview samples per year--------------------------

cntr <- c("Denmark", "Finland", "Germany", "Poland", "Latvia", "Lithuania", "Sweden")

IDlist <- subset(zoobenthos, CNTRY %in% cntr & SMTYP %in% grab_type & 
                   SAREA >= 930.000 & SAREA <= 1260.000 & StationID != "") %>%
  group_by(StationID, year, CNTRY) %>%
  summarise()

test <- length(unique(IDlist$StationID))

#ggplot(data = IDlist, aes(colour = factor(CNTRY))) +
  #geom_point(aes(x = year, y = StationID), size = 0.1) +
  #theme(axis.text.y = element_text(size = 0.5))

#IDcntr <- subset(IDlist, CNTRY == "Latvia")

#ggplot(data = IDcntr) +
  #geom_point(aes(x = year, y = StationID), size = 0.2)

# -------------------testing two potential subsets------------------

 #subset A: 2007-2022

#temp <- subset(zoobenthos, CNTRY %in% cntr & SMTYP %in% grab_type & 
                #SAREA >= 930.000 & SAREA <= 1260.000 & StationID != ""
              #& year %in% c(2007:2022)) %>%
  #group_by(StationID) %>%
  #summarise(y = length(unique(year)))

#IDXA <- subset(temp, y == 15) # stations with only one year missing
#IDXA <- as.vector(IDXA$StationID)

#IDA <- subset(temp, y == 16)

#IDA <- as.vector(IDA$StationID)

#temp <- subset(zoobenthos, year %in% c(2007:2022) & StationID %in% IDXA) %>%
  #group_by(StationID, year, CNTRY) %>%
  #summarise()

#ggplot(data = temp, aes(colour = factor(CNTRY))) +        #Test
  #geom_point(aes(x = year, y = StationID), size = 1) +
  #theme(axis.text.y = element_text(size = 8))

#del <- c("K23") # lösche K23 aus IDXA

#IDXA <- setdiff(IDXA, del)

#IDA <- c(IDA, IDXA) # insert stations from IDXA into IDA 

# subset B: 2010-2022

temp <- subset(zoobenthos, CNTRY %in% cntr & SMTYP %in% grab_type & 
                 SAREA >= 930.000 & SAREA <= 1260.000 & StationID != ""
               & year %in% c(2010:2022)) %>%
  group_by(StationID) %>%
  summarise(y = length(unique(year)))

IDXB <- subset(temp, y == 12)
IDXB <- as.vector(IDXB$StationID)

IDB <- subset(temp, y == 13) 

IDB <- as.vector(IDB$StationID)

#temp <- subset(zoobenthos, year %in% c(2010:2022) & StationID %in% IDXB) %>%
  #group_by(StationID, year, CNTRY) %>%
  #summarise()

#ggplot(data = temp, aes(colour = factor(CNTRY))) +        tTest
  #geom_point(aes(x = year, y = StationID), size = 1) +
  #theme(axis.text.y = element_text(size = 5))

del <- c("K23", "BMPK13", "AMN") # delete stations where a year other than 2019 
                                 # is missing

del <- c(del, "NB 3", "NB 5", "NB 1", "NB 7", "NB 8", "N 8", "N 6", "N 10",
         "N 7", "N 11") # delete stations in the Bay of Bothnia

IDB <- c(IDB, IDXB)

IDB <- setdiff(IDB, del)

### overview maps:

# subset A:

#temp <- subset(zoobenthos, StationID %in% IDA) %>%
  #group_by(StationID) %>%
  #summarise(lat = mean(Latitude), lon = mean(Longitude))

#ggplot(data = world) +
  #geom_sf() +
  #geom_point(data = temp, aes(x = lon, y = lat)) +
  #coord_sf(xlim = c(8, 28), ylim = c(53, 66)) +
  #labs(title = "Sampling Stations 2008-2022")

#subset B:

#temp <- subset(zoobenthos, StationID %in% IDB) %>%
  #group_by(StationID) %>%
  #summarise(lat = mean(Latitude), lon = mean(Longitude))

#ggplot(data = world) +
  #geom_sf() +
  #geom_point(data = temp, aes(x = lon, y = lat)) +
  #coord_sf(xlim = c(8, 28), ylim = c(53, 66)) +
  #labs(title = "Sampling Stations 2010-2022 ohne 2019") #+
  #geom_text(data = temp, aes(x = lon, y = lat, label = StationID),
            #size = 2, position = position_nudge(x = 0.002, y= -0.002))

# find duplicates:

#ID_B <- subset(zoobenthos, StationID %in% IDXB) %>%
  #group_by(StationID, CNTRY) %>%
  #summarise(lat = mean(Latitude), lon = mean(Longitude))

#ID_ref <- subset(zoobenthos, year %in% c(2010:2022) & CNTRY %in% cntr & 
                   #SMTYP %in% grab_type & SAREA >= 930.000 & 
                   #SAREA <= 1260.000 & StationID != "") %>%
  #group_by(StationID, CNTRY) %>%
  #summarise(lat = mean(Latitude), lon = mean(Longitude))

#ggplot(data = world) +
  #geom_sf() +
  #geom_point(data = ID_ref, aes(x = lon, y = lat)) +
  #geom_point(data = ID_B, aes(x = lon, y = lat, colour = "red")) +
  #coord_sf(xlim = c(11.6, 11.8), ylim = c(57.2, 57.3)) +
  #geom_text(data = ID_B, aes(x = lon, y = lat, label = StationID, colour = "red"), 
            #size = 2, position = position_nudge(x = 0.006, y= -0.006)) +
  #geom_text(data = ID_ref, aes(x = lon, y = lat, label = StationID), 
            #size = 2, position = position_nudge(x = 0.006, y= -0.006)) +
  #labs(title = "Sampling Stations 2010-2022")

# -----------------Final subset---------------------------------

years <- c(2010:2018, 2020:2022) # years excluding 2019

zoo_b <- subset(zoobenthos, StationID %in% IDB & year %in% years)

### -----------------taxa and WORMSIDs--------------------------------

test <- zoo_b %>%
  group_by(WoRMS_accepted_name, AphiaID_accepted, checked_name, checked_ID, SPECI_name) %>%
  summarise()

test$status <- "N/a"

test$rank <- "N/a"

for (x in 1:nrow(test)) { # test if accepted WoRMS names are up to date
  
  ID <- as.numeric(test[x, "AphiaID_accepted"])
  
  if(is.na(ID) == TRUE) {
    
    test [x, "status"] <- "unaccepted"
    
  }
  
  else {

  rec <- wm_record(ID)
  
  test[x, "status"] <- rec$status
  
  test[x, "rank"] <- rec$rank} # show taxonomic level
}

zoobenthos$checked_name[
  zoobenthos$WoRMS_accepted_name == "Eteone barbata"
  ] <- "Mysta barbata"

zoobenthos$checked_ID[
  zoobenthos$WoRMS_accepted_name == "Eteone barbata"
] <- "147027"

ID_valid <- c() # list of valid IDs (species and genus)

for (x in 1:nrow(test)) {
  
  if ( test[x, "rank"] %in% c("Genus", "Species")) {
    
    ID_valid <- c(ID_valid, as.numeric(test[x, "AphiaID_accepted"]))
      
  }
}

test <- subset(test, AphiaID_accepted %in% ID_valid)

zoobenthos$checked_name[
  zoobenthos$WoRMS_accepted_name == "Bathyporeia sarsi" & 
    zoobenthos$SPECI_name == "Bathyporeia pilosa"
] <- "Bathyporeia pilosa"

zoobenthos$checked_ID[
  zoobenthos$WoRMS_accepted_name == "Bathyporeia sarsi" & 
    zoobenthos$SPECI_name == "Bathyporeia pilosa"
] <- "103068"

test <- subset(test, WoRMS_accepted_name != SPECI_name) 
# compare SPECI vs. accepted names

### ------------------last steps of data cleanup--------------------

zoo_b <- subset(zoobenthos, StationID %in% IDB & 
                  year %in% years &
                  checked_ID %in% ID_valid) # subset aktualisieren

# remove duplicates

zoo_b$is_add <- rep(FALSE, nrow(zoo_b))

zoo_b$is_add[
  zoo_b$StationID == "K18" &
    zoo_b$year == "2018" &
    zoo_b$month == "6"
] <- TRUE

zoo_b$is_add[
  zoo_b$StationID == "K18" &
    zoo_b$year == "2020" &
    zoo_b$month == "8"
] <- TRUE

zoo_b$is_add[
  zoo_b$StationID == "K18" &
    zoo_b$year == "2021" &
    zoo_b$month == "9"
] <- TRUE

zoo_b$is_add[
  zoo_b$StationID == "K18" &
    zoo_b$year == "2022" &
    zoo_b$month == "11"
] <- TRUE

zoo_b$is_add[
  zoo_b$StationID == "K32" &
    zoo_b$year == "2014" &
    zoo_b$month == "9"
] <- TRUE

zoo_b$is_add[
  zoo_b$StationID == "K41" &
    zoo_b$year == "2014" &
    zoo_b$month == "9"
] <- TRUE

zoo_b$is_add[
  zoo_b$StationID == "OMMVZBB15" &
    zoo_b$year == "2020" &
    zoo_b$month == "1"
] <- TRUE

zoo_b$is_add[
  zoo_b$StationID == "OMMVZBB22" &
    zoo_b$year == "2020" &
    zoo_b$month == "1"
] <- TRUE

zoo_b$is_add[
  zoo_b$StationID == "OMMVZBC15" &
    zoo_b$year == "2020" &
    zoo_b$month == "1"
] <- TRUE

zoo_b$is_add[
  zoo_b$StationID == "OMMVZBC20" &
    zoo_b$year == "2020" &
    zoo_b$month == "1"
] <- TRUE

zoo_b$is_add[
  zoo_b$StationID == "OMMVZBG26" &
    zoo_b$year == "2020" &
    zoo_b$month == "1"
] <- TRUE

zoo_b$is_add[
  zoo_b$StationID == "OMMVZBH20" &
    zoo_b$year == "2020" &
    zoo_b$month == "1"
] <- TRUE

zoo_b$is_add[
  zoo_b$StationID == "OMMVZBS15" &
    zoo_b$year == "2021" &
    zoo_b$month == "3"
] <- TRUE

zoo_b <- subset(zoo_b, is_add == FALSE)

# ---------------------write csv file-------------------------------

write.csv2(zoo_b, file = "zoobenthos_final.csv", fileEncoding = "UTF-8")

