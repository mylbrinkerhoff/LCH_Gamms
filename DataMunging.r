####################################################################
###
###   DataMunging.R
###   
###   M. Brinkerhoff  * UCSC  * 2024-05-14 (T)
###   
####################################################################
melt
# Libraries that are required
library(ggplot2)
library(mgcv)
library(itsadug)
library(tidyverse)
library(viridis)
library(reshape2)
library(readr)
source("R/gamm_hacks.R")

# Loading Data
slz <- read.csv("data/raw/Voice_Master_Split.csv", header = TRUE)

### Preprocessing the data
### convert certain columns into factors.
slz$Phonation <- factor(slz$Phonation, levels = c("modal", 
                                                "breathy", 
                                                "checked", 
                                                "laryngealized"))
slz$Speaker <- slz$Speaker %>% factor()
slz$Word <- slz$Word %>% factor()
slz$Vowel <- slz$Vowel %>% factor()
slz$Iter <- slz$Iter %>% factor()
slz$Tone <- slz$Tone %>% factor()

### Rename seg_End
colnames(slz)[colnames(slz) == 'seg_End'] <- 'Duration'

### Rearranging values for analysis and comparison
slz <- slz %>% 
    mutate(idnum = row_number())

#### h1h2c

slz_h1h2c <- slz %>% 
    select(Speaker, 
           Word, 
           Iter, 
           Vowel, 
           Phonation, 
           Tone, 
           Duration,
           idnum,
           H1H2c_means001,
           H1H2c_means002,
           H1H2c_means003,
           H1H2c_means004,
           H1H2c_means005,
           H1H2c_means006,
           H1H2c_means007,
           H1H2c_means008,
           H1H2c_means009,
           H1H2c_means010)

slz_h1h2c_trans <- melt(slz_h1h2c, id = c("Speaker",
                                        "Word", 
                                        "Iter", 
                                        "Vowel", 
                                        "Phonation", 
                                        'Tone', 
                                        "Duration",
                                        "idnum"))

slz_h1h2c_trans$measurement.no <- str_sub(slz_h1h2c_trans$variable,-2,-1)
slz_h1h2c_trans <-  slz_h1h2c_trans %>%
  rename(h1h2c = value) %>%
  select(-variable)

#### h1c
slz_h1c <-  slz %>%
  select(idnum, H1c_means001, H1c_means002, H1c_means003,
         H1c_means004, H1c_means005, H1c_means006,
         H1c_means007, H1c_means008, H1c_means009,
         H1c_means010)

slz_h1c_trans  <-  melt(slz_h1c, id = c("idnum"))
slz_h1c_trans$measurement.no  <-  str_sub(slz_h1c_trans$variable,-2,-1)
slz_h1c_trans <-  slz_h1c_trans %>%
  rename(h1c = value)%>%
  select(-variable)

#####  h2h4c
slz_h2h4c <-  slz %>%
  select(idnum, H2H4c_means001, H2H4c_means002, H2H4c_means003,
         H2H4c_means004, H2H4c_means005, H2H4c_means006,
         H2H4c_means007, H2H4c_means008, H2H4c_means009,
         H2H4c_means010)

slz_h2h4c_trans  <-  melt(slz_h2h4c, id = c("idnum"))
slz_h2h4c_trans$measurement.no  <-  str_sub(slz_h2h4c_trans$variable,-2,-1)
slz_h2h4c_trans <-  slz_h2h4c_trans %>%
  rename(h2h4c = value)%>%
  select(-variable)

##### h1a1c
slz_h1a1c <-  slz %>%
  select(idnum, H1A1c_means001, H1A1c_means002, H1A1c_means003,
         H1A1c_means004, H1A1c_means005, H1A1c_means006,
         H1A1c_means007, H1A1c_means008, H1A1c_means009,
         H1A1c_means010)

slz_h1a1c_trans  <-  melt(slz_h1a1c, id = c("idnum"))
slz_h1a1c_trans$measurement.no  <-  str_sub(slz_h1a1c_trans$variable,-2,-1)
slz_h1a1c_trans <-  slz_h1a1c_trans %>%
  rename(h1a1c = value)%>%
  select(-variable)

#### h1a2c
slz_h1a2c <-  slz %>%
  select(idnum, H1A2c_means001, H1A2c_means002, H1A2c_means003,
         H1A2c_means004, H1A2c_means005, H1A2c_means006,
         H1A2c_means007, H1A2c_means008, H1A2c_means009,
         H1A2c_means010)

slz_h1a2c_trans  <-  melt(slz_h1a2c, id = c("idnum"))
slz_h1a2c_trans$measurement.no  <-  str_sub(slz_h1a2c_trans$variable,-2,-1)
slz_h1a2c_trans <-  slz_h1a2c_trans %>%
  rename(h1a2c = value)%>%
  select(-variable)

#### h1a3c
slz_h1a3c <-  slz %>%
  select(idnum, H1A3c_means001, H1A3c_means002, H1A3c_means003,
         H1A3c_means004, H1A3c_means005, H1A3c_means006,
         H1A3c_means007, H1A3c_means008, H1A3c_means009,
         H1A3c_means010)

slz_h1a3c_trans  <-  melt(slz_h1a3c, id = c("idnum"))
slz_h1a3c_trans$measurement.no  <-  str_sub(slz_h1a3c_trans$variable,-2,-1)
slz_h1a3c_trans <-  slz_h1a3c_trans %>%
  rename(h1a3c = value)%>%
  select(-variable)

#### h2h4c
slz_h2h4c <-  slz %>%
  select(idnum, H2H4c_means001, H2H4c_means002, H2H4c_means003,
         H2H4c_means004, H2H4c_means005, H2H4c_means006,
         H2H4c_means007, H2H4c_means008, H2H4c_means009,
         H2H4c_means010)

slz_h2h4c_trans  <-  melt(slz_h2h4c, id = c("idnum"))
slz_h2h4c_trans$measurement.no  <-  str_sub(slz_h2h4c_trans$variable,-2,-1)
slz_h2h4c_trans <-  slz_h2h4c_trans %>%
  rename(h2h4c = value)%>%
  select(-variable)

#### h42Kc
slz_h42Kc <-  slz %>%
  select(idnum, H42Kc_means001, H42Kc_means002, H42Kc_means003,
         H42Kc_means004, H42Kc_means005, H42Kc_means006,
         H42Kc_means007, H42Kc_means008, H42Kc_means009,
         H42Kc_means010)

slz_h42Kc_trans  <-  melt(slz_h42Kc, id = c("idnum"))
slz_h42Kc_trans$measurement.no  <-  str_sub(slz_h42Kc_trans$variable,-2,-1)
slz_h42Kc_trans <-  slz_h42Kc_trans %>%
  rename(h42Kc = value)%>%
  select(-variable)

#### h2Kh5Kc
slz_h2Kh5Kc <-  slz %>%
  select(idnum, H2KH5Kc_means001, H2KH5Kc_means002, H2KH5Kc_means003,
         H2KH5Kc_means004, H2KH5Kc_means005, H2KH5Kc_means006,
         H2KH5Kc_means007, H2KH5Kc_means008, H2KH5Kc_means009,
         H2KH5Kc_means010)

slz_h2Kh5Kc_trans  <-  melt(slz_h2Kh5Kc, id = c("idnum"))
slz_h2Kh5Kc_trans$measurement.no  <-  str_sub(slz_h2Kh5Kc_trans$variable,-2,-1)
slz_h2Kh5Kc_trans <-  slz_h2Kh5Kc_trans %>%
  rename(h2Kh5Kc = value)%>%
  select(-variable)

#### cpp
slz_cpp <-  slz %>%
  select(idnum, CPP_means001, CPP_means002, CPP_means003,
         CPP_means004, CPP_means005, CPP_means006,
         CPP_means007, CPP_means008, CPP_means009,
         CPP_means010)

slz_cpp_trans  <-  melt(slz_cpp, id = c("idnum"))
slz_cpp_trans$measurement.no  <-  str_sub(slz_cpp_trans$variable,-2,-1)
slz_cpp_trans <-  slz_cpp_trans %>%
  rename(cpp = value)%>%
  select(-variable)

#### energy
slz_energy <-  slz %>%
  select(idnum, Energy_means001, Energy_means002, Energy_means003,
         Energy_means004, Energy_means005, Energy_means006,
         Energy_means007, Energy_means008, Energy_means009,
         Energy_means010)

slz_energy_trans  <-  melt(slz_energy, id = c("idnum"))
slz_energy_trans$measurement.no  <-  str_sub(slz_energy_trans$variable,-2,-1)
slz_energy_trans <-  slz_energy_trans %>%
  rename(energy = value)%>%
  select(-variable)

#### hnr05
slz_hnr05 <-  slz %>%
  select(idnum, HNR05_means001, HNR05_means002, HNR05_means003,
         HNR05_means004, HNR05_means005, HNR05_means006,
         HNR05_means007, HNR05_means008, HNR05_means009,
         HNR05_means010)

slz_hnr05_trans  <-  melt(slz_hnr05, id = c("idnum"))
slz_hnr05_trans$measurement.no  <-  str_sub(slz_hnr05_trans$variable,-2,-1)
slz_hnr05_trans <-  slz_hnr05_trans %>%
  rename(hnr05 = value)%>%
  select(-variable)

#### hnr15
slz_hnr15 <-  slz %>%
  select(idnum, HNR15_means001, HNR15_means002, HNR15_means003,
         HNR15_means004, HNR15_means005, HNR15_means006,
         HNR15_means007, HNR15_means008, HNR15_means009,
         HNR15_means010)

slz_hnr15_trans  <-  melt(slz_hnr15, id = c("idnum"))
slz_hnr15_trans$measurement.no  <-  str_sub(slz_hnr15_trans$variable,-2,-1)
slz_hnr15_trans <-  slz_hnr15_trans %>%
  rename(hnr15 = value)%>%
  select(-variable)

#### hnr25
slz_hnr25 <-  slz %>%
  select(idnum, HNR25_means001, HNR25_means002, HNR25_means003,
         HNR25_means004, HNR25_means005, HNR25_means006,
         HNR25_means007, HNR25_means008, HNR25_means009,
         HNR25_means010)

slz_hnr25_trans  <-  melt(slz_hnr25, id = c("idnum"))
slz_hnr25_trans$measurement.no  <-  str_sub(slz_hnr25_trans$variable,-2,-1)
slz_hnr25_trans <-  slz_hnr25_trans %>%
  rename(hnr25 = value)%>%
  select(-variable)

#### hnr35
slz_hnr35 <-  slz %>%
  select(idnum, HNR35_means001, HNR35_means002, HNR35_means003,
         HNR35_means004, HNR35_means005, HNR35_means006,
         HNR35_means007, HNR35_means008, HNR35_means009,
         HNR35_means010)

slz_hnr35_trans  <-  melt(slz_hnr35, id = c("idnum"))
slz_hnr35_trans$measurement.no  <-  str_sub(slz_hnr35_trans$variable,-2,-1)
slz_hnr35_trans <-  slz_hnr35_trans %>%
  rename(hnr35 = value)%>%
  select(-variable)

#### strF0
slz_strF0 <-  slz %>%
  select(idnum, strF0_means001, strF0_means002, strF0_means003,
         strF0_means004, strF0_means005, strF0_means006,
         strF0_means007, strF0_means008, strF0_means009,
         strF0_means010)

slz_strF0_trans  <-  melt(slz_strF0, id = c("idnum"))
slz_strF0_trans$measurement.no  <-  str_sub(slz_strF0_trans$variable,-2,-1)
slz_strF0_trans <-  slz_strF0_trans %>%
  rename(strF0 = value)%>%
  select(-variable)

#### sF1
slz_sF1 <-  slz %>%
  select(idnum, sF1_means001, sF1_means002, sF1_means003,
         sF1_means004, sF1_means005, sF1_means006,
         sF1_means007, sF1_means008, sF1_means009,
         sF1_means010)

slz_sF1_trans  <-  melt(slz_sF1, id = c("idnum"))
slz_sF1_trans$measurement.no  <-  str_sub(slz_sF1_trans$variable,-2,-1)
slz_sF1_trans <-  slz_sF1_trans %>%
  rename(sF1 = value)%>%
  select(-variable)

#### sF2
slz_sF2 <-  slz %>%
  select(idnum, sF2_means001, sF2_means002, sF2_means003,
         sF2_means004, sF2_means005, sF2_means006,
         sF2_means007, sF2_means008, sF2_means009,
         sF2_means010)

slz_sF2_trans  <-  melt(slz_sF2, id = c("idnum"))
slz_sF2_trans$measurement.no  <-  str_sub(slz_sF2_trans$variable,-2,-1)
slz_sF2_trans <-  slz_sF2_trans %>%
  rename(sF2 = value)%>%
  select(-variable)

#### sB1
slz_sB1 <-  slz %>%
  select(idnum, sB1_means001, sB1_means002, sB1_means003,
         sB1_means004, sB1_means005, sB1_means006,
         sB1_means007, sB1_means008, sB1_means009,
         sB1_means010)

slz_sB1_trans  <-  melt(slz_sB1, id = c("idnum"))
slz_sB1_trans$measurement.no  <-  str_sub(slz_sB1_trans$variable,-2,-1)
slz_sB1_trans <-  slz_sB1_trans %>%
  rename(sB1 = value)%>%
  select(-variable)

#### sB2
slz_sB2 <-  slz %>%
  select(idnum, sB2_means001, sB2_means002, sB2_means003,
         sB2_means004, sB2_means005, sB2_means006,
         sB2_means007, sB2_means008, sB2_means009,
         sB2_means010)

slz_sB2_trans  <-  melt(slz_sB2, id = c("idnum"))
slz_sB2_trans$measurement.no  <-  str_sub(slz_sB2_trans$variable,-2,-1)
slz_sB2_trans <-  slz_sB2_trans %>%
  rename(sB2 = value)%>%
  select(-variable)

#### soe
slz_soe <-  slz %>%
  select(idnum, soe_means001, soe_means002, soe_means003,
         soe_means004, soe_means005, soe_means006,
         soe_means007, soe_means008, soe_means009,
         soe_means010)

slz_soe_trans  <-  melt(slz_soe, id = c("idnum"))
slz_soe_trans$measurement.no  <-  str_sub(slz_soe_trans$variable,-2,-1)
slz_soe_trans <-  slz_soe_trans %>%
  rename(soe = value)%>%
  select(-variable)

### merging
slz_trans <-  list(slz_h1h2c_trans,
                  slz_h1c_trans,
                  slz_h2h4c_trans,
                  slz_h42Kc_trans,
                  slz_h2Kh5Kc_trans,
                  slz_h1a1c_trans,
                  slz_h1a2c_trans,
                  slz_h1a3c_trans,
                  slz_cpp_trans,
                  slz_hnr05_trans,
                  slz_hnr15_trans,
                  slz_hnr25_trans,
                  slz_hnr35_trans,
                  slz_strF0_trans,
                  slz_sF1_trans,
                  slz_sF2_trans,
                  slz_sB1_trans,
                  slz_sB2_trans,
                  slz_energy_trans,
                  slz_soe_trans) %>% reduce(merge, by = c("idnum","measurement.no"))

# Saving the file 
write.csv(slz_trans, file = "data/interim/slz_transformed.csv", row.names = F, fileEncoding = "UTF-8")

