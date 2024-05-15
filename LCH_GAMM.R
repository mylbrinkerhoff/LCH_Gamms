####################################################################
###
###   LCH_GAMM.R
###   
###   M. Brinkerhoff  * UCSC  * 2024-05-15 (W)
###   
####################################################################

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
slz.clean <- read.csv("data/processed/slz_cleaned.csv", header = TRUE)

### convert certain columns into factors.
slz.clean$Phonation <- factor(slz.clean$Phonation, levels = c("modal", 
                                                              "breathy", 
                                                              "checked", 
                                                              "laryngealized"))
slz.clean$Speaker <- slz.clean$Speaker %>% factor()
slz.clean$Word <- slz.clean$Word %>% factor()
slz.clean$Vowel <- slz.clean$Vowel %>% factor()
slz.clean$Iter <- slz.clean$Iter %>% factor()
slz.clean$Tone <- slz.clean$Tone %>% factor()
slz.clean$Position <- slz.clean$Position %>% factor()