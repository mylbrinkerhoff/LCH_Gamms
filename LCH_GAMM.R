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

# GAMM analysis
## f0 ~ phonation

f0.gam <- bam(strF0z ~ Phonation + 
                 s(time, by = Phonation, bs = "cr"),
               data = slz.clean, method = "ML")
summary(f0.gam)

slz.clean$phon.ord <- as.ordered(slz.clean$Phonation) 
contrasts(slz.clean$phon.ord) <- "contr.treatment" 
f0.gam.diff <- bam(strF0z ~ phon.ord + s(time, bs="cr") +
                           s(time, by=phon.ord, bs="cr"),
                         data=slz.clean, 
                         method="ML")
summary.coefs(f0.gam.diff)

f0.gam.diff.2 <- bam(strF0z ~ phon.ord +
                       s(time, bs = "cr") +
                       s(Tone, bs = "cr", k = 3) +
                       s(time, by = phon.ord, bs = "cr") +
                       s(Speaker, Word, Iter, bs = "re") +
                       s(Vowel, bs = "re"),
                     data=slz.clean, 
                     method="ML")


## CPP/HNR ~ phonation



## SoE ~ phonation


