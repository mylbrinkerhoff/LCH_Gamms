####################################################################
###
###   LCH_GAMM.R
###   
###   M. Brinkerhoff  * UCSC  * 2024-05-15 (W)
###   
####################################################################

# Libraries that are required
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
slz.clean$Tone <- slz.clean$Tone %>% factor()


summary(slz.clean)
str(slz.clean)


# GAMM analysis
## CPP/HNR ~ phonation
## Plot showing the data
ggplot(slz.clean, aes(x=measurement.no, y=hnr15z, 
                      group = idnum, colour = Speaker)) +
  facet_wrap(~Word) + geom_line()

# Linear model
hnr.gam.linear <- bam(hnr15 ~ Phonation, 
                      data = slz.clean, method = "ML")
summary(hnr.gam.linear)

# Extend the linear model over time
hnr.gam <- bam(hnr15 ~ Phonation +
                 s(measurement.no, by = Phonation, bs = "cr"),
               data = slz.clean, method = "ML")
summary(hnr.gam)



# Separating out the intercept difference and non-linear difference
slz.clean$phon.ord <- as.ordered(slz.clean$Phonation)
contrasts(slz.clean$phon.ord) <- "contr.treatment"

hnr.gam.diff <- bam(hnr15 ~ phon.ord +
                     s(measurement.no) +
                     s(measurement.no, by = phon.ord),
                   data = slz.clean, method = "ML")
summary(hnr.gam.diff)

plot_smooth(hnr.gam.diff, view="measurement.no",
            plot_all="phon.ord", rug=F)

plot_diff(hnr.gam.diff, view="measurement.no", 
          comp=list(phon.ord=c("checked","modal")))

plot_diff(hnr.gam.diff, view="measurement.no", 
          comp=list(phon.ord=c("breathy","modal")))

plot_diff(hnr.gam.diff, view="measurement.no", 
          comp=list(phon.ord=c("laryngealized","modal")))
