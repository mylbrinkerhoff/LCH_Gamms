####################################################################
###
###   Standardization.R
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
library(lme4)
source("R/gamm_hacks.R")

# Loading Data
slz_fil <- read.csv("data/interim/slz_filtered.csv", header = TRUE)


### Standardization of measurements for ease of comparison across different 
### measures

slz.s <- slz_fil %>% group_by(Speaker) %>%
  mutate(h1cz = (h1c - mean(h1c, na.rm = T))/sd(h1c, na.rm = T),
         h1h2cz = (h1h2c - mean(h1h2c, na.rm = T))/sd(h1h2c, na.rm = T),
         h2h4cz = (h2h4c - mean(h2h4c, na.rm = T))/sd(h2h4c, na.rm = T),
         h42Kcz = (h42Kc - mean(h42Kc, na.rm = T))/sd(h42Kc, na.rm = T),
         h2Kh5Kcz = (h2Kh5Kc - mean(h2Kh5Kc, na.rm = T))/sd(h2Kh5Kc, na.rm = T),
         h1a1cz = (h1a1c - mean(h1a1c, na.rm = T))/sd(h1a1c, na.rm = T),
         h1a2cz = (h1a2c - mean(h1a2c, na.rm = T))/sd(h1a2c, na.rm = T),
         h1a3cz = (h1a3c - mean(h1a3c, na.rm = T))/sd(h1a3c, na.rm = T),
         energyz = (energy - mean(energy, na.rm = T))/sd(energy, na.rm = T),
         hnr05z = (hnr05 - mean(hnr05, na.rm = T))/sd(hnr05, na.rm = T),
         hnr15z = (hnr15 - mean(hnr15, na.rm = T))/sd(hnr15, na.rm = T),
         hnr25z = (hnr25 - mean(hnr25, na.rm = T))/sd(hnr25, na.rm = T),
         hnr35z = (hnr35 - mean(hnr35, na.rm = T))/sd(hnr35, na.rm = T),
         cppz = (cpp - mean(cpp, na.rm = T))/sd(cpp, na.rm = T),
         f0z = (strF0 - mean(strF0, na.rm = T))/sd(strF0, na.rm = T),
         f1z = (sF1 - mean(sF1, na.rm = T))/sd(sF1, na.rm = T),
         f2z = (sF2 - mean(sF2, na.rm = T))/sd(sF2, na.rm = T),
         b1z = (sB1 - mean(sB1, na.rm = T))/sd(sB1, na.rm = T),
         b2z = (sB2 - mean(sB2, na.rm = T))/sd(sB2, na.rm = T)
  ) %>%
  mutate(log.soe = log10(soe+0.001),
         m.log.soe = mean(log.soe, na.rm=T),
         sd.log.soe = sd(log.soe, na.rm=T),
         z.log.soe = (log.soe-m.log.soe)/sd.log.soe,
         max.soe = max(log.soe),
         min.soe = min(log.soe),) %>%
  mutate(norm.soe = (log.soe-min.soe)/(max.soe-min.soe),) %>%
  select(-c(log.soe,
            m.log.soe,
            sd.log.soe,
            z.log.soe,
            max.soe,
            min.soe)) %>% 
  ungroup()

### Adding variable to sort the data into initial, middle, and end of the
### vowels
slz.s <- slz.s %>% 
  mutate(Position = case_when(measurement.no <= 2 ~ 1,
                              measurement.no <= 4 ~ 2,
                              measurement.no <= 6 ~ 3,
                              measurement.no <= 8 ~ 4,
                              TRUE ~ 5))
slz.s$Position <- slz.s$Position %>% factor()

### Calculating Residual h1
#### Generate the lmer model for residual h1
model.position.h1c.covariant <- lmer(h1cz ~ energyz + 
                                       (energyz||Speaker),
                                     data = slz.s,
                                     REML = FALSE)

#### extract the energy factor
energy.factor <- fixef(model.position.h1c.covariant)[2]

#### generate the residual H1 score
slz.s$H1c.resid = slz.s$h1cz - slz.s$energyz * energy.factor

write.csv(slz.s, file = "data/processed/slz_cleaned.csv", row.names = F, fileEncoding = "UTF-8")

