####################################################################
###
###   OutlierRemoval.R
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
slz_trans <- read.csv("data/interim/slz_transformed.csv", header = TRUE)
summary(slz_trans)

### Remove outliers by F0, Formants, and Energy
#strF0 outlier flag
slz_fil = slz_trans %>%
  group_by(Speaker) %>%
  mutate(strF0z = (strF0 - mean(strF0, na.rm = T))/sd(strF0, na.rm = T)) %>%
  ungroup()

slz_fil = slz_fil %>%
  mutate(str_outlier = if_else(abs(strF0z) > 3, "outlier", "OK"))

### Formant outlier flagging
### Calculate Mahalanobis distance for formants
vmahalanobis = function (dat) {
  if (nrow(dat) < 25) {
    dat$zF1F2 = NA
    return(dat)
  }
  means = c(mean(dat$sF1, na.rm=T), mean(dat$sF2, na.rm=T))
  cov = cov(cbind(dat$sF1, dat$sF2))
  
  dat$zF1F2 = mahalanobis(cbind(dat$sF1, dat$sF2),
                          center=means, cov=cov)
  dat
}

# Distance larger than 6 is considered as outlier
distance_cutoff = 6

# Perform Mahalanobis on dataset
slz_fil =  slz_fil %>%
  group_by(Vowel) %>%
  do(vmahalanobis(.)) %>%
  ungroup() %>%
  mutate(formant_outlier = NA)

# Visualize the formants with flagged values
slz_fil %>% 
  filter(is.na(formant_outlier)) %>%
  ggplot(aes(x = sF2, y = sF1, color = zF1F2 > distance_cutoff)) +
  geom_point(size = 0.6) +
  facet_wrap(.~Vowel)+
  scale_y_reverse(limits = c(2000,0),position = "right") + 
  scale_x_reverse(limits = c(3500,0),position = "top")+
  theme_bw()

for (i in 1:nrow(slz_fil)) {
  if (!is.na(slz_fil$zF1F2[i])) {
    if (slz_fil$zF1F2[i] > distance_cutoff){
      slz_fil$formant_outlier[i] = "outlier"
    }
  }
}

# Visualize the vowel formant after exclusion
slz_fil %>% 
  filter(is.na(formant_outlier)) %>%
  ggplot(aes(x = sF2, y = sF1)) +
  geom_point(size = 0.6) +
  #geom_text()+
  facet_wrap(.~Vowel)+
  #geom_density_2d() +
  #  scale_color_manual(values=c('#a6611a','#slzc27d','#018571'))+
  scale_y_reverse(limits = c(2000,0),position = "right") + 
  scale_x_reverse(limits = c(3500,0),position = "top")+
  theme_bw()

slz_fil <- slz_fil %>%
  filter(str_outlier == "OK")

slz_fil <- slz_fil %>% 
  filter(is.na(formant_outlier))

# removing energy outliers
slz_fil$energy[slz_fil$energy == 0] <- NA

slz_fil <- slz_fil %>%
  mutate(energy = log10(energy))

slz_fil <- slz_fil %>% filter(!is.na(energy))

write.csv(slz_fil, file = "data/interim/slz_filtered.csv", row.names = F, fileEncoding = "UTF-8")

