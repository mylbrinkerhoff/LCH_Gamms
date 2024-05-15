####################################################################
###
###   DataExploration_raw.R
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

### convert certain columns into factors.
slz_trans$Phonation <- factor(slz_trans$Phonation, levels = c("modal", 
                                                  "breathy", 
                                                  "checked", 
                                                  "laryngealized"))
slz_trans$Speaker <- slz_trans$Speaker %>% factor()
slz_trans$Word <- slz_trans$Word %>% factor()
slz_trans$Vowel <- slz_trans$Vowel %>% factor()
slz_trans$Iter <- slz_trans$Iter %>% factor()
slz_trans$Tone <- slz_trans$Tone %>% factor()

# Generating plots 
h1h2z_plot <- slz_trans %>% ggplot(aes(x = Position,
                                    y = h1h2cz)) +
  scale_color_viridis(discrete = T) +
  labs(title = "H1*-H2* by position and phonation",
       x = "Vowel Position",
       y = "H1*-H2* (normalized)") +
  geom_boxplot(aes(colour = Phonation)) +
  theme_bw()
h1h2z_plot

h1a3z_plot <- slz_trans %>% ggplot(aes(x = Position,
                                    y = h1a3cz)) +
  scale_color_viridis(discrete = T) +
  labs(title = "H1*-A3 by position and phonation",
       x = "Vowel Position",
       y = "H1*-A3 (normalized)") +
  geom_boxplot(aes(colour = Phonation), notch = T) +
  theme_bw()
h1a3z_plot

h1z_plot <- slz_trans %>% ggplot(aes(x = Position,
                                  y = H1c.resid)) +
  scale_color_viridis(discrete = T) +
  labs(title = "Residual H1* by position and phonation",
       x = "Vowel Position",
       y = "Residual H1* (normalized)") +
  geom_boxplot(aes(colour = Phonation), notch = T) +
  theme_bw()
h1z_plot
