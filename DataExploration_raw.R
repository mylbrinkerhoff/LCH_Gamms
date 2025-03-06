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

# Adding positions for data analysis. 
slz_trans <- slz_trans %>% 
  mutate(Position = case_when(time <= 2 ~ 1,
                              time <= 4 ~ 2,
                              time <= 6 ~ 3,
                              time <= 8 ~ 4,
                              TRUE ~ 5))

slz_trans$Position <- slz_trans$Position %>% factor()


# Generating boxplots 
h1h2_raw <- slz_trans %>% ggplot(aes(x = Position,
                                    y = h1h2c)) +
  scale_color_viridis(discrete = T) +
  labs(title = "raw H1*-H2* by position and phonation",
       x = "Vowel Position",
       y = "H1*-H2*") +
  geom_boxplot(aes(colour = Phonation), notch = T) +
  theme_bw()
h1h2_raw

h1a3_raw <- slz_trans %>% ggplot(aes(x = Position,
                                    y = h1a3c)) +
  scale_color_viridis(discrete = T) +
  labs(title = "raw H1*-A3 by position and phonation",
       x = "Vowel Position",
       y = "H1*-A3") +
  geom_boxplot(aes(colour = Phonation), notch = T) +
  theme_bw()
h1a3_raw

soe_raw <- slz_trans %>% ggplot(aes(x = Position,
                                     y = soe)) +
  scale_color_viridis(discrete = T) +
  labs(title = "raw SoE by position and phonation",
       x = "Vowel Position",
       y = "SoE") +
  geom_boxplot(aes(colour = Phonation), notch = T) +
  theme_bw()
soe_raw

cpp_raw <- slz_trans %>% ggplot(aes(x = Position,
                                     y = cpp)) +
  scale_color_viridis(discrete = T) +
  labs(title = "raw CPP by position and phonation",
       x = "Vowel Position",
       y = "CPP") +
  geom_boxplot(aes(colour = Phonation), notch = T) +
  theme_bw()
cpp_raw

f0_raw <- slz_trans %>% ggplot(aes(x = Position,
                                    y = strF0)) +
  scale_color_viridis(discrete = T) +
  labs(title = "raw f0 by position and phonation",
       x = "Vowel Position",
       y = "f0") +
  geom_boxplot(aes(colour = Phonation), notch = T) +
  theme_bw()
f0_raw


# Separating out the data by speakers

h1h2_raw.sp <- slz_trans %>% ggplot(aes(x = Position,
                                     y = h1h2c)) +
  scale_color_viridis(discrete = T) +
  labs(title = "raw H1*-H2* by position and phonation by Speaker",
       x = "Vowel Position",
       y = "H1*-H2*") +
  geom_boxplot(aes(colour = Phonation), notch = F) +
  facet_wrap(.~Speaker) +
  theme_bw()
h1h2_raw.sp

h1a3_raw.sp <- slz_trans %>% ggplot(aes(x = Position,
                                     y = h1a3c)) +
  scale_color_viridis(discrete = T) +
  labs(title = "raw H1*-A3 by position and phonation by Speaker",
       x = "Vowel Position",
       y = "H1*-A3") +
  geom_boxplot(aes(colour = Phonation), notch = F) +
  facet_wrap(.~Speaker) +
  theme_bw()
h1a3_raw.sp

soe_raw.sp <- slz_trans %>% ggplot(aes(x = Position,
                                    y = soe)) +
  scale_color_viridis(discrete = T) +
  labs(title = "raw SoE by position and phonation by Speaker",
       x = "Vowel Position",
       y = "SoE") +
  geom_boxplot(aes(colour = Phonation), notch = F) +
  facet_wrap(.~Speaker) +
  theme_bw()
soe_raw.sp

cpp_raw.sp <- slz_trans %>% ggplot(aes(x = Position,
                                    y = cpp)) +
  scale_color_viridis(discrete = T) +
  labs(title = "raw CPP by position and phonation by Speaker",
       x = "Vowel Position",
       y = "CPP") +
  geom_boxplot(aes(colour = Phonation), notch = F) +
  facet_wrap(.~Speaker) +
  theme_bw()
cpp_raw.sp

f0_raw.sp <- slz_trans %>% ggplot(aes(x = Position,
                                   y = strF0)) +
  scale_color_viridis(discrete = T) +
  labs(title = "raw f0 by position and phonation by Speaker",
       x = "Vowel Position",
       y = "f0") +
  geom_boxplot(aes(colour = Phonation), notch = F) +
  facet_wrap(.~Speaker) +
  theme_bw()
f0_raw.sp

# Loess smooths

h1h2.raw.line <- slz_trans %>% 
  ggplot(aes(x = time, 
             y=h1h2c, 
             group=Phonation, 
             colour=Phonation)) +
  geom_smooth(method = "loess", linewidth = 2) +
  scale_color_viridis(discrete=TRUE) +
  labs(title = "raw H1*-H2* measure across the vowel", 
       x = "Normalized time (% of vowel duration)",
       y = "H1*-H2* ") +
  theme_bw()
h1h2.raw.line

h1a3.raw.line <- slz_trans %>% 
  ggplot(aes(x = time, 
             y = h1a3c, 
             group=Phonation, 
             colour=Phonation)) +
  geom_smooth(method = "loess", linewidth = 2) +
  scale_color_viridis(discrete=TRUE) +
  labs(title = "raw H1*-A3 measure across the vowel", 
       x = "Normalized time (% of vowel duration)",
       y = "H1*-A3 (normalized)") +
  theme_bw()
h1a3.raw.line

cpp.raw.line <- slz_trans %>% 
  ggplot(aes(x = time, 
             y = cpp, 
             group=Phonation, 
             colour=Phonation)) +
  geom_smooth(method = "loess", linewidth = 2) +
  scale_color_viridis(discrete=TRUE) +
  labs(title = "raw CPP measure across the vowel", 
       x = "Normalized time (% of vowel duration)",
       y = "CPP") +
  theme_bw()
cpp.raw.line

soe.raw.line <- slz_trans %>% 
  ggplot(aes(x = time, 
             y = soe, 
             group=Phonation, 
             colour=Phonation)) +
  geom_smooth(method = "loess", linewidth = 2) +
  scale_color_viridis(discrete=TRUE) +
  labs(title = "raw SoE measure across the vowel", 
       x = "Normalized time (% of vowel duration)",
       y = "SoE") +
  theme_bw()
soe.raw.line

f0.raw.line <- slz_trans %>% 
  ggplot(aes(x = time, 
             y = strF0, 
             group=Phonation, 
             colour=Phonation)) +
  geom_smooth(method = "loess", linewidth = 2) +
  scale_color_viridis(discrete=TRUE) +
  labs(title = "raw f0 measure across the vowel", 
       x = "Normalized time (% of vowel duration)",
       y = "f0") +
  theme_bw()
f0.raw.line

# by speaker
h1h2.raw.line.sp <- slz_trans %>% 
  ggplot(aes(x = time, 
             y=h1h2c, 
             group=Phonation, 
             colour=Phonation)) +
  geom_smooth(method = "loess", linewidth = 2) +
  scale_color_viridis(discrete=TRUE) +
  labs(title = "raw H1*-H2* measure across the vowel by Speaker", 
       x = "Normalized time (% of vowel duration)",
       y = "H1*-H2* ") +
  facet_wrap(.~Speaker) +
  theme_bw()
h1h2.raw.line.sp

h1a3.raw.line.sp <- slz_trans %>% 
  ggplot(aes(x = time, 
             y = h1a3c, 
             group=Phonation, 
             colour=Phonation)) +
  geom_smooth(method = "loess", linewidth = 2) +
  scale_color_viridis(discrete=TRUE) +
  labs(title = "raw H1*-A3 measure across the vowel by Speaker", 
       x = "Normalized time (% of vowel duration)",
       y = "H1*-A3 (normalized)") +
  facet_wrap(.~Speaker) +
  theme_bw()
h1a3.raw.line.sp

cpp.raw.line.sp <- slz_trans %>% 
  ggplot(aes(x = time, 
             y = cpp, 
             group=Phonation, 
             colour=Phonation)) +
  geom_smooth(method = "loess", linewidth = 2) +
  scale_color_viridis(discrete=TRUE) +
  labs(title = "raw CPP measure across the vowel by Speaker", 
       x = "Normalized time (% of vowel duration)",
       y = "CPP") +
  facet_wrap(.~Speaker) +
  theme_bw()
cpp.raw.line.sp

soe.raw.line.sp <- slz_trans %>% 
  ggplot(aes(x = time, 
             y = soe, 
             group=Phonation, 
             colour=Phonation)) +
  geom_smooth(method = "loess", linewidth = 2) +
  scale_color_viridis(discrete=TRUE) +
  labs(title = "raw SoE measure across the vowel by Speaker", 
       x = "Normalized time (% of vowel duration)",
       y = "SoE") +
  facet_wrap(.~Speaker) +
  theme_bw()
soe.raw.line.sp

f0.raw.line.sp <- slz_trans %>% 
  ggplot(aes(x = time, 
             y = strF0, 
             group=Phonation, 
             colour=Phonation)) +
  geom_smooth(method = "loess", linewidth = 2) +
  scale_color_viridis(discrete=TRUE) +
  labs(title = "raw f0 measure across the vowel by Speaker", 
       x = "Normalized time (% of vowel duration)",
       y = "f0") +
  facet_wrap(.~Speaker) +
  theme_bw()
f0.raw.line.sp
