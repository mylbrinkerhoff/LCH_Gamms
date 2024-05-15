####################################################################
###
###   DataExploration_cleaned.R
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

# Generating boxplots 
h1h2_clean <- slz.clean %>% ggplot(aes(x = Position,
                                     y = h1h2cz)) +
  scale_color_viridis(discrete = T) +
  labs(title = "clean H1*-H2* by position and phonation",
       x = "Vowel Position",
       y = "H1*-H2* (normalized)") +
  geom_boxplot(aes(colour = Phonation), notch = T) +
  theme_bw()
h1h2_clean

h1a3_clean <- slz.clean %>% ggplot(aes(x = Position,
                                     y = h1a3cz)) +
  scale_color_viridis(discrete = T) +
  labs(title = "clean H1*-A3 by position and phonation",
       x = "Vowel Position",
       y = "H1*-A3 (normalized)") +
  geom_boxplot(aes(colour = Phonation), notch = T) +
  theme_bw()
h1a3_clean

soe_clean <- slz.clean %>% ggplot(aes(x = Position,
                                    y = norm.soe)) +
  scale_color_viridis(discrete = T) +
  labs(title = "clean SoE by position and phonation",
       x = "Vowel Position",
       y = "SoE (normalized)") +
  geom_boxplot(aes(colour = Phonation), notch = T) +
  theme_bw()
soe_clean

cpp_clean <- slz.clean %>% ggplot(aes(x = Position,
                                    y = cppz)) +
  scale_color_viridis(discrete = T) +
  labs(title = "clean CPP by position and phonation",
       x = "Vowel Position",
       y = "CPP (normalized)") +
  geom_boxplot(aes(colour = Phonation), notch = T) +
  theme_bw()
cpp_clean

f0_clean <- slz.clean %>% ggplot(aes(x = Position,
                                   y = strF0z)) +
  scale_color_viridis(discrete = T) +
  labs(title = "clean f0 by position and phonation",
       x = "Vowel Position",
       y = "f0 (normalized)") +
  geom_boxplot(aes(colour = Phonation), notch = T) +
  theme_bw()
f0_clean


# Separating out the data by speakers

h1h2_clean.sp <- slz.clean %>% ggplot(aes(x = Position,
                                        y = h1h2cz)) +
  scale_color_viridis(discrete = T) +
  labs(title = "clean H1*-H2* by position and phonation by Speaker",
       x = "Vowel Position",
       y = "H1*-H2* (normalized)") +
  geom_boxplot(aes(colour = Phonation), notch = F) +
  facet_wrap(.~Speaker) +
  theme_bw()
h1h2_clean.sp

h1a3_clean.sp <- slz.clean %>% ggplot(aes(x = Position,
                                        y = h1a3cz)) +
  scale_color_viridis(discrete = T) +
  labs(title = "clean H1*-A3 by position and phonation by Speaker",
       x = "Vowel Position",
       y = "H1*-A3 (normalized)") +
  geom_boxplot(aes(colour = Phonation), notch = F) +
  facet_wrap(.~Speaker) +
  theme_bw()
h1a3_clean.sp

soe_clean.sp <- slz.clean %>% ggplot(aes(x = Position,
                                       y = norm.soe)) +
  scale_color_viridis(discrete = T) +
  labs(title = "clean SoE by position and phonation by Speaker",
       x = "Vowel Position",
       y = "SoE (normalized)") +
  geom_boxplot(aes(colour = Phonation), notch = F) +
  facet_wrap(.~Speaker) +
  theme_bw()
soe_clean.sp

cpp_clean.sp <- slz.clean %>% ggplot(aes(x = Position,
                                       y = cppz)) +
  scale_color_viridis(discrete = T) +
  labs(title = "clean CPP by position and phonation by Speaker",
       x = "Vowel Position",
       y = "CPP (normalized)") +
  geom_boxplot(aes(colour = Phonation), notch = F) +
  facet_wrap(.~Speaker) +
  theme_bw()
cpp_clean.sp

f0_clean.sp <- slz.clean %>% ggplot(aes(x = Position,
                                      y = strF0z)) +
  scale_color_viridis(discrete = T) +
  labs(title = "clean f0 by position and phonation by Speaker",
       x = "Vowel Position",
       y = "f0 (normalized)") +
  geom_boxplot(aes(colour = Phonation), notch = F) +
  facet_wrap(.~Speaker) +
  theme_bw()
f0_clean.sp

# Loess smooths

h1h2.clean.line <- slz.clean %>% 
  ggplot(aes(x = time, 
             y=h1h2cz, 
             group=Phonation, 
             colour=Phonation)) +
  geom_smooth(method = "loess", linewidth = 2) +
  scale_color_viridis(discrete=TRUE) +
  labs(title = "clean H1*-H2* measure across the vowel", 
       x = "Normalized time (% of vowel duration)",
       y = "H1*-H2* (normalized)") +
  theme_bw()
h1h2.clean.line

h1a3.clean.line <- slz.clean %>% 
  ggplot(aes(x = time, 
             y = h1a3cz, 
             group=Phonation, 
             colour=Phonation)) +
  geom_smooth(method = "loess", linewidth = 2) +
  scale_color_viridis(discrete=TRUE) +
  labs(title = "clean H1*-A3 measure across the vowel", 
       x = "Normalized time (% of vowel duration)",
       y = "H1*-A3 (normalized)") +
  theme_bw()
h1a3.clean.line

cpp.clean.line <- slz.clean %>% 
  ggplot(aes(x = time, 
             y = cppz, 
             group=Phonation, 
             colour=Phonation)) +
  geom_smooth(method = "loess", linewidth = 2) +
  scale_color_viridis(discrete=TRUE) +
  labs(title = "clean CPP measure across the vowel", 
       x = "Normalized time (% of vowel duration)",
       y = "CPP (normalized)") +
  theme_bw()
cpp.clean.line

soe.clean.line <- slz.clean %>% 
  ggplot(aes(x = time, 
             y = norm.soe, 
             group=Phonation, 
             colour=Phonation)) +
  geom_smooth(method = "loess", linewidth = 2) +
  scale_color_viridis(discrete=TRUE) +
  labs(title = "clean SoE measure across the vowel", 
       x = "Normalized time (% of vowel duration)",
       y = "SoE (normalized)") +
  theme_bw()
soe.clean.line

f0.clean.line <- slz.clean %>% 
  ggplot(aes(x = time, 
             y = strF0z, 
             group=Phonation, 
             colour=Phonation)) +
  geom_smooth(method = "loess", linewidth = 2) +
  scale_color_viridis(discrete=TRUE) +
  labs(title = "clean f0 measure across the vowel", 
       x = "Normalized time (% of vowel duration)",
       y = "f0 (normalized)") +
  theme_bw()
f0.clean.line

# by speaker
h1h2.clean.line.sp <- slz.clean %>% 
  ggplot(aes(x = time, 
             y=h1h2cz, 
             group=Phonation, 
             colour=Phonation)) +
  geom_smooth(method = "loess", linewidth = 2) +
  scale_color_viridis(discrete=TRUE) +
  labs(title = "clean H1*-H2* measure across the vowel by Speaker", 
       x = "Normalized time (% of vowel duration)",
       y = "H1*-H2* (normalized)") +
  facet_wrap(.~Speaker) +
  theme_bw()
h1h2.clean.line.sp

h1a3.clean.line.sp <- slz.clean %>% 
  ggplot(aes(x = time, 
             y = h1a3cz, 
             group=Phonation, 
             colour=Phonation)) +
  geom_smooth(method = "loess", linewidth = 2) +
  scale_color_viridis(discrete=TRUE) +
  labs(title = "clean H1*-A3 measure across the vowel by Speaker", 
       x = "Normalized time (% of vowel duration)",
       y = "H1*-A3 (normalized)") +
  facet_wrap(.~Speaker) +
  theme_bw()
h1a3.clean.line.sp

cpp.clean.line.sp <- slz.clean %>% 
  ggplot(aes(x = time, 
             y = cppz, 
             group=Phonation, 
             colour=Phonation)) +
  geom_smooth(method = "loess", linewidth = 2) +
  scale_color_viridis(discrete=TRUE) +
  labs(title = "clean CPP measure across the vowel by Speaker", 
       x = "Normalized time (% of vowel duration)",
       y = "CPP (normalized)") +
  facet_wrap(.~Speaker) +
  theme_bw()
cpp.clean.line.sp

soe.clean.line.sp <- slz.clean %>% 
  ggplot(aes(x = time, 
             y = norm.soe, 
             group=Phonation, 
             colour=Phonation)) +
  geom_smooth(method = "loess", linewidth = 2) +
  scale_color_viridis(discrete=TRUE) +
  labs(title = "clean SoE measure across the vowel by Speaker", 
       x = "Normalized time (% of vowel duration)",
       y = "SoE (normalized)") +
  facet_wrap(.~Speaker) +
  theme_bw()
soe.clean.line.sp

f0.clean.line.sp <- slz.clean %>% 
  ggplot(aes(x = time, 
             y = strF0z, 
             group=Phonation, 
             colour=Phonation)) +
  geom_smooth(method = "loess", linewidth = 2) +
  scale_color_viridis(discrete=TRUE) +
  labs(title = "clean f0 measure across the vowel by Speaker", 
       x = "Normalized time (% of vowel duration)",
       y = "f0 (normalized)") +
  facet_wrap(.~Speaker) +
  theme_bw()
f0.clean.line.sp
