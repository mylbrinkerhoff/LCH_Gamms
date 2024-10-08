####################################################################
###
###   DataExploration_cleaned.R
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

hnr5_clean <- slz.clean %>% ggplot(aes(x = Position,
                                      y = hnr05z)) +
  scale_color_viridis(discrete = T) +
  labs(title = "clean HNR 500Hz by position and phonation",
       x = "Vowel Position",
       y = "HNR 500Hz (normalized)") +
  geom_boxplot(aes(colour = Phonation), notch = T) +
  theme_bw()
hnr5_clean

hnr15_clean <- slz.clean %>% ggplot(aes(x = Position,
                                       y = hnr15z)) +
  scale_color_viridis(discrete = T) +
  labs(title = "clean HNR 1500Hz by position and phonation",
       x = "Vowel Position",
       y = "HNR 1500Hz (normalized)") +
  geom_boxplot(aes(colour = Phonation), notch = T) +
  theme_bw()
hnr15_clean

hnr25_clean <- slz.clean %>% ggplot(aes(x = Position,
                                       y = hnr25z)) +
  scale_color_viridis(discrete = T) +
  labs(title = "clean HNR 2500Hz by position and phonation",
       x = "Vowel Position",
       y = "HNR 2500Hz (normalized)") +
  geom_boxplot(aes(colour = Phonation), notch = T) +
  theme_bw()
hnr25_clean

hnr35_clean <- slz.clean %>% ggplot(aes(x = Position,
                                       y = hnr35z)) +
  scale_color_viridis(discrete = T) +
  labs(title = "clean HNR 3500Hz by position and phonation",
       x = "Vowel Position",
       y = "HNR 3500Hz (normalized)") +
  geom_boxplot(aes(colour = Phonation), notch = T) +
  theme_bw()
hnr35_clean

h1_clean <- slz.clean %>% ggplot(aes(x = Position,
                                     y = H1c.resid)) +
  scale_color_viridis(discrete = T) +
  labs(title = "clean resid. H1* by position and phonation",
       x = "Vowel Position",
       y = "resid. H1 (normalized)") +
  geom_boxplot(aes(colour = Phonation), notch = T) +
  theme_bw()
h1_clean

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
  facet_wrap(.~Speaker, nrow = 2, ncol = 5) +
  theme_bw()
h1h2_clean.sp

h1a3_clean.sp <- slz.clean %>% ggplot(aes(x = Position,
                                        y = h1a3cz)) +
  scale_color_viridis(discrete = T) +
  labs(title = "clean H1*-A3 by position and phonation by Speaker",
       x = "Vowel Position",
       y = "H1*-A3 (normalized)") +
  geom_boxplot(aes(colour = Phonation), notch = F) +
  facet_wrap(.~Speaker, nrow = 2, ncol = 5) +
  theme_bw()
h1a3_clean.sp

soe_clean.sp <- slz.clean %>% ggplot(aes(x = Position,
                                       y = norm.soe)) +
  scale_color_viridis(discrete = T) +
  labs(title = "clean SoE by position and phonation by Speaker",
       x = "Vowel Position",
       y = "SoE (normalized)") +
  geom_boxplot(aes(colour = Phonation), notch = F) +
  facet_wrap(.~Speaker, nrow = 2, ncol = 5) +
  theme_bw()
soe_clean.sp

cpp_clean.sp <- slz.clean %>% ggplot(aes(x = Position,
                                       y = cppz)) +
  scale_color_viridis(discrete = T) +
  labs(title = "clean CPP by position and phonation by Speaker",
       x = "Vowel Position",
       y = "CPP (normalized)") +
  geom_boxplot(aes(colour = Phonation), notch = F) +
  facet_wrap(.~Speaker, nrow = 2, ncol = 5) +
  theme_bw()
cpp_clean.sp

h1_clean.sp <- slz.clean %>% ggplot(aes(x = Position,
                                     y = H1c.resid)) +
  scale_color_viridis(discrete = T) +
  labs(title = "clean resid. H1* by position and phonation",
       x = "Vowel Position",
       y = "resid. H1 (normalized)") +
  geom_boxplot(aes(colour = Phonation), notch = F) +
  facet_wrap(.~Speaker, nrow = 2, ncol = 5) +
  theme_bw()
h1_clean.sp

f0_clean.sp <- slz.clean %>% ggplot(aes(x = Position,
                                      y = strF0z)) +
  scale_color_viridis(discrete = T) +
  labs(title = "clean f0 by position and phonation by Speaker",
       x = "Vowel Position",
       y = "f0 (normalized)") +
  geom_boxplot(aes(colour = Phonation), notch = F) +
  facet_wrap(.~Speaker, nrow = 2, ncol = 5) +
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

hnr5.clean.line <- slz.clean %>% 
  ggplot(aes(x = time, 
             y = hnr05z, 
             group=Phonation, 
             colour=Phonation)) +
  geom_smooth(method = "loess", linewidth = 2) +
  scale_color_viridis(discrete=TRUE) +
  labs(title = "clean HNR 500Hz measure across the vowel", 
       x = "Normalized time (% of vowel duration)",
       y = "HNR 500Hz (normalized)") +
  theme_bw()
hnr5.clean.line

hnr15.clean.line <- slz.clean %>% 
  ggplot(aes(x = time, 
             y = hnr15z, 
             group=Phonation, 
             colour=Phonation)) +
  geom_smooth(method = "loess", linewidth = 2) +
  scale_color_viridis(discrete=TRUE) +
  labs(title = "clean HNR 1500Hz measure across the vowel", 
       x = "Normalized time (% of vowel duration)",
       y = "HNR 1500Hz (normalized)") +
  theme_bw()
hnr15.clean.line

hnr25.clean.line <- slz.clean %>% 
  ggplot(aes(x = time, 
             y = hnr25z, 
             group=Phonation, 
             colour=Phonation)) +
  geom_smooth(method = "loess", linewidth = 2) +
  scale_color_viridis(discrete=TRUE) +
  labs(title = "clean HNR 2500Hz measure across the vowel", 
       x = "Normalized time (% of vowel duration)",
       y = "HNR 2500Hz (normalized)") +
  theme_bw()
hnr25.clean.line

hnr35.clean.line <- slz.clean %>% 
  ggplot(aes(x = time, 
             y = hnr35z, 
             group=Phonation, 
             colour=Phonation)) +
  geom_smooth(method = "loess", linewidth = 2) +
  scale_color_viridis(discrete=TRUE) +
  labs(title = "clean HNR 3500Hz measure across the vowel", 
       x = "Normalized time (% of vowel duration)",
       y = "HNR 3500Hz (normalized)") +
  theme_bw()
hnr35.clean.line

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

h1.clean.line <- slz.clean %>% 
  ggplot(aes(x = time, 
             y = H1c.resid, 
             group=Phonation, 
             colour=Phonation)) +
  geom_smooth(method = "loess", linewidth = 2) +
  scale_color_viridis(discrete=TRUE) +
  labs(title = "clean residual H1* measure across the vowel", 
       x = "Normalized time (% of vowel duration)",
       y = "resid. H1* (normalized)") +
  theme_bw()
h1.clean.line

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
  facet_wrap(.~Speaker, nrow = 2, ncol = 5) +
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
  facet_wrap(.~Speaker, nrow = 2, ncol = 5) +
  theme_bw()
h1a3.clean.line.sp

cpp.clean.line.sp <- slz.clean %>% 
  ggplot(aes(x = time, 
             y = cppz, 
             group=Phonation, 
             colour=Phonation)) +
  geom_smooth(method = "loess", linewidth = 1) +
  scale_color_viridis(discrete=TRUE) +
  labs(title = "clean CPP measure across the vowel by Speaker", 
       x = "Normalized time (% of vowel duration)",
       y = "CPP (normalized)") +
  facet_wrap(.~Speaker, nrow = 2, ncol = 5) +
  theme_bw()
cpp.clean.line.sp

hnr5.clean.line.sp <- slz.clean %>% 
  ggplot(aes(x = time, 
             y = hnr05z, 
             group=Phonation, 
             colour=Phonation)) +
  geom_smooth(method = "loess", linewidth = 2) +
  scale_color_viridis(discrete=TRUE) +
  labs(title = "clean HNR 500Hz measure across the vowel by Speaker", 
       x = "Normalized time (% of vowel duration)",
       y = "HNR 500Hz (normalized)") +
  facet_wrap(.~Speaker, nrow = 2, ncol = 5) +
  theme_bw()
hnr5.clean.line.sp

hnr15.clean.line.sp <- slz.clean %>% 
  ggplot(aes(x = time, 
             y = hnr15z, 
             group=Phonation, 
             colour=Phonation)) +
  geom_smooth(method = "loess", linewidth = 2) +
  scale_color_viridis(discrete=TRUE) +
  labs(title = "clean HNR 1500Hz measure across the vowel by Speaker", 
       x = "Normalized time (% of vowel duration)",
       y = "HNR 1500Hz (normalized)") +
  facet_wrap(.~Speaker, nrow = 2, ncol = 5) +
  theme_bw()
hnr15.clean.line.sp

hnr25.clean.line.sp <- slz.clean %>% 
  ggplot(aes(x = time, 
             y = hnr25z, 
             group=Phonation, 
             colour=Phonation)) +
  geom_smooth(method = "loess", linewidth = 2) +
  scale_color_viridis(discrete=TRUE) +
  labs(title = "clean HNR 2500Hz measure across the vowel by Speaker", 
       x = "Normalized time (% of vowel duration)",
       y = "HNR 2500Hz (normalized)") +
  facet_wrap(.~Speaker, nrow = 2, ncol = 5) +
  theme_bw()
hnr25.clean.line.sp

hnr35.clean.line.sp <- slz.clean %>% 
  ggplot(aes(x = time, 
             y = hnr35z, 
             group=Phonation, 
             colour=Phonation)) +
  geom_smooth(method = "loess", linewidth = 2) +
  scale_color_viridis(discrete=TRUE) +
  labs(title = "clean HNR 3500Hz measure across the vowel by Speaker", 
       x = "Normalized time (% of vowel duration)",
       y = "HNR 3500Hz (normalized)") +
  facet_wrap(.~Speaker, nrow = 2, ncol = 5) +
  theme_bw()
hnr35.clean.line.sp

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
  facet_wrap(.~Speaker, nrow = 2, ncol = 5) +
  theme_bw()
soe.clean.line.sp

h1.clean.line.sp <- slz.clean %>% 
  ggplot(aes(x = time, 
             y = H1c.resid, 
             group=Phonation, 
             colour=Phonation)) +
  geom_smooth(method = "loess", linewidth = 2) +
  scale_color_viridis(discrete=TRUE) +
  labs(title = "clean resid. H1* measure across the vowel by Speaker", 
       x = "Normalized time (% of vowel duration)",
       y = "resid. H1 (normalized)") +
  facet_wrap(.~Speaker, nrow = 2, ncol = 5) +
  theme_bw()
h1.clean.line.sp

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
  facet_wrap(.~Speaker, nrow = 2, ncol = 5) +
  theme_bw()
f0.clean.line.sp

# Saving the plots

ggsave(filename = "reports/figs/cpp_clean_line.png",
       plot = cpp.clean.line,
       width = 5,
       height = 3,
       dpi = 300,
       units = "in")

ggsave(filename = "reports/figs/hnr5_clean_line.png",
       plot = hnr5.clean.line,
       width = 5,
       height = 3,
       dpi = 300,
       units = "in")

ggsave(filename = "reports/figs/hnr15_clean_line.png",
       plot = hnr15.clean.line,
       width = 5,
       height = 3,
       dpi = 300,
       units = "in")

ggsave(filename = "reports/figs/soe_clean_line.png",
       plot = soe.clean.line,
       width = 5,
       height = 3,
       dpi = 300,
       units = "in")

ggsave(filename = "reports/figs/f0_clean_line.png",
       plot = f0.clean.line,
       width = 5,
       height = 3,
       dpi = 300,
       units = "in")

ggsave(filename = "reports/figs/h1h2_clean_line.png",
       plot = h1h2.clean.line,
       width = 5,
       height = 3,
       dpi = 300,
       units = "in")

ggsave(filename = "reports/figs/h1_clean_line.png",
       plot = h1.clean.line,
       width = 5,
       height = 3,
       dpi = 300,
       units = "in")

ggsave(filename = "reports/figs/h1a3_clean_line.png",
       plot = h1a3.clean.line,
       width = 5,
       height = 3,
       dpi = 300,
       units = "in")

# by speaker plots 
ggsave(filename = "reports/figs/cpp_line_speaker.png",
       plot = cpp.clean.line.sp,
       width = 5,
       height = 3,
       dpi = 300,
       units = "in")

ggsave(filename = "reports/figs/hnr5_line_speaker.png",
       plot = hnr5.clean.line.sp,
       width = 5,
       height = 3,
       dpi = 300,
       units = "in")

ggsave(filename = "reports/figs/hnr15_line_speaker.png",
       plot = hnr15.clean.line.sp,
       width = 5,
       height = 3,
       dpi = 300,
       units = "in")

ggsave(filename = "reports/figs/soe_line_speaker.png",
       plot = soe.clean.line.sp,
       width = 5,
       height = 3,
       dpi = 300,
       units = "in")

ggsave(filename = "reports/figs/f0_line_speaker.png",
       plot = f0.clean.line.sp,
       width = 5,
       height = 3,
       dpi = 300,
       units = "in")

ggsave(filename = "reports/figs/h1_line_speaker.png",
       plot = h1.clean.line.sp,
       width = 5,
       height = 3,
       dpi = 300,
       units = "in")

ggsave(filename = "reports/figs/h1a3_line_speaker.png",
       plot = h1a3.clean.line.sp,
       width = 5,
       height = 3,
       dpi = 300,
       units = "in")

ggsave(filename = "reports/figs/h1h2_line_speaker.png",
       plot = h1h2.clean.line.sp,
       width = 5,
       height = 3,
       dpi = 300,
       units = "in")
