####################################################################
###
###   PlottingExamples.R
###   
###   M. Brinkerhoff  * UCSC  * 2024-08-11 (F)
###   
####################################################################

# Libraries that are required
library(mgcv)
library(itsadug)
library(tidyverse)
library(viridis)
library(reshape2)
library(readr)
library(lme4)
library(knitr)
library(plotrix)

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

# Create a variable for colorblind palette

colorblind <- palette.colors(palette = "Okabe-Ito")

# Create a new dataframe that contains the average and standard errors for the 
# measures

df.plot <- slz.clean%>%
  group_by(measurement.no, Phonation) %>%
  summarize(h1h2.mean = mean(h1h2c),
            h1h2.se   = std.error(h1h2c),
            h1h2.pse = h1h2.mean + 1.96 * h1h2.se,
            h1h2.mse = h1h2.mean - 1.96 * h1h2.se,
            h1c.mean = mean(h1c),
            h1c.se   = std.error(h1c),
            h1c.pse = h1c.mean + 1.96 * h1c.se,
            h1c.mse = h1c.mean - 1.96 * h1c.se,
            h2h4.mean = mean(h2h4c),
            h2h4.se   = std.error(h2h4c),
            h2h4.pse = h2h4.mean + 1.96 * h2h4.se,
            h2h4.mse = h2h4.mean - 1.96 * h2h4.se,
            h42K.mean = mean(h42Kc),
            h42K.se   = std.error(h42Kc),
            h42K.pse = h42K.mean + 1.96 * h42K.se,
            h42K.mse = h42K.mean - 1.96 * h42K.se,
            h2Kh5K.mean = mean(h2Kh5Kc),
            h2Kh5K.se   = std.error(h2Kh5Kc),
            h2Kh5K.pse = h2Kh5K.mean + 1.96 * h2Kh5K.se,
            h2Kh5K.mse = h2Kh5K.mean - 1.96 * h2Kh5K.se,
            h1a1.mean = mean(h1a1c),
            h1a1.se   = std.error(h1a1c),
            h1a1.pse = h1a1.mean + 1.96 * h1a1.se,
            h1a1.mse = h1a1.mean - 1.96 * h1a1.se,
            h1a2.mean = mean(h1a2c),
            h1a2.se   = std.error(h1a2c),
            h1a2.pse = h1a2.mean + 1.96 * h1a2.se,
            h1a2.mse = h1a2.mean - 1.96 * h1a2.se,
            h1a3.mean = mean(h1a3c),
            h1a3.se   = std.error(h1a3c),
            h1a3.pse = h1a3.mean + 1.96 * h1a3.se,
            h1a3.mse = h1a3.mean - 1.96 * h1a3.se,
            cpp.mean = mean(cpp),
            cpp.se   = std.error(cpp),
            cpp.pse = cpp.mean + 1.96 * cpp.se,
            cpp.mse = cpp.mean - 1.96 * cpp.se,
            hnr05.mean = mean(hnr05),
            hnr05.se   = std.error(hnr05),
            hnr05.pse = hnr05.mean + 1.96 * hnr05.se,
            hnr05.mse = hnr05.mean - 1.96 * hnr05.se,
            hnr15.mean = mean(hnr15),
            hnr15.se   = std.error(hnr15),
            hnr15.pse = hnr15.mean + 1.96 * hnr15.se,
            hnr15.mse = hnr15.mean - 1.96 * hnr15.se,
            hnr25.mean = mean(hnr25),
            hnr25.se   = std.error(hnr25),
            hnr25.pse = hnr25.mean + 1.96 * hnr25.se,
            hnr25.mse = hnr25.mean - 1.96 * hnr25.se,
            hnr35.mean = mean(hnr35),
            hnr35.se   = std.error(hnr35),
            hnr35.pse = hnr35.mean + 1.96 * hnr35.se,
            hnr35.mse = hnr35.mean - 1.96 * hnr35.se,
            strF0.mean = mean(strF0),
            strF0.se   = std.error(strF0),
            strF0.pse = strF0.mean + 1.96 * strF0.se,
            strF0.mse = strF0.mean - 1.96 * strF0.se,
            sF1.mean = mean(sF1),
            sF1.se   = std.error(sF1),
            sF1.pse = sF1.mean + 1.96 * sF1.se,
            sF1.mse = sF1.mean - 1.96 * sF1.se,
            sF2.mean = mean(sF2),
            sF2.se   = std.error(sF2),
            sF2.pse = sF2.mean + 1.96 * sF2.se,
            sF2.mse = sF2.mean - 1.96 * sF2.se,
            sB1.mean = mean(sB1),
            sB1.se   = std.error(sB1),
            sB1.pse = sB1.mean + 1.96 * sB1.se,
            sB1.mse = sB1.mean - 1.96 * sB1.se,
            sB2.mean = mean(sB2),
            sB2.se   = std.error(sB2),
            sB2.pse = sB2.mean + 1.96 * sB2.se,
            sB2.mse = sB2.mean - 1.96 * sB2.se,
            energy.mean = mean(energy),
            energy.se   = std.error(energy),
            energy.pse = energy.mean + 1.96 * energy.se,
            energy.mse = energy.mean - 1.96 * energy.se,
            norm.soe.mean = mean(norm.soe),
            norm.soe.se   = std.error(norm.soe),
            norm.soe.pse = norm.soe.mean + 1.96 * norm.soe.se,
            norm.soe.mse = norm.soe.mean - 1.96 * norm.soe.se,
            H1c.resid.mean = mean(H1c.resid),
            H1c.resid.se   = std.error(H1c.resid),
            H1c.resid.pse = H1c.resid.mean + 1.96 * H1c.resid.se,
            H1c.resid.mse = H1c.resid.mean - 1.96 * H1c.resid.se,
            a1c.mean = mean(a1c),
            a1c.se   = std.error(a1c),
            a1c.pse = a1c.mean + 1.96 * a1c.se,
          a1c.mse = a1c.mean - 1.96 * a1c.se,
          a2c.mean = mean(a2c),
          a2c.se   = std.error(a2c),
          a2c.pse = a2c.mean + 1.96 * a2c.se,
          a2c.mse = a2c.mean - 1.96 * a2c.se,
          ) %>%
  ungroup() %>%
  mutate(Phonation = recode(Phonation, "laryngealized" = "rearticulated"))

write.csv(df.plot, file = "data/processed/plotting.csv", row.names = F, fileEncoding = "UTF-8")


# This first approach gathers mean, mean minus 1.96 Std. Error, and mean plus
# 1.96 Std. Error into the same columns, with "stat" marking which stat it is,
# and yvar storing the value. (I picked those because +/- 1.96 Std. Error
# captures ~95% of a normal distribution.) Then we can plot them together in a
# single call to geom_line.

# This was a test with a different df to see if I could get it to work 
# p <- ggplot(sample_sum,
#             aes(x = measurement.no, y = mean)) + 
#   geom_point(aes(shape = Phonation), size = 7) +
#   scale_shape_manual(values = c(0, 1, 2, 5)) +
#   # geom_smooth(aes(group = Phonation, colour = Phonation), stat = "identity") +
#   geom_errorbar(aes(ymin = mean_mse, ymax = mean_pse, colour = Phonation), width = 0.2)+
#   geom_line(aes(colour = Phonation), linewidth = 1.5) +
#   scale_colour_manual(values = colorblind) +
#   scale_x_continuous(n.breaks = 10) +
#   labs(title = "clean H1*-H2* by position and phonation",
#        x = "Vowel Position",
#        y = "H1*-H2* (normalized)") +
#   theme_bw()
# p

# Plotting point and line graphs

h1h2.line <- ggplot(df.plot,
            aes(x = measurement.no, y = h1h2.mean)) + 
  geom_point(aes(shape = Phonation), size = 3) +
  scale_shape_manual(values = c(0, 1, 2, 5)) +
  # geom_smooth(aes(group = Phonation, colour = Phonation), stat = "identity") +
  geom_errorbar(aes(ymin = h1h2.mse, ymax = h1h2.pse, colour = Phonation), width = 0.2)+
  geom_line(aes(colour = Phonation), linewidth = 1) +
  scale_colour_manual(values = colorblind) +
  scale_x_continuous(n.breaks = 10) +
  labs(title = "H1*-H2*",
       x = "Measurement Number",
       y = "H1*-H2* (dB)") +
  theme_bw()
h1h2.line

h1h2.line.sp <- ggplot(df.plot,
            aes(x = measurement.no, y = h1h2.mean)) + 
  geom_point(aes(shape = Phonation), size = 3) +
  scale_shape_manual(values = c(0, 1, 2, 5)) +
  # geom_smooth(aes(group = Phonation, colour = Phonation), stat = "identity") +
  geom_errorbar(aes(ymin = h1h2.mse, ymax = h1h2.pse, colour = Phonation), width = 0.2)+
  geom_line(aes(colour = Phonation), linewidth = 1) +
  scale_colour_manual(values = colorblind) +
  scale_x_continuous(n.breaks = 10) +
  facet_wrap(.~Speaker, nrow = 2, ncol = 5) +
  labs(title = "H1*-H2* by speaker",
       x = "Measurement Number",
       y = "H1*-H2* (dB)") +
  theme_bw()
h1h2.line.sp

h1c.line <- ggplot(df.plot,
                    aes(x = measurement.no, y = h1c.mean)) + 
  geom_point(aes(shape = Phonation), size = 7) +
  scale_shape_manual(values = c(0, 1, 2, 5)) +
  # geom_smooth(aes(group = Phonation, colour = Phonation), stat = "identity") +
  geom_errorbar(aes(ymin = h1c.mse, ymax = h1c.pse, colour = Phonation), width = 0.2)+
  geom_line(aes(colour = Phonation), linewidth = 1.5) +
  scale_colour_manual(values = colorblind) +
  scale_x_continuous(n.breaks = 10) +
  labs(title = "H1*",
       x = "Measurement Number",
       y = "H1* (dB)") +
  theme_bw()
h1c.line

h1c.line.sp <- ggplot(df.plot,
                    aes(x = measurement.no, y = h1c.mean)) + 
  geom_point(aes(shape = Phonation), size = 7) +
  scale_shape_manual(values = c(0, 1, 2, 5)) +
  # geom_smooth(aes(group = Phonation, colour = Phonation), stat = "identity") +
  geom_errorbar(aes(ymin = h1c.mse, ymax = h1c.pse, colour = Phonation), width = 0.2)+
  geom_line(aes(colour = Phonation), linewidth = 1.5) +
  scale_colour_manual(values = colorblind) +
  scale_x_continuous(n.breaks = 10) +
  facet_wrap(.~Speaker, nrow = 2, ncol = 5) +
  labs(title = "H1*",
       x = "Measurement Number",
       y = "H1* (dB)") +
  theme_bw()
h1c.line.sp


h2h4.line <- ggplot(df.plot,
                    aes(x = measurement.no, y = h2h4.mean)) + 
  geom_point(aes(shape = Phonation), size = 7) +
  scale_shape_manual(values = c(0, 1, 2, 5)) +
  # geom_smooth(aes(group = Phonation, colour = Phonation), stat = "identity") +
  geom_errorbar(aes(ymin = h2h4.mse, ymax = h2h4.pse, colour = Phonation), width = 0.2)+
  geom_line(aes(colour = Phonation), linewidth = 1.5) +
  scale_colour_manual(values = colorblind) +
  scale_x_continuous(n.breaks = 10) +
  labs(title = "H2*-H4*",
       x = "Measurement Number",
       y = "H2*-H4* (dB)") +
  theme_bw()
h2h4.line

h42K.line <- ggplot(df.plot,
                    aes(x = measurement.no, y = h42K.mean)) + 
  geom_point(aes(shape = Phonation), size = 7) +
  scale_shape_manual(values = c(0, 1, 2, 5)) +
  # geom_smooth(aes(group = Phonation, colour = Phonation), stat = "identity") +
  geom_errorbar(aes(ymin = h42K.mse, ymax = h42K.pse, colour = Phonation), width = 0.2)+
  geom_line(aes(colour = Phonation), linewidth = 1.5) +
  scale_colour_manual(values = colorblind) +
  scale_x_continuous(n.breaks = 10) +
  labs(title = "H4*-H 2000 Hz*",
       x = "Measurement Number",
       y = "H4*-H 2000 Hz * (dB)") +
  theme_bw()
h42K.line

h2Kh5K.line <- ggplot(df.plot,
                    aes(x = measurement.no, y = h2Kh5K.mean)) + 
  geom_point(aes(shape = Phonation), size = 7) +
  scale_shape_manual(values = c(0, 1, 2, 5)) +
  # geom_smooth(aes(group = Phonation, colour = Phonation), stat = "identity") +
  geom_errorbar(aes(ymin = h2Kh5K.mse, ymax = h2Kh5K.pse, colour = Phonation), width = 0.2)+
  geom_line(aes(colour = Phonation), linewidth = 1.5) +
  scale_colour_manual(values = colorblind) +
  scale_x_continuous(n.breaks = 10) +
  labs(title = "H 2000 Hz* - H 5000 Hz*",
       x = "Measurement Number",
       y = "H 2000 Hz*-H 5000 Hz* (dB)") +
  theme_bw()
h2Kh5K.line

h1a1.line <- ggplot(df.plot,
                    aes(x = measurement.no, y = h1a1.mean)) + 
  geom_point(aes(shape = Phonation), size = 7) +
  scale_shape_manual(values = c(0, 1, 2, 5)) +
  # geom_smooth(aes(group = Phonation, colour = Phonation), stat = "identity") +
  geom_errorbar(aes(ymin = h1a1.mse, ymax = h1a1.pse, colour = Phonation), width = 0.2)+
  geom_line(aes(colour = Phonation), linewidth = 1.5) +
  scale_colour_manual(values = colorblind) +
  scale_x_continuous(n.breaks = 10) +
  labs(title = "H1*-A1*",
       x = "Measurement Number",
       y = "H1*-A1* (dB)") +
  theme_bw()
h1a1.line

h1a2.line <- ggplot(df.plot,
                    aes(x = measurement.no, y = h1a2.mean)) + 
  geom_point(aes(shape = Phonation), size = 3) +
  scale_shape_manual(values = c(0, 1, 2, 5)) +
  # geom_smooth(aes(group = Phonation, colour = Phonation), stat = "identity") +
  geom_errorbar(aes(ymin = h1a2.mse, ymax = h1a2.pse, colour = Phonation), width = 0.2)+
  geom_line(aes(colour = Phonation), linewidth = 1) +
  scale_colour_manual(values = colorblind) +
  scale_x_continuous(n.breaks = 10) +
  labs(title = "H1*-A2*",
       x = "Measurement Number",
       y = "H1*-A2* (dB)") +
  theme_bw()
h1a2.line

h1a3.line <- ggplot(df.plot,
                    aes(x = measurement.no, y = h1a3.mean)) + 
  geom_point(aes(shape = Phonation), size = 7) +
  scale_shape_manual(values = c(0, 1, 2, 5)) +
  # geom_smooth(aes(group = Phonation, colour = Phonation), stat = "identity") +
  geom_errorbar(aes(ymin = h1a3.mse, ymax = h1a3.pse, colour = Phonation), width = 0.2)+
  geom_line(aes(colour = Phonation), linewidth = 1.5) +
  scale_colour_manual(values = colorblind) +
  scale_x_continuous(n.breaks = 10) +
  labs(title = "H1*-A3*",
       x = "Measurement Number",
       y = "H1*-A3* (dB)") +
  theme_bw()
h1a3.line

cpp.line <- ggplot(df.plot,
                    aes(x = measurement.no, y = cpp.mean)) + 
  geom_point(aes(shape = Phonation), size = 7) +
  scale_shape_manual(values = c(0, 1, 2, 5)) +
  # geom_smooth(aes(group = Phonation, colour = Phonation), stat = "identity") +
  geom_errorbar(aes(ymin = cpp.mse, ymax = cpp.pse, colour = Phonation), width = 0.2)+
  geom_line(aes(colour = Phonation), linewidth = 1.5) +
  scale_colour_manual(values = colorblind) +
  scale_x_continuous(n.breaks = 10) +
  labs(title = "CPP",
       x = "Measurement Number",
       y = "CPP (dB)") +
  theme_bw()
cpp.line

hnr05.line <- ggplot(df.plot,
                    aes(x = measurement.no, y = hnr05.mean)) + 
  geom_point(aes(shape = Phonation), size = 7) +
  scale_shape_manual(values = c(0, 1, 2, 5)) +
  # geom_smooth(aes(group = Phonation, colour = Phonation), stat = "identity") +
  geom_errorbar(aes(ymin = hnr05.mse, ymax = hnr05.pse, colour = Phonation), width = 0.2)+
  geom_line(aes(colour = Phonation), linewidth = 1.5) +
  scale_colour_manual(values = colorblind) +
  scale_x_continuous(n.breaks = 10) +
  labs(title = "HNR 500 Hz",
       x = "Measurement Number",
       y = "HNR 500 Hz (dB)") +
  theme_bw()
hnr05.line

hnr15.line <- ggplot(df.plot,
                    aes(x = measurement.no, y = hnr15.mean)) + 
  geom_point(aes(shape = Phonation), size = 3) +
  scale_shape_manual(values = c(0, 1, 2, 5)) +
  # geom_smooth(aes(group = Phonation, colour = Phonation), stat = "identity") +
  geom_errorbar(aes(ymin = hnr15.mse, ymax = hnr15.pse, colour = Phonation), width = 0.2)+
  geom_line(aes(colour = Phonation), linewidth = 1.5) +
  scale_colour_manual(values = colorblind) +
  scale_x_continuous(n.breaks = 10) +
  labs(title = "HNR 1500 Hz",
       x = "Measurement Number",
       y = "HNR 1500 Hz (dB)") +
  theme_bw()
hnr15.line

hnr25.line <- ggplot(df.plot,
                    aes(x = measurement.no, y = hnr25.mean)) + 
  geom_point(aes(shape = Phonation), size = 7) +
  scale_shape_manual(values = c(0, 1, 2, 5)) +
  # geom_smooth(aes(group = Phonation, colour = Phonation), stat = "identity") +
  geom_errorbar(aes(ymin = hnr25.mse, ymax = hnr25.pse, colour = Phonation), width = 0.2)+
  geom_line(aes(colour = Phonation), linewidth = 1.5) +
  scale_colour_manual(values = colorblind) +
  scale_x_continuous(n.breaks = 10) +
  labs(title = "HNR 2500 Hz",
       x = "Measurement Number",
       y = "HNR 2500 Hz (dB)") +
  theme_bw()
hnr25.line

hnr35.line <- ggplot(df.plot,
                    aes(x = measurement.no, y = hnr35.mean)) + 
  geom_point(aes(shape = Phonation), size = 7) +
  scale_shape_manual(values = c(0, 1, 2, 5)) +
  # geom_smooth(aes(group = Phonation, colour = Phonation), stat = "identity") +
  geom_errorbar(aes(ymin = hnr35.mse, ymax = hnr35.pse, colour = Phonation), width = 0.2)+
  geom_line(aes(colour = Phonation), linewidth = 1.5) +
  scale_colour_manual(values = colorblind) +
  scale_x_continuous(n.breaks = 10) +
  labs(title = "HNR 3500 Hz",
       x = "Measurement Number",
       y = "HNR 3500 Hz (dB)") +
  theme_bw()
hnr35.line

strF0.line <- ggplot(df.plot,
                    aes(x = measurement.no, y = strF0.mean)) + 
  geom_point(aes(shape = Phonation), size = 7) +
  scale_shape_manual(values = c(0, 1, 2, 5)) +
  # geom_smooth(aes(group = Phonation, colour = Phonation), stat = "identity") +
  geom_errorbar(aes(ymin = strF0.mse, ymax = strF0.pse, colour = Phonation), width = 0.2)+
  geom_line(aes(colour = Phonation), linewidth = 1.5) +
  scale_colour_manual(values = colorblind) +
  scale_x_continuous(n.breaks = 10) +
  labs(title = "f0",
       x = "Measurement Number",
       y = "f0 (Hz)") +
  theme_bw()
strF0.line

sF1.line <- ggplot(df.plot,
                    aes(x = measurement.no, y = sF1.mean)) + 
  geom_point(aes(shape = Phonation), size = 7) +
  scale_shape_manual(values = c(0, 1, 2, 5)) +
  # geom_smooth(aes(group = Phonation, colour = Phonation), stat = "identity") +
  geom_errorbar(aes(ymin = sF1.mse, ymax = sF1.pse, colour = Phonation), width = 0.2)+
  geom_line(aes(colour = Phonation), linewidth = 1.5) +
  scale_colour_manual(values = colorblind) +
  scale_x_continuous(n.breaks = 10) +
  labs(title = "F1",
       x = "Measurement Number",
       y = "F1 (Hz)") +
  theme_bw()
sF1.line

sF2.line <- ggplot(df.plot,
                    aes(x = measurement.no, y = sF2.mean)) + 
  geom_point(aes(shape = Phonation), size = 7) +
  scale_shape_manual(values = c(0, 1, 2, 5)) +
  # geom_smooth(aes(group = Phonation, colour = Phonation), stat = "identity") +
  geom_errorbar(aes(ymin = sF2.mse, ymax = sF2.pse, colour = Phonation), width = 0.2)+
  geom_line(aes(colour = Phonation), linewidth = 1.5) +
  scale_colour_manual(values = colorblind) +
  scale_x_continuous(n.breaks = 10) +
  labs(title = "F2",
       x = "Measurement Number",
       y = "F2 Hz") +
  theme_bw()
sF2.line

sB1.line <- ggplot(df.plot,
                    aes(x = measurement.no, y = sB1.mean)) + 
  geom_point(aes(shape = Phonation), size = 7) +
  scale_shape_manual(values = c(0, 1, 2, 5)) +
  # geom_smooth(aes(group = Phonation, colour = Phonation), stat = "identity") +
  geom_errorbar(aes(ymin = sB1.mse, ymax = sB1.pse, colour = Phonation), width = 0.2)+
  geom_line(aes(colour = Phonation), linewidth = 1.5) +
  scale_colour_manual(values = colorblind) +
  scale_x_continuous(n.breaks = 10) +
  labs(title = "B1",
       x = "Measurement Number",
       y = "B1") +
  theme_bw()
sB1.line

sB2.line <- ggplot(df.plot,
                    aes(x = measurement.no, y = sB2.mean)) + 
  geom_point(aes(shape = Phonation), size = 7) +
  scale_shape_manual(values = c(0, 1, 2, 5)) +
  # geom_smooth(aes(group = Phonation, colour = Phonation), stat = "identity") +
  geom_errorbar(aes(ymin = sB2.mse, ymax = sB2.pse, colour = Phonation), width = 0.2)+
  geom_line(aes(colour = Phonation), linewidth = 1.5) +
  scale_colour_manual(values = colorblind) +
  scale_x_continuous(n.breaks = 10) +
  labs(title = "B2",
       x = "Measurement Number",
       y = "B2") +
  theme_bw()
sB2.line

energy.line <- ggplot(df.plot,
                    aes(x = measurement.no, y = energy.mean)) + 
  geom_point(aes(shape = Phonation), size = 7) +
  scale_shape_manual(values = c(0, 1, 2, 5)) +
  # geom_smooth(aes(group = Phonation, colour = Phonation), stat = "identity") +
  geom_errorbar(aes(ymin = energy.mse, ymax = energy.pse, colour = Phonation), width = 0.2)+
  geom_line(aes(colour = Phonation), linewidth = 1.5) +
  scale_colour_manual(values = colorblind) +
  scale_x_continuous(n.breaks = 10) +
  labs(title = "Energy",
       x = "Measurement Number",
       y = "Energy") +
  theme_bw()
energy.line

norm.soe.line <- ggplot(df.plot,
                    aes(x = measurement.no, y = norm.soe.mean)) + 
  geom_point(aes(shape = Phonation), size = 3) +
  scale_shape_manual(values = c(0, 1, 2, 5)) +
  # geom_smooth(aes(group = Phonation, colour = Phonation), stat = "identity") +
  geom_errorbar(aes(ymin = norm.soe.mse, ymax = norm.soe.pse, colour = Phonation), width = 0.2)+
  geom_line(aes(colour = Phonation), linewidth = 1) +
  scale_colour_manual(values = colorblind) +
  scale_x_continuous(n.breaks = 10) +
  labs(title = "Strength of Excitation",
       x = "Measurement Number",
       y = "Strength of Excitation (normalized)") +
  theme_bw()
norm.soe.line

H1c.resid.line <- ggplot(df.plot,
                    aes(x = measurement.no, y = H1c.resid.mean)) + 
  geom_point(aes(shape = Phonation), size = 3) +
  scale_shape_manual(values = c(0, 1, 2, 5)) +
  # geom_smooth(aes(group = Phonation, colour = Phonation), stat = "identity") +
  geom_errorbar(aes(ymin = H1c.resid.mse, ymax = H1c.resid.pse, colour = Phonation), width = 0.2)+
  geom_line(aes(colour = Phonation), linewidth = 1) +
  scale_colour_manual(values = colorblind) +
  scale_x_continuous(n.breaks = 10) +
  labs(title = "Residual H1*",
       x = "Measurement Number",
       y = "Residual H1* (normalized)") +
  theme_bw()
H1c.resid.line

H1c.resid.line.sp <- ggplot(df.plot,
                    aes(x = measurement.no, y = H1c.resid.mean)) + 
  geom_point(aes(shape = Phonation), size = 3) +
  scale_shape_manual(values = c(0, 1, 2, 5)) +
  # geom_smooth(aes(group = Phonation, colour = Phonation), stat = "identity") +
  geom_errorbar(aes(ymin = H1c.resid.mse, ymax = H1c.resid.pse, colour = Phonation), width = 0.2)+
  geom_line(aes(colour = Phonation), linewidth = 1) +
  scale_colour_manual(values = colorblind) +
  facet_wrap(.~Speaker, nrow = 2, ncol = 5) +
  scale_x_continuous(n.breaks = 10) +
  labs(title = "Residual H1* by speaker",
       x = "Measurement Number",
       y = "Residual H1* (normalized)") +
  theme_bw()
H1c.resid.line.sp

a1c.line <- ggplot(df.plot,
                    aes(x = measurement.no, y = a1c.mean)) + 
  geom_point(aes(shape = Phonation), size = 3) +
  scale_shape_manual(values = c(0, 1, 2, 5)) +
  # geom_smooth(aes(group = Phonation, colour = Phonation), stat = "identity") +
  geom_errorbar(aes(ymin = a1c.mse, ymax = a1c.pse, colour = Phonation), width = 0.2)+
  geom_line(aes(colour = Phonation), linewidth = 1) +
  scale_colour_manual(values = colorblind) +
  scale_x_continuous(n.breaks = 10) +
  # facet_wrap(.~Vowel, ncol = 5) +
  labs(title = "A1*",
       x = "Measurement Number",
       y = "A1* (dB)") +
  theme_bw()
a1c.line

a1c.resid.line.sp <- ggplot(df.plot,
                    aes(x = measurement.no, y = a1c.mean)) + 
  geom_point(aes(shape = Phonation), size = 3) +
  scale_shape_manual(values = c(0, 1, 2, 5)) +
  # geom_smooth(aes(group = Phonation, colour = Phonation), stat = "identity") +
  geom_errorbar(aes(ymin = a1c.mse, ymax = a1c.pse, colour = Phonation), width = 0.2)+
  geom_line(aes(colour = Phonation), linewidth = 1) +
  scale_colour_manual(values = colorblind) +
  facet_wrap(.~Speaker, nrow = 2, ncol = 5) +
  scale_x_continuous(n.breaks = 10) +
  labs(title = "A1* by speaker",
       x = "Measurement Number",
       y = "A1* (db)") +
  theme_bw()
a1c.resid.line.sp


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
       width = 6,
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

ggsave(filename = "reports/figs/h1h2_line_2:1.eps",
       plot = h1h2.line,
       width = 6,
       height = 3,
       dpi = 300,
       units = "in")

ggsave(filename = "reports/figs/H1c_resid_line_2:1.eps",
       plot = H1c.resid.line,
       width = 6,
       height = 3,
       dpi = 300,
       units = "in")

ggsave(filename = "reports/figs/h1_clean_line.eps",
       plot = h1.clean.line,
       width = 6,
       height = 4,
       dpi = 300,
       units = "in")

ggsave(filename = "reports/figs/h1a3_clean_line.png",
       plot = h1a3.clean.line,
       width = 5,
       height = 3,
       dpi = 300,
       units = "in")

ggsave(filename = "reports/figs/a1_clean_line.png",
       plot = a1c.line,
       width = 6,
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
