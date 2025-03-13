#------------------------------------------------------------------------------
# File: 07_gamm_hnr15.R
# Project: LCH_GAMMs
# Author: Mykel Brinkerhoff
# Date: 2025-03-06 (Th)
# Description: 
#       This script runs the GAMM for HNR15.
#
# Usage:
#   Rscript 07_gamm_hnr15.R
#
# Notes:
#   - Ensure all required packages are installed.
#   - Modify the script as needed for your specific dataset and analysis requirements.
#------------------------------------------------------------------------------

# load in the dataframe 
slz_gamm <- slz_normalized

# Low tone only
slz_gamm_L <- slz_normalized %>%
  filter(Tone == "L")

# factorize the variables
slz_gamm_L$Phonation <- slz_gamm_L$Phonation %>% 
    factor(levels = c("modal",
                      "breathy", 
                      "checked", 
                      "rearticulated"))
slz_gamm_L$Speaker <- slz_gamm_L$Speaker %>% 
    factor()
slz_gamm_L$Vowel <- slz_gamm_L$Vowel %>% 
    factor()
slz_gamm_L$Word <- slz_gamm_L$Word %>%
    factor()


# Linear gamma model for HNR15
hnr15_linear <- bam(hnr15 ~ Phonation, 
                    data = slz_gamm_L, method = "ML")
summary(hnr15_linear)

# Extend the linear model over time
hnr15_gam <- bam(hnr15 ~ Phonation +
                   s(measurement_no),
                 data = slz_gamm_L, method = "ML")
summary(hnr15_gam)

# plotting the smooths and differences
par(mfrow=c(2,2))

plot_smooth(hnr15_gam, view="measurement_no",
            plot_all="Phonation", rug=F, col = colorblind)

plot_diff(hnr15_gam, view="measurement_no", 
          comp=list(Phonation=c("breathy","modal"))) 

plot_diff(hnr15_gam, view="measurement_no",
            comp=list(Phonation=c("checked","modal")))

plot_diff(hnr15_gam, view="measurement_no",
            comp=list(Phonation=c("rearticulated","modal")))

gam.check(hnr15_gam)

# Extending the measurement_no smooth by Phonation
hnr15_gam2 <- bam(hnr15 ~ Phonation +
                    s(measurement_no, by=Phonation),
                  data = slz_gamm_L, method = "ML")

summary(hnr15_gam2)

# plotting the smooths and differences
par(mfrow=c(2,2))

plot_smooth(hnr15_gam2, view="measurement_no",
            plot_all="Phonation", rug=F, col = colorblind)

plot_diff(hnr15_gam2, view="measurement_no",
            comp=list(Phonation=c("breathy","modal")))
plot_diff(hnr15_gam2, view="measurement_no",
            comp=list(Phonation=c("checked","modal")))
plot_diff(hnr15_gam2, view="measurement_no",
            comp=list(Phonation=c("rearticulated","modal")))

gam.check(hnr15_gam2)

# model comparison
compareML(hnr15_gam, hnr15_gam2)

# phonation as ordered factor
slz_gamm_L$phon_ord <- as.ordered(slz_gamm_L$Phonation)
contrasts(slz_gamm_L$phon_ord) <- "contr.treatment"

# model with ordered factor
hnr15_gam3 <- bam(hnr15 ~ phon_ord +
                    s(measurement_no),
                  data = slz_gamm_L, method = "ML")
summary(hnr15_gam3)

# checking the model
gam.check(hnr15_gam3)

# plotting the smooths and differences
par(mfrow=c(2,2))

plot_smooth(hnr15_gam3, view="measurement_no",
            plot_all="phon_ord", rug=F, col = colorblind)

plot_diff(hnr15_gam3, view="measurement_no",
            comp=list(phon_ord=c("breathy","modal")))
plot_diff(hnr15_gam3, view="measurement_no",
            comp=list(phon_ord=c("checked","modal")))
plot_diff(hnr15_gam3, view="measurement_no",
            comp=list(phon_ord=c("rearticulated","modal")))

# model comparison
compareML(hnr15_gam, hnr15_gam3)
compareML(hnr15_gam2, hnr15_gam3)

# model with ordered factor and interaction
hnr15_gam4 <- bam(hnr15 ~ phon_ord +
                    s(measurement_no, by=phon_ord),
                  data = slz_gamm_L, method = "ML")
summary(hnr15_gam4)

# checking the model
gam.check(hnr15_gam4)

# plotting the smooths and differences
par(mfrow=c(2,2))

plot_smooth(hnr15_gam4, view="measurement_no",
            plot_all="phon_ord", rug=F, col = colorblind)

plot_diff(hnr15_gam4, view="measurement_no",
            comp=list(phon_ord=c("breathy","modal")))
plot_diff(hnr15_gam4, view="measurement_no",
            comp=list(phon_ord=c("checked","modal")))
plot_diff(hnr15_gam4, view="measurement_no",
            comp=list(phon_ord=c("rearticulated","modal")))

# model comparison between hnr15_gam3 and hnr15_gam4
compareML(hnr15_gam3, hnr15_gam4)

# model with random effect
hnr15_gam5 <- bam(hnr15 ~ phon_ord +
                     s(measurement_no) +
                     s(Speaker, bs="re"),
                   data = slz_gamm_L, method = "ML")
summary(hnr15_gam5)

# checking the model
gam.check(hnr15_gam5)

# model comparison
compareML(hnr15_gam4, hnr15_gam5)

# model with random effect of speaker and word
hnr15_gam6 <- bam(hnr15 ~ phon_ord +
                    s(measurement_no) +
                    s(Speaker.f1, bs="re") +
                    s(Word, bs="re"),
                    data = slz_gamm_L, method = "ML")
summary(hnr15_gam6)

# checking the model
gam.check(hnr15_gam6)

# model comparison
compareML(hnr15_gam5, hnr15_gam6)

# plotting the smooths and differences
par(mfrow=c(2,2))

plot_smooth(hnr15_gam6, view="measurement_no",
            plot_all="phon_ord", rug=F, col = colorblind)

plot_diff(hnr15_gam6, view="measurement_no",
            comp=list(phon_ord=c("breathy","modal")))
plot_diff(hnr15_gam6, view="measurement_no",
            comp=list(phon_ord=c("checked","modal")))
plot_diff(hnr15_gam6, view="measurement_no",
            comp=list(phon_ord=c("rearticulated","modal")))

# model with random effect of speaker by word
hnr15_gam7 <- bam(hnr15 ~ phon_ord +
                    s(measurement_no) +
                    s(Speaker.f1, bs="re") +
                    s(Word, by=Speaker.f1, bs="re"),
                  data = slz_gamm_L, method = "ML")
summary(hnr15_gam7)

# Model check
gam.check(hnr15_gam7)

# model comparison
compareML(hnr15_gam6, hnr15_gam7)

# plotting the smooths and differences for hnr15_gam7
par(mfrow = c(2,2))

plot_smooth(hnr15_gam7, view = "measurement_no", 
            plot_all = "phon_ord", rug = F, col = colorblind)

plot_diff(hnr15_gam7, view="measurement_no",
          comp=list(phon_ord=c("breathy","modal")))
plot_diff(hnr15_gam7, view="measurement_no",
          comp=list(phon_ord=c("checked","modal")))
plot_diff(hnr15_gam7, view="measurement_no",
          comp=list(phon_ord=c("rearticulated","modal")))

# model with random effect of speaker by word
hnr15_gam8 <- bam(hnr15 ~ phon_ord +
                    s(measurement_no) +
                    s(Speaker, bs="re") +
                    s(Word, Speaker, bs="re"),
                  data = slz_gamm_L, method = "ML")
summary(hnr15_gam8)

# Model check
gam.check(hnr15_gam8)

# model comparison
compareML(hnr15_gam7, hnr15_gam8)

# plotting the smooths and differences for hnr15_gam7
par(mfrow = c(2,2))

plot_smooth(hnr15_gam8, view = "measurement_no", 
            plot_all = "phon_ord", rug = F, col = colorblind)

plot_diff(hnr15_gam8, view="measurement_no",
          comp=list(phon_ord=c("breathy","modal")))
plot_diff(hnr15_gam8, view="measurement_no",
          comp=list(phon_ord=c("checked","modal")))
plot_diff(hnr15_gam8, view="measurement_no",
          comp=list(phon_ord=c("rearticulated","modal")))

# model with random effect of speaker by word
hnr15_gam9 <- bam(hnr15 ~ phon_ord +
                    s(measurement_no) +
                    s(Speaker, bs="re") +
                    s(Word, Speaker, by = phon_ord, bs="re"),
                  data = slz_gamm_L, method = "ML")
summary(hnr15_gam9)

# Model check
gam.check(hnr15_gam9)

# model comparison
compareML(hnr15_gam8, hnr15_gam9)

# plotting the smooths and differences for hnr15_gam7
par(mfrow = c(2,2))

plot_smooth(hnr15_gam7, view = "measurement_no", 
            plot_all = "phon_ord", rug = F, col = colorblind)

plot_diff(hnr15_gam7, view="measurement_no",
          comp=list(phon_ord=c("breathy","modal")))
plot_diff(hnr15_gam7, view="measurement_no",
          comp=list(phon_ord=c("checked","modal")))
plot_diff(hnr15_gam7, view="measurement_no",
          comp=list(phon_ord=c("rearticulated","modal")))
