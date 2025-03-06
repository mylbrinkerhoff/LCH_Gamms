#----------------------------------------------------------------------------------------
# File: 03_Outlier_Removal.R
# Project: 
# Author: Mykel Brinkerhoff
# Date: 2025-03-05 (W)
# Description: This scripts identifies and removes outliers from the dataset based on 
#              z-score of F1 and F2, energy, and mahalnobis distance and saves the 
#              cleaned dataset as a .csv file.
#
# Usage:
#   Rscript 03_Outlier_Removal.R
#
# Notes:
#   - Ensure all required packages are installed.
#   - Modify the script as needed for your specific dataset and analysis requirements.
#----------------------------------------------------------------------------------------

slz_filtered = slz_trans %>%
  group_by(Speaker) %>%
  mutate(F0z = (f0 - mean(f0, na.rm = T))/sd(f0, na.rm = T)) %>%
  ungroup()

slz_filtered = slz_filtered %>%
  mutate(str_outlier = if_else(abs(F0z) > 3, "outlier", "OK"))

### Formant outlier flagging
### Calculate Mahalanobis distance for formants
vmahalanobis = function (dat) {
  if (nrow(dat) < 25) {
    dat$zF1F2 = NA
    return(dat)
  }
  means = c(mean(dat$f1, na.rm=T), mean(dat$f2, na.rm=T))
  cov = cov(cbind(dat$f1, dat$f2))
  
  dat$zF1F2 = mahalanobis(cbind(dat$f1, dat$f2),
                          center=means, cov=cov)
  dat
}

# Distance larger than 6 is considered as outlier
distance_cutoff = 6

# Perform Mahalanobis on dataset
slz_filtered =  slz_filtered %>%
  group_by(Vowel) %>%
  do(vmahalanobis(.)) %>%
  ungroup() %>%
  mutate(formant_outlier = NA)

# Visualize the formants with flagged values
slz_filtered %>% 
  filter(is.na(formant_outlier)) %>%
  ggplot(aes(x = f2, y = f1, color = zF1F2 > distance_cutoff)) +
  geom_point(size = 0.6) +
  facet_wrap(.~Vowel)+
  scale_y_reverse(limits = c(2000,0),position = "right") + 
  scale_x_reverse(limits = c(3500,0),position = "top")+
  theme_bw()

for (i in 1:nrow(slz_filtered)) {
  if (!is.na(slz_filtered$zF1F2[i])) {
    if (slz_filtered$zF1F2[i] > distance_cutoff){
      slz_filtered$formant_outlier[i] = "outlier"
    }
  }
}

# Visualize the vowel formant after exclusion
slz_filtered %>% 
  filter(is.na(formant_outlier)) %>%
  ggplot(aes(x = f2, y = f1)) +
  geom_point(size = 0.6) +
  #geom_text()+
  facet_wrap(.~Vowel)+
  #geom_density_2d() +
  #  scale_color_manual(values=c('#a6611a','#slzc27d','#018571'))+
  scale_y_reverse(limits = c(2000,0),position = "right") + 
  scale_x_reverse(limits = c(3500,0),position = "top")+
  theme_bw()

slz_filtered <- slz_filtered %>%
  filter(str_outlier == "OK")

slz_filtered <- slz_filtered %>% 
  filter(is.na(formant_outlier))

# removing energy outliers
slz_filtered$energy[slz_filtered$energy == 0] <- NA

slz_filtered <- slz_filtered %>%
  mutate(energy = log10(energy))

slz_filtered <- slz_filtered %>% 
  filter(!is.na(energy))

# removing the columns for outlier removal
slz_filtered <- slz_filtered %>% 
  select(-c(str_outlier,
            formant_outlier,
            zF1F2,
            F0z
            )
         )

# writing the  filtered dataframe
write.csv(slz_filtered, 
          file = "data/interim/slz_filtered.csv", 
          row.names = F, 
          fileEncoding = "UTF-8")
