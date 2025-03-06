#----------------------------------------------------------------------------------------
# File: 06_variable_coding.R
# Project: LCH_GAMMs
# Author: Mykel Brinkerhoff
# Date: 2025-03-05 (W)
# Description: This script changes the coding of the variables for the HNR15 GAMM.
#
# Usage:
#   Rscript 06_variable_coding.R
#
# Notes:
#   - Ensure all required packages are installed.
#   - Modify the script as needed for your specific dataset and analysis requirements.
#----------------------------------------------------------------------------------------

# Orthogonal coding for Tone
slz_normalized$measurement_no <- as.numeric(slz_normalized$measurement_no)
slz_normalized$Tone.f1 <- slz_normalized$Tone %>% factor(levels = c("H", "M", "L", "F", "R"))

# assigning the orthogonal polynomial contrasts to Tone
contrasts(slz_normalized$Tone.f1) <- contr.poly(5)
table(slz_normalized$Tone.f1)

# Simple coding for Gender
slz_normalized <- slz_normalized %>% 
  mutate(Gender = if_else(str_detect(Speaker, 'f'), "Female", "Male"))

slz_normalized$Gender.f <- slz_normalized$Gender %>% factor()

#creating the contrast matrix manually by modifying the dummy coding scheme
(c <-contr.treatment(2))
my.coding <- matrix(rep(1/2, 2), ncol=1)
my.simple <- c-my.coding
my.simple

contrasts(slz_normalized$Gender.f) <- my.simple
slz_normalized$Gender.f 

# Orthogonal coding for Gender
slz_normalized$Gender.f1 <- slz_normalized$Gender %>% factor()
contrasts(slz_normalized$Gender.f1) <- contr.poly(2)
table(slz_normalized$Gender.f1)

# Orthogonal coding for Speaker
slz_normalized$Speaker.f1 <- slz_normalized$Speaker %>% factor()
contrasts(slz_normalized$Speaker.f1) <- contr.poly(10)
table(slz_normalized$Speaker.f1)

# Orthogonal coding for Vowel
slz_normalized$Vowel.f1 <- slz_normalized$Vowel %>% factor()
contrasts(slz_normalized$Vowel.f1) <- contr.poly(5)
table(slz_normalized$Vowel.f1)
