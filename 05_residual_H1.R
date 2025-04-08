#----------------------------------------------------------------------------------------
# File: 05_residual_H1.R
# Project: LC_GAMMs
# Author: Mykel Brinkerhoff
# Date: 2025-03-05 (W)
# Description: This scripts calculates residual H1* according to Chai & Garellek (2022).
#
# Usage:
#   Rscript 05_residual_H1.R
#
# Notes:
#   - Ensure all required packages are installed.
#   - Modify the script as needed for your specific dataset and analysis requirements.
#----------------------------------------------------------------------------------------

### Calculating Residual h1
#### Generate the lmer model for residual h1
model_position_h1c_covariant <- lmer(h1cz ~ energyz + (energyz||Speaker),
                                    data = slz_normalized,
                                    REML = FALSE)

#### extract the energy factor
energy_factor <- fixef(model_position_h1c_covariant)[2]

#### generate the residual H1 score
slz_normalized$resid_H1 = slz_normalized$h1cz - slz_normalized$energyz * energy_factor

#### Save the normalized dataset WITH residual H1
write.csv(slz_normalized, 
          file = "data/processed/slz_normalized.csv", 
          row.names = F, 
          fileEncoding = "UTF-8")
