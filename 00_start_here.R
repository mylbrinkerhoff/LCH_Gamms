# File: 00_start_here.R
# Project: LCH_GAMMs
# Author: Mykel Brinkerhoff
# Date: 2025-03-05 (W)
# Description: This script loads in the packages required for the GAMM analysis
#
# Usage:
#   Rscript 00_start_here.R
#
# Notes:
#   - Ensure all required packages are installed.
#   - Modify the script as needed for your specific dataset and analysis requirements.


# Install packages if not yet installed
packages <- c("lme4",
              "tidyverse", 
              "mgcv",
              "itsadug", 
              "reshape2", 
              "readr",
              "here",
              # "tidygam",
              # "tidymv",
              "remotes",
              "ggpubr",
              "cowplot"  
) 

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

remotes::install_github(
  "stefanocoretta/tidymv@devel",
  build_vignettes = TRUE
)

# Loading the packages and functions 
library(lme4) # required for residual H1*
library(mgcv) # required for GAMM
library(itsadug) # required for GAMM
library(tidyverse) # required for data manipulation
library(reshape2) # required for data manipulation
library(readr) # required for reading in data
library(here) # required for setting the working directory
library(tidygam) # required for GAMM
library(tidymv) # required for GAMM
library(cowplot) # required for plotting
library(ggpubr) # required for plotting
library(remotes) # required for installing packages from GitHub
source("R/gamm_hacks.R") # series of functions that make GAMM easier to work with

# Loading the data

slz <- read.csv(here("data/raw/", "Voice_Master_Split.csv"))

# Create a variable for colorblind palette

colorblind <- palette.colors(palette = "Okabe-Ito")

## set the theme and color palette for ggplot2
theme_set(theme_bw())

## terms to exclude from gamm plots
to_exclude <- c("s(Speaker.f1)", "s(Speaker.f1,Phonation)", 
                "te(measurement_no,Iter):Phonationmodal",
                "te(measurement_no,Iter):Phonationbreathy",
                "te(measurement_no,Iter):Phonationchecked",
                "te(measurement_no,Iter):Phonationrearticulated")
