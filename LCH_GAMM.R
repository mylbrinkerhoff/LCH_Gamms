####################################################################
###
###   LCH_GAMM.R
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
slz_fil <- read.csv("data/interim/slz_filtered.csv", header = TRUE)