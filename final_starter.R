#####starter code####

#first load libraries (install if necessary)
library(tidyverse)
library(emmeans)
library(fixest)
library(EffectLiteR)
library(sandwich)
library(marginaleffects)


#load final_df.csv using a preferred method (e.g., readr package)
#make sure to either have a common project folder with the datafile, a proper
#pathname, or use the here() package
read_csv("final_df.csv")


