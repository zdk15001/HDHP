## Project:  HDHP: Diffusion Index
# Located:   GITHUB Repository, data on google drive
# File Name: HDHP-KFF.R
# Date:      Last updated 2025_9_11
# Who:       Zachary Kline, Mina Guglietta, and Daniel Baron


####################################################################################
############              Pre-Analysis: settings, packages, and data    ############
####################################################################################
### NOTE: Cloned from Github - data kept on google drive

### Settings + Packages
# Kline's command to set WD
setwd("G:/My Drive/EDU_SYNC/Research/Active/HDHP/work")

# Mina's command to set WD
setwd("~/Google Drive/My Drive/HDHP MUSE 2025/KFF Data/Data")

# Daniel's command to set WD

#install.packages("dplyr")
#install.packages("readxl")
#install.packages("janitor")
#install.packages("ggplot2")
#install.packages("haven")


library(dplyr)
library(readxl)
library(janitor)
library(ggplot2)
library(haven)


# load the data
kff_2000 <- read_sav('health benefits 00.sav')
kff_2001 <- read_sav('health benefits 01.sav')
kff_2002 <- read_sav('health benefits 02.sav')
kff_2003 <- read_sav('health benefits 03.sav')
kff_2004 <- read_sav('health benefits 04.sav')
kff_2005 <- read_sav('health benefits 05.sav')
kff_2006 <- read_sav('health benefits 06 updated 2014.sav')
kff_2007 <- read_sav('health benefits 07 updated 2014.sav')
kff_2008 <- read_sav('health benefits 08 updated 2014.sav')
kff_2009 <- read_sav('health benefits 09 updated 2014.sav')
kff_2010 <- read_sav('health benefits 10 updated 2014.sav')
kff_2011 <- read_sav('health benefits 11 updated.sav')
kff_2012 <- read_sav('health benefits 12.sav')
kff_2013 <- read_sav('health benefits 2013_updated.sav')
kff_2014 <- read_sav('health benefits 2014.sav')
kff_2015 <- read_sav('health benefits 2015.sav')
kff_2016 <- read_sav('health benefits 2016.sav')
kff_2017 <- read_sav('health benefits 2017.sav')
kff_2018 <- read_sav('health benefits 2018.sav')
kff_2019 <- read_sav('health benefits 2019.sav')
kff_2020 <- read_sav('health benefits 2020.sav')
kff_2021 <- read.csv('health benefits 2021.sav')
kff_2022 <- read.csv('2022-11-28 health benefits 2022.sav')
kff_2023 <- read.csv('2023-10-17 health benefits 2023.sav')
kff_2024 <- read.csv('2022-10-10 health benefits 2024.sav')






