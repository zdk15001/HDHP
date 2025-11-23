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
setwd("~/Library/CloudStorage/GoogleDrive-gugliem2@tcnj.edu/.shortcut-targets-by-id/14oLkrWtHW1NzX87aL0DxDGo_9Ysj-XBQ/HDHP/work")

# Daniel's command to set WD
setwd("G:/.shortcut-targets-by-id/14oLkrWtHW1NzX87aL0DxDGo_9Ysj-XBQ/HDHP/work")

#install.packages("dplyr")
#install.packages("readxl")
#install.packages("janitor")
#install.packages("ggplot2")
#install.packages("haven")
#install.packages("tidyr")
#install.packages("lme4")

library(dplyr)
library(readxl)
library(janitor)
library(ggplot2)
library(haven)
library(tidyr)
library(lme4)

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
kff_2021 <- read.csv('health benefits 2021.csv') 
kff_2022 <- read.csv('2022-11-28 health benefits 2022.csv') 
kff_2023 <- read.csv('2023-10-17 health benefits 2023.csv')
kff_2024 <- read.csv('2024-10-10 health benefits 2024.csv')




####################################################################################
############              Phase 1: variable Cleaning        ############
####################################################################################

# follow three steps of cleaning data for each variable for each year
# step 1: examine the variable
# Step 2: Clean the variable (always create a new variable!)
# Step 3: Confirm cleaning was correct


###### Firm Offers High Deductible Health Plans #####
### 2003
#Step 1: Examine variable
summary(kff_2003$j3) 
table(kff_2003$j3, useNA = "ifany") 

# Step 2: Clean variable (always create new variable!)
kff_2003$offers <- ifelse(kff_2003$j3 == 1, 1, 
                          ifelse(kff_2003$j3 == 2, 0, NA))
kff_2003$doesnt_offer <- ifelse(kff_2003$j3 == 2, 1,
                                ifelse(kff_2003$j3 == 1, 0, NA))


# step 3: Confirm correct cleaning
table(kff_2003$j3, kff_2003$offers, useNA = "ifany")
table(kff_2003$j3, kff_2003$doesnt_offer, useNA = "ifany")



### 2004
#Step 1: Examine variable
summary(kff_2004$j3)

# Step 2: Clean variable (always create new variable!)
kff_2004$offers <- ifelse(kff_2004$j3 == 1, 1, 
                          ifelse(kff_2004$j3 == 2, 0,
                                 ifelse(kff_2004$j3 == 3, NA, NA)))
kff_2004$doesnt_offer <- ifelse(kff_2004$j3 == 2,1,
                                ifelse(kff_2004$j3 == 1,0,
                                       ifelse(kff_2004$j3 == 3, NA, NA)))

# step 3: Confirm correct cleaning
table(kff_2004$j3, kff_2004$offers, useNA = "ifany")
table(kff_2004$j3, kff_2004$doesnt_offer, useNA = "ifany")


### 2005
#Step 1: Examine variable
table(kff_2005$b8e) 

# Step 2: Clean variable (always create new variable!)
kff_2005$offers <- ifelse(kff_2005$b8e == 1, 1, 0)
kff_2005$doesnt_offer <- ifelse(kff_2005$b8e == 2,1,0)

# step 3: Confirm correct cleaning
table(kff_2005$b8e, kff_2005$offers, useNA = "ifany")
table(kff_2005$b8e, kff_2005$doesnt_offer, useNA = "ifany")


###2006
#Step 1: Examine variable
table(kff_2006$b8e) 

# Step 2: Clean variable (always create new variable!)
kff_2006$offers <- ifelse(kff_2006$b8e == 1, 1, 0)
kff_2006$doesnt_offer <- ifelse(kff_2006$b8e == 2,1,0)

# step 3: Confirm correct cleaning
table(kff_2006$b8e, kff_2006$offers, useNA = "ifany")
table(kff_2006$b8e, kff_2006$doesnt_offer, useNA = "ifany")


###2007
#Step 1: Examine variable
table(kff_2007$b8e)

# Step 2: Clean variable (always create new variable!)
kff_2007$offers <- ifelse(kff_2007$b8e == 1, 1, 0)
kff_2007$doesnt_offer <- ifelse(kff_2007$b8e == 2,1,0)

# step 3: Confirm correct cleaning
table(kff_2007$b8e, kff_2007$offers, useNA = "ifany")
table(kff_2007$b8e, kff_2007$doesnt_offer, useNA = "ifany")

###2008
table(kff_2008$b8e)

kff_2008$offers <- ifelse(kff_2008$b8e == 1, 1, 0)
kff_2008$doesnt_offer <- ifelse(kff_2008$b8e == 2,1,0)

table(kff_2008$b8e, kff_2008$offers, useNA = "ifany")
table(kff_2008$b8e, kff_2008$doesnt_offer, useNA = "ifany")


###2009
table(kff_2009$b8e)

kff_2009$offers <- ifelse(kff_2009$b8e == 1, 1, 0)
kff_2009$doesnt_offer <- ifelse(kff_2009$b8e == 2,1,0)

table(kff_2009$b8e, kff_2009$offers, useNA = "ifany")
table(kff_2009$b8e, kff_2009$doesnt_offer, useNA = "ifany")


###2010
table(kff_2010$b8e)

kff_2010$offers <- ifelse(kff_2010$b8e == 1, 1, 0)
kff_2010$doesnt_offer <- ifelse(kff_2010$b8e == 2,1,0)

table(kff_2010$b8e, kff_2010$offers, useNA = "ifany")
table(kff_2010$b8e, kff_2010$doesnt_offer, useNA = "ifany")


###2011
table(kff_2011$b8e)

kff_2011$offers <- ifelse(kff_2011$b8e == 1, 1, 0)
kff_2011$doesnt_offer <- ifelse(kff_2011$b8e == 2,1,0)

table(kff_2011$b8e, kff_2011$offers, useNA = "ifany")
table(kff_2011$b8e, kff_2011$doesnt_offer, useNA = "ifany")


###2012
##ERROR ERROR: NO b8e IN 2012 DATASET (there is b8e in 2012 codebook though)
summary(kff_2012$b8e)


###2013
table(kff_2013$b8e)

kff_2013$offers <- ifelse(kff_2013$b8e == 1, 1, 
                         ifelse(kff_2013$b8e == 2, 0,
                                ifelse(kff_2013$b8e == 3, NA, NA)))
kff_2013$doesnt_offer <- ifelse(kff_2013$b8e == 2,1,
                               ifelse(kff_2013$b8e == 1,0,
                                      ifelse(kff_2013$b8e == 3, NA, NA)))

table(kff_2013$b8e, kff_2013$offers, useNA = "ifany")
table(kff_2013$b8e, kff_2013$doesnt_offer, useNA = "ifany")



####2014
table(kff_2014$b8e)

kff_2014$offers <- ifelse(kff_2014$b8e == 1, 1, 
                         ifelse(kff_2014$b8e == 2, 0,
                                ifelse(kff_2014$b8e == 3, NA, NA)))
kff_2014$doesnt_offer <- ifelse(kff_2014$b8e == 2,1,
                               ifelse(kff_2014$b8e == 1,0,
                                      ifelse(kff_2014$b8e == 3, NA, NA)))

table(kff_2014$b8e, kff_2014$offers, useNA = "ifany")
table(kff_2014$b8e, kff_2014$doesnt_offer, useNA = "ifany")



###2015
table(kff_2015$b8e)

kff_2015$offers <- ifelse(kff_2015$b8e == 1, 1, 0)
kff_2015$doesnt_offer <- ifelse(kff_2015$b8e == 2,1,0)

table(kff_2015$b8e, kff_2015$offers, useNA = "ifany")
table(kff_2015$b8e, kff_2015$doesnt_offer, useNA = "ifany")


###2016
table(kff_2016$b8e)

kff_2016$offers <- ifelse(kff_2016$b8e == 1, 1, 
                        ifelse(kff_2016$b8e == 2, 0,
                               ifelse(kff_2016$b8e == 3, NA, NA)))
kff_2016$doesnt_offer <- ifelse(kff_2016$b8e == 2,1,
                              ifelse(kff_2016$b8e == 1,0,
                                     ifelse(kff_2016$b8e == 3, NA, NA)))

table(kff_2016$b8e, kff_2016$offers, useNA = "ifany")
table(kff_2016$b8e, kff_2016$doesnt_offer, useNA = "ifany")


###2017
table(kff_2017$b8e)

kff_2017$offers <- ifelse(kff_2017$b8e == 1, 1, 
                        ifelse(kff_2017$b8e == 2, 0,
                               ifelse(kff_2017$b8e == 3, NA, NA)))
kff_2017$doesnt_offer <- ifelse(kff_2017$b8e == 2,1,
                              ifelse(kff_2017$b8e == 1,0,
                                     ifelse(kff_2017$b8e == 3, NA, NA)))

table(kff_2017$b8e, kff_2017$offers, useNA = "ifany")
table(kff_2017$b8e, kff_2017$doesnt_offer, useNA = "ifany")

###2018
table(kff_2018$b8e)

kff_2018$offers <- ifelse(kff_2018$b8e == 1, 1, 
                        ifelse(kff_2018$b8e == 2, 0,
                               ifelse(kff_2018$b8e == 3, NA, NA)))
kff_2018$doesnt_offer <- ifelse(kff_2018$b8e == 2,1,
                              ifelse(kff_2018$b8e == 1,0,
                                     ifelse(kff_2018$b8e == 3, NA, NA)))

table(kff_2018$b8e, kff_2018$offers, useNA = "ifany")
table(kff_2018$b8e, kff_2018$doesnt_offer, useNA = "ifany")


###2019
table(kff_2019$b8e,useNA = "ifany")

kff_2019$offers <- ifelse(kff_2019$b8e == 1, 1, 
                        ifelse(kff_2019$b8e == 2, 0,
                               ifelse(kff_2019$b8e == 3, NA, NA)))
kff_2019$doesnt_offer <- ifelse(kff_2019$b8e == 2,1,
                              ifelse(kff_2019$b8e == 1,0,
                                     ifelse(kff_2019$b8e == 3, NA, NA)))

table(kff_2019$b8e, kff_2019$offers, useNA = "ifany")
table(kff_2019$b8e, kff_2019$doesnt_offer, useNA = "ifany")

###2020
table(kff_2020$b8e,useNA = "ifany")

#NOTE: 2020 has 3rd encoded outcome, unsure if they offer HDHP in the dataset but not in the codebook/data dictionary
kff_2020$offers <- ifelse(kff_2020$b8e == 1, 1, 
                        ifelse(kff_2020$b8e == 2, 0,
                               ifelse(kff_2020$b8e == 3, NA, NA)))
kff_2020$doesnt_offer <- ifelse(kff_2020$b8e == 2,1,
                              ifelse(kff_2020$b8e == 1,0,
                                     ifelse(kff_2020$b8e == 3, NA, NA)))

table(kff_2020$b8e, kff_2020$offers, useNA = "ifany")
table(kff_2020$b8e, kff_2020$doesnt_offer, useNA = "ifany")


###2021
table(kff_2021$b8e,useNA = "ifany")

kff_2021$offers <- ifelse(kff_2021$b8e == 1, 1,
                        ifelse(kff_2021$b8e == 2, 0,
                               ifelse(kff_2021$b8e == 3, NA, NA)))
kff_2021$doesnt_offer <- ifelse(kff_2021$b8e == 2,1,
                              ifelse(kff_2021$b8e == 1,0,
                                     ifelse(kff_2021$b8e == 3, NA, NA)))

table(kff_2021$b8e, kff_2021$offers, useNA = "ifany")
table(kff_2021$b8e, kff_2021$doesnt_offer, useNA = "ifany")


###2022
table(kff_2022$b8e,useNA = "ifany")

kff_2022$offers <- ifelse(kff_2022$b8e == 1, 1, 
                        ifelse(kff_2022$b8e == 2, 0,
                               ifelse(kff_2022$b8e == 3, NA, NA)))
kff_2022$doesnt_offer <- ifelse(kff_2022$b8e == 2,1,
                              ifelse(kff_2022$b8e == 1,0,
                                     ifelse(kff_2022$b8e == 3, NA, NA)))

table(kff_2022$b8e, kff_2022$offers, useNA = "ifany")
table(kff_2022$b8e, kff_2022$doesnt_offer, useNA = "ifany")


###2023
table(kff_2023$b8e,useNA = "ifany")

kff_2023$offers <- ifelse(kff_2023$b8e == 1, 1, 
                        ifelse(kff_2023$b8e == 2, 0,
                               ifelse(kff_2023$b8e == 3, NA, NA)))
kff_2023$doesnt_offer <- ifelse(kff_2023$b8e == 2,1,
                              ifelse(kff_2023$b8e == 1,0,
                                     ifelse(kff_2023$b8e == 3, NA, NA)))

table(kff_2023$b8e, kff_2023$offers, useNA = "ifany")
table(kff_2023$b8e, kff_2023$doesnt_offer, useNA = "ifany")

###2024
table(kff_2024$b8e,useNA = "ifany")

kff_2024$offers <- ifelse(kff_2024$b8e == 1, 1,
                        ifelse(kff_2024$b8e == 2, 0,
                               ifelse(kff_2024$b8e == 3, NA, NA)))
kff_2024$doesnt_offer <- ifelse(kff_2024$b8e == 2,1,
                              ifelse(kff_2024$b8e == 1,0,
                                     ifelse(kff_2024$b8e == 3, NA, NA)))

table(kff_2024$b8e, kff_2024$offers, useNA = "ifany")
table(kff_2024$b8e, kff_2024$doesnt_offer, useNA = "ifany")



###### Percent of Workers with Health Benefits Covered in HDHP #####

### 2006
#examine variable by showing summary stats (min, 1st quartile, median, mean, 3rd quartile, max, and number of missing values (NA))
summary(kff_2006$b12e)

kff_2006$percent_hdhp <- kff_2006$b12e #clean data by creating new variable 

#confirm correct cleaning by checking that the new variable matches the original (difference should be 0 for all observations)
kff_2006$test_percent_hdhp <- kff_2006$b12e - kff_2006$percent_hdhp
summary(kff_2006$test_percent_hdhp) #print summary stats

### 2007
# Step 1: Examine variable
summary(kff_2007$b12e)

# Step 2: Clean variable (always create new variable!)
kff_2007$percent_hdhp <- kff_2007$b12e

# step 3: Confirm correct cleaning
kff_2007$test_percent_hdhp <- kff_2007$b12e - kff_2007$percent_hdhp
summary(kff_2007$test_percent_hdhp)

### 2008
summary(kff_2008$b12e)

kff_2008$percent_hdhp <- kff_2008$b12e

kff_2008$test_percent_hdhp <- kff_2008$b12e - kff_2008$percent_hdhp
summary(kff_2008$test_percent_hdhp)

### 2009
summary(kff_2009$b12e)

kff_2009$percent_hdhp <- kff_2009$b12e

kff_2009$test_percent_hdhp <- kff_2009$b12e - kff_2009$percent_hdhp
summary(kff_2009$test_percent_hdhp)

### 2010
summary(kff_2010$b12e)

kff_2010$percent_hdhp <- kff_2010$b12e

kff_2010$test_percent_hdhp <- kff_2010$b12e - kff_2010$percent_hdhp
summary(kff_2010$test_percent_hdhp)

#NOTE:mean increases from 2010 to 2011, from 0.0725 to 0.155

### 2011
summary(kff_2011$b12e)

kff_2011$percent_hdhp <- kff_2011$b12e

kff_2011$test_percent_hdhp <- kff_2011$b12e - kff_2011$percent_hdhp
summary(kff_2011$test_percent_hdhp)

### 2012
summary(kff_2012$b12e)

kff_2012$percent_hdhp <- kff_2012$b12e

kff_2012$test_percent_hdhp <- kff_2012$b12e - kff_2012$percent_hdhp
summary(kff_2012$test_percent_hdhp)


### 2013
# Step 1: Examine variable
summary(kff_2013$b12e)

# Step 2: Clean variable (always create new variable!)
kff_2013$percent_hdhp <- kff_2013$b12e

# step 3: Confirm correct cleaning
kff_2013$test_percent_hdhp <- kff_2013$b12e - kff_2013$percent_hdhp
summary(kff_2013$test_percent_hdhp)


### 2014
# Step 1: Examine variable
summary(kff_2014$b12e)

# Step 2: Clean variable (always create new variable!)
kff_2014$percent_hdhp <- kff_2014$b12e

# step 3: Confirm correct cleaning
kff_2014$test_percent_hdhp <- kff_2014$b12e - kff_2014$percent_hdhp
summary(kff_2014$test_percent_hdhp)

### 2015
summary(kff_2015$b12e)

kff_2015$percent_hdhp <- kff_2015$b12e

kff_2015$test_percent_hdhp <- kff_2015$b12e - kff_2015$percent_hdhp
summary(kff_2015$test_percent_hdhp)

### 2016
summary(kff_2016$b12e)

kff_2016$percent_hdhp <- kff_2016$b12e

kff_2016$test_percent_hdhp <- kff_2016$b12e - kff_2016$percent_hdhp
summary(kff_2016$test_percent_hdhp)

### 2017
summary(kff_2017$b12e)

kff_2017$percent_hdhp <- kff_2017$b12e

kff_2017$test_percent_hdhp <- kff_2017$b12e - kff_2017$percent_hdhp
summary(kff_2017$test_percent_hdhp)

### 2018
summary(kff_2018$b12e)

kff_2018$percent_hdhp <- kff_2018$b12e

kff_2018$test_percent_hdhp <- kff_2018$b12e - kff_2018$percent_hdhp
summary(kff_2018$test_percent_hdhp)

#NOTE: variable are tracked in 2019 and 2020 datasets but not mentioned in codebooks/dictionaries

### 2019
summary(kff_2019$b12e_pct)

kff_2019$percent_hdhp <- kff_2019$b12e_pct

kff_2019$test_percent_hdhp <- kff_2019$b12e_pct - kff_2019$percent_hdhp
summary(kff_2019$test_percent_hdhp)

### 2020
summary(kff_2020$b12e_pct)

kff_2020$percent_hdhp <- kff_2020$b12e_pct

kff_2020$test_percent_hdhp <- kff_2020$b12e_pct - kff_2020$percent_hdhp
summary(kff_2020$test_percent_hdhp)

###2021
summary(kff_2021$b12e_pct)

kff_2021$percent_hdhp <- kff_2021$b12e_pct

kff_2021$test_percent_hdhp <- kff_2021$b12e_pct - kff_2021$percent_hdhp
summary(kff_2021$test_percent_hdhp)

###2022
summary(kff_2022$b12e_pct)

kff_2022$percent_hdhp <- kff_2022$b12e_pct

kff_2022$test_percent_hdhp <- kff_2022$b12e_pct - kff_2022$percent_hdhp
summary(kff_2022$test_percent_hdhp)

###2023
summary(kff_2023$b12e_pct)

kff_2023$percent_hdhp <- kff_2023$b12e_pct

kff_2023$test_percent_hdhp <- kff_2023$b12e_pct - kff_2023$percent_hdhp
summary(kff_2023$test_percent_hdhp)

###2024
summary(kff_2024$b12e_pct)

kff_2024$percent_hdhp <- kff_2024$b12e_pct

kff_2024$test_percent_hdhp <- kff_2024$b12e_pct - kff_2024$percent_hdhp
summary(kff_2024$test_percent_hdhp)



###### Industry #####

### 2003 ###
# Step 1: Examine variable
table(kff_2003$industry, useNA = "ifany")

# Step 2: Clean variable (always create new variable!)
kff_2003$MinConst <- ifelse(kff_2003$industry == 1 | kff_2003$industry == 2, 1, 0)
kff_2003$manufacturing <- ifelse(kff_2003$industry == 3, 1, 0)
kff_2003$transportutilcomms <- ifelse(kff_2003$industry == 4, 1, 0)
kff_2003$wholesale <- ifelse(kff_2003$industry == 5, 1, 0)
kff_2003$retail <- ifelse(kff_2003$industry == 6, 1, 0)
kff_2003$financial <- ifelse(kff_2003$industry == 7, 1, 0)
kff_2003$service <- ifelse(kff_2003$industry == 8, 1, 0)
kff_2003$government <- ifelse(kff_2003$industry == 9, 1, 0)
kff_2003$healthcare <- ifelse(kff_2003$industry == 10, 1, 0)

# step 3: Confirm correct cleaning
table(kff_2003$industry, kff_2003$MinConst, useNA = "ifany")
table(kff_2003$industry, kff_2003$manufacturing, useNA = "ifany")
table(kff_2003$industry, kff_2003$transportutilcomms, useNA = "ifany")
table(kff_2003$industry, kff_2003$wholesale, useNA = "ifany")
table(kff_2003$industry, kff_2003$retail, useNA = "ifany")
table(kff_2003$industry, kff_2003$financial, useNA = "ifany")
table(kff_2003$industry, kff_2003$service, useNA = "ifany")
table(kff_2003$industry, kff_2003$government, useNA = "ifany")
table(kff_2003$industry, kff_2003$healthcare, useNA = "ifany")


### 2004 ###
# Step 1: Examine variable
table(kff_2004$industry, useNA = "ifany")

# Step 2: Clean variable (always create new variable!)
kff_2004$MinConst <- ifelse(kff_2004$industry == 1 | kff_2004$industry == 2, 1, 0)
kff_2004$manufacturing <- ifelse(kff_2004$industry == 3, 1, 0)
kff_2004$transportutilcomms <- ifelse(kff_2004$industry == 4, 1, 0)
kff_2004$wholesale <- ifelse(kff_2004$industry == 5, 1, 0)
kff_2004$retail <- ifelse(kff_2004$industry == 6, 1, 0)
kff_2004$financial <- ifelse(kff_2004$industry == 7, 1, 0)
kff_2004$service <- ifelse(kff_2004$industry == 8, 1, 0)
kff_2004$government <- ifelse(kff_2004$industry == 9, 1, 0)
kff_2004$healthcare <- ifelse(kff_2004$industry == 10, 1, 0)

# step 3: Confirm correct cleaning
table(kff_2004$industry, kff_2004$MinConst, useNA = "ifany")
table(kff_2004$industry, kff_2004$manufacturing, useNA = "ifany")
table(kff_2004$industry, kff_2004$transportutilcomms, useNA = "ifany")
table(kff_2004$industry, kff_2004$wholesale, useNA = "ifany")
table(kff_2004$industry, kff_2004$retail, useNA = "ifany")
table(kff_2004$industry, kff_2004$financial, useNA = "ifany")
table(kff_2004$industry, kff_2004$service, useNA = "ifany")
table(kff_2004$industry, kff_2004$government, useNA = "ifany")
table(kff_2004$industry, kff_2004$healthcare, useNA = "ifany")


### 2005 ###
# Step 1: Examine variable
table(kff_2005$industry, useNA = "ifany")

# Step 2: Clean variable (always create new variable!)
kff_2005$MinConst <- ifelse(kff_2005$industry == 1 | kff_2005$industry == 2, 1, 0)
kff_2005$manufacturing <- ifelse(kff_2005$industry == 3, 1, 0)
kff_2005$transportutilcomms <- ifelse(kff_2005$industry == 4, 1, 0)
kff_2005$wholesale <- ifelse(kff_2005$industry == 5, 1, 0)
kff_2005$retail <- ifelse(kff_2005$industry == 6, 1, 0)
kff_2005$financial <- ifelse(kff_2005$industry == 7, 1, 0)
kff_2005$service <- ifelse(kff_2005$industry == 8, 1, 0)
kff_2005$government <- ifelse(kff_2005$industry == 9, 1, 0)
kff_2005$healthcare <- ifelse(kff_2005$industry == 10, 1, 0)

# step 3: Confirm correct cleaning
table(kff_2005$industry, kff_2005$MinConst, useNA = "ifany")
table(kff_2005$industry, kff_2005$manufacturing, useNA = "ifany")
table(kff_2005$industry, kff_2005$transportutilcomms, useNA = "ifany")
table(kff_2005$industry, kff_2005$wholesale, useNA = "ifany")
table(kff_2005$industry, kff_2005$retail, useNA = "ifany")
table(kff_2005$industry, kff_2005$financial, useNA = "ifany")
table(kff_2005$industry, kff_2005$service, useNA = "ifany")
table(kff_2005$industry, kff_2005$government, useNA = "ifany")
table(kff_2005$industry, kff_2005$healthcare, useNA = "ifany")


### 2006 ###
# Step 1: Examine variable
table(kff_2006$industry, useNA = "ifany")
table(kff_2006$indust2, useNA = "ifany") 
table(kff_2006$industry, kff_2006$indust2, useNA = "ifany")

# Step 2: Clean variable (always create new variable!)
kff_2006$AgriMinConst <- ifelse(kff_2006$indust2 == 1,1,0) #experimental, wholesales counts agriculture?
#kff_2006$MinConst <- ifelse(kff_2006$industry == 1 | kff_2006$industry == 2, 1, 0)
kff_2006$manufacturing <- ifelse(kff_2006$industry == 3, 1, 0)
kff_2006$transportutilcomms <- ifelse(kff_2006$industry == 4, 1, 0)
kff_2006$wholesale <- ifelse(kff_2006$industry == 5, 1, 0)
kff_2006$retail <- ifelse(kff_2006$industry == 6, 1, 0)
kff_2006$financial <- ifelse(kff_2006$industry == 7, 1, 0)
kff_2006$service <- ifelse(kff_2006$industry == 8, 1, 0)
kff_2006$government <- ifelse(kff_2006$industry == 9, 1, 0)
kff_2006$healthcare <- ifelse(kff_2006$industry == 10, 1, 0)

# step 3: Confirm correct cleaning
table(kff_2006$industry, kff_2006$AgriMinConst, useNA = "ifany") #experimental
#table(kff_2006$industry, kff_2006$MinConst, useNA = "ifany")
table(kff_2006$industry, kff_2006$manufacturing, useNA = "ifany")
table(kff_2006$industry, kff_2006$transportutilcomms, useNA = "ifany")
table(kff_2006$industry, kff_2006$wholesale, useNA = "ifany")
table(kff_2006$industry, kff_2006$retail, useNA = "ifany")
table(kff_2006$industry, kff_2006$financial, useNA = "ifany")
table(kff_2006$industry, kff_2006$service, useNA = "ifany")
table(kff_2006$industry, kff_2006$government, useNA = "ifany")
table(kff_2006$industry, kff_2006$healthcare, useNA = "ifany")


### 2007 ###
# Step 1: Examine variable
table(kff_2007$industry, useNA = "ifany")
table(kff_2007$indust2, useNA = "ifany")
table(kff_2007$industry, kff_2007$indust2, useNA = "ifany")

# Step 2: Clean variable (always create new variable!)
kff_2007$AgriMinConst <- ifelse(kff_2007$indust2 == 1,1,0)
kff_2007$manufacturing <- ifelse(kff_2007$industry == 3, 1, 0)
kff_2007$transportutilcomms <- ifelse(kff_2007$industry == 4, 1, 0)
kff_2007$wholesale <- ifelse(kff_2007$industry == 5, 1, 0)
kff_2007$retail <- ifelse(kff_2007$industry == 6, 1, 0)
kff_2007$financial <- ifelse(kff_2007$industry == 7, 1, 0)
kff_2007$service <- ifelse(kff_2007$industry == 8, 1, 0)
kff_2007$government <- ifelse(kff_2007$industry == 9, 1, 0)
kff_2007$healthcare <- ifelse(kff_2007$industry == 10, 1, 0)

# step 3: Confirm correct cleaning
table(kff_2007$industry, kff_2007$AgriMinConst, useNA = "ifany")
table(kff_2007$industry, kff_2007$manufacturing, useNA = "ifany")
table(kff_2007$industry, kff_2007$transportutilcomms, useNA = "ifany")
table(kff_2007$industry, kff_2007$wholesale, useNA = "ifany")
table(kff_2007$industry, kff_2007$retail, useNA = "ifany")
table(kff_2007$industry, kff_2007$financial, useNA = "ifany")
table(kff_2007$industry, kff_2007$service, useNA = "ifany")
table(kff_2007$industry, kff_2007$government, useNA = "ifany")
table(kff_2007$industry, kff_2007$healthcare, useNA = "ifany")


### 2008 ###
# Step 1: Examine variable
table(kff_2008$industry, useNA = "ifany")
table(kff_2008$indust2, useNA = "ifany")
table(kff_2008$industry, kff_2008$indust2, useNA = "ifany")

# Step 2: Clean variable (always create new variable!)
kff_2008$AgriMinConst <- ifelse(kff_2008$indust2 == 1,1,0)
kff_2008$manufacturing <- ifelse(kff_2008$industry == 3, 1, 0)
kff_2008$transportutilcomms <- ifelse(kff_2008$industry == 4, 1, 0)
kff_2008$wholesale <- ifelse(kff_2008$industry == 5, 1, 0)
kff_2008$retail <- ifelse(kff_2008$industry == 6, 1, 0)
kff_2008$financial <- ifelse(kff_2008$industry == 7, 1, 0)
kff_2008$service <- ifelse(kff_2008$industry == 8, 1, 0)
kff_2008$government <- ifelse(kff_2008$industry == 9, 1, 0)
kff_2008$healthcare <- ifelse(kff_2008$industry == 10, 1, 0)

# step 3: Confirm correct cleaning
table(kff_2008$industry, kff_2008$AgriMinConst, useNA = "ifany")
table(kff_2008$industry, kff_2008$manufacturing, useNA = "ifany")
table(kff_2008$industry, kff_2008$transportutilcomms, useNA = "ifany")
table(kff_2008$industry, kff_2008$wholesale, useNA = "ifany")
table(kff_2008$industry, kff_2008$retail, useNA = "ifany")
table(kff_2008$industry, kff_2008$financial, useNA = "ifany")
table(kff_2008$industry, kff_2008$service, useNA = "ifany")
table(kff_2008$industry, kff_2008$government, useNA = "ifany")
table(kff_2008$industry, kff_2008$healthcare, useNA = "ifany")


### 2009 ###
# Step 1: Examine variable
table(kff_2009$industry, useNA = "ifany")
table(kff_2009$indust2, useNA = "ifany")
table(kff_2009$industry, kff_2009$indust2, useNA = "ifany")

# Step 2: Clean variable (always create new variable!)
kff_2009$AgriMinConst <- ifelse(kff_2009$indust2 == 1,1,0)
kff_2009$manufacturing <- ifelse(kff_2009$industry == 3, 1, 0)
kff_2009$transportutilcomms <- ifelse(kff_2009$industry == 4, 1, 0)
kff_2009$wholesale <- ifelse(kff_2009$industry == 5, 1, 0)
kff_2009$retail <- ifelse(kff_2009$industry == 6, 1, 0)
kff_2009$financial <- ifelse(kff_2009$industry == 7, 1, 0)
kff_2009$service <- ifelse(kff_2009$industry == 8, 1, 0)
kff_2009$government <- ifelse(kff_2009$industry == 9, 1, 0)
kff_2009$healthcare <- ifelse(kff_2009$industry == 10, 1, 0)

# step 3: Confirm correct cleaning
table(kff_2009$industry, kff_2009$AgriMinConst, useNA = "ifany")
table(kff_2009$industry, kff_2009$manufacturing, useNA = "ifany")
table(kff_2009$industry, kff_2009$transportutilcomms, useNA = "ifany")
table(kff_2009$industry, kff_2009$wholesale, useNA = "ifany")
table(kff_2009$industry, kff_2009$retail, useNA = "ifany")
table(kff_2009$industry, kff_2009$financial, useNA = "ifany")
table(kff_2009$industry, kff_2009$service, useNA = "ifany")
table(kff_2009$industry, kff_2009$government, useNA = "ifany")
table(kff_2009$industry, kff_2009$healthcare, useNA = "ifany")


### 2010 ###
# Step 1: Examine variable
table(kff_2010$industry, useNA = "ifany")
table(kff_2010$indust2, useNA = "ifany")
table(kff_2010$industry, kff_2010$indust2, useNA = "ifany")

# Step 2: Clean variable (always create new variable!)
kff_2010$AgriMinConst <- ifelse(kff_2010$indust2 == 1,1,0)
kff_2010$manufacturing <- ifelse(kff_2010$industry == 3, 1, 0)
kff_2010$transportutilcomms <- ifelse(kff_2010$industry == 4, 1, 0)
kff_2010$wholesale <- ifelse(kff_2010$industry == 5, 1, 0)
kff_2010$retail <- ifelse(kff_2010$industry == 6, 1, 0)
kff_2010$financial <- ifelse(kff_2010$industry == 7, 1, 0)
kff_2010$service <- ifelse(kff_2010$industry == 8, 1, 0)
kff_2010$government <- ifelse(kff_2010$industry == 9, 1, 0)
kff_2010$healthcare <- ifelse(kff_2010$industry == 10, 1, 0)

# step 3: Confirm correct cleaning
table(kff_2010$industry, kff_2010$AgriMinConst, useNA = "ifany")
table(kff_2010$industry, kff_2010$manufacturing, useNA = "ifany")
table(kff_2010$industry, kff_2010$transportutilcomms, useNA = "ifany")
table(kff_2010$industry, kff_2010$wholesale, useNA = "ifany")
table(kff_2010$industry, kff_2010$retail, useNA = "ifany")
table(kff_2010$industry, kff_2010$financial, useNA = "ifany")
table(kff_2010$industry, kff_2010$service, useNA = "ifany")
table(kff_2010$industry, kff_2010$government, useNA = "ifany")
table(kff_2010$industry, kff_2010$healthcare, useNA = "ifany")


## 2011 ##
# Step 1: Examine variable
table(kff_2011$industry, useNA = "ifany")
table(kff_2011$indust2, useNA = "ifany")
table(kff_2011$industry, kff_2011$indust2, useNA = "ifany")

# Step 2: Clean variable (always create new variable!)
kff_2011$AgriMinConst <- ifelse(kff_2011$indust2 == 1,1,0)
kff_2011$manufacturing <- ifelse(kff_2011$industry == 3, 1, 0)
kff_2011$transportutilcomms <- ifelse(kff_2011$industry == 4, 1, 0)
kff_2011$wholesale <- ifelse(kff_2011$industry == 5, 1, 0)
kff_2011$retail <- ifelse(kff_2011$industry == 6, 1, 0)
kff_2011$financial <- ifelse(kff_2011$industry == 7, 1, 0)
kff_2011$service <- ifelse(kff_2011$industry == 8, 1, 0)
kff_2011$government <- ifelse(kff_2011$industry == 9, 1, 0)
kff_2011$healthcare <- ifelse(kff_2011$industry == 10, 1, 0)

# step 3: Confirm correct cleaning
table(kff_2011$industry, kff_2011$AgriMinConst, useNA = "ifany")
table(kff_2011$industry, kff_2011$manufacturing, useNA = "ifany")
table(kff_2011$industry, kff_2011$transportutilcomms, useNA = "ifany")
table(kff_2011$industry, kff_2011$wholesale, useNA = "ifany")
table(kff_2011$industry, kff_2011$retail, useNA = "ifany")
table(kff_2011$industry, kff_2011$financial, useNA = "ifany")
table(kff_2011$industry, kff_2011$service, useNA = "ifany")
table(kff_2011$industry, kff_2011$government, useNA = "ifany")
table(kff_2011$industry, kff_2011$healthcare, useNA = "ifany")


### 2012 ###
# Step 1: Examine variable
table(kff_2012$industry, useNA = "ifany")

# Step 2: Clean variable (always create new variable!)
kff_2012$AgriMinConst<- ifelse(kff_2012$industry == 2, 1, 0)
kff_2012$manufacturing <- ifelse(kff_2012$industry == 3, 1, 0)
kff_2012$transportutilcomms <- ifelse(kff_2012$industry == 4, 1, 0)
kff_2012$wholesale <- ifelse(kff_2012$industry == 5, 1, 0)
kff_2012$retail <- ifelse(kff_2012$industry == 6, 1, 0)
kff_2012$financial <- ifelse(kff_2012$industry == 7, 1, 0)
kff_2012$service <- ifelse(kff_2012$industry == 8, 1, 0)
kff_2012$government <- ifelse(kff_2012$industry == 9, 1, 0)
kff_2012$healthcare <- ifelse(kff_2012$industry == 10, 1, 0)

# step 3: Confirm correct cleaning
table(kff_2012$industry, kff_2012$AgriMinConst, useNA = "ifany")
table(kff_2012$industry, kff_2012$manufacturing, useNA = "ifany")
table(kff_2012$industry, kff_2012$transportutilcomms, useNA = "ifany")
table(kff_2012$industry, kff_2012$wholesale, useNA = "ifany")
table(kff_2012$industry, kff_2012$retail, useNA = "ifany")
table(kff_2012$industry, kff_2012$financial, useNA = "ifany")
table(kff_2012$industry, kff_2012$service, useNA = "ifany")
table(kff_2012$industry, kff_2012$government, useNA = "ifany")
table(kff_2012$industry, kff_2012$healthcare, useNA = "ifany")


### 2013 ###
# Step 1: Examine variable
table(kff_2013$industry, useNA = "ifany")

# Step 2: Clean variable (always create new variable!)
kff_2013$AgriMinConst<- ifelse(kff_2013$industry == 2, 1, 0)
kff_2013$manufacturing <- ifelse(kff_2013$industry == 3, 1, 0)
kff_2013$transportutilcomms <- ifelse(kff_2013$industry == 4, 1, 0)
kff_2013$wholesale <- ifelse(kff_2013$industry == 5, 1, 0)
kff_2013$retail <- ifelse(kff_2013$industry == 6, 1, 0)
kff_2013$financial <- ifelse(kff_2013$industry == 7, 1, 0)
kff_2013$service <- ifelse(kff_2013$industry == 8, 1, 0)
kff_2013$government <- ifelse(kff_2013$industry == 9, 1, 0)
kff_2013$healthcare <- ifelse(kff_2013$industry == 10, 1, 0)

# step 3: Confirm correct cleaning
table(kff_2013$industry, kff_2013$AgriMinConst, useNA = "ifany")
table(kff_2013$industry, kff_2013$manufacturing, useNA = "ifany")
table(kff_2013$industry, kff_2013$transportutilcomms, useNA = "ifany")
table(kff_2013$industry, kff_2013$wholesale, useNA = "ifany")
table(kff_2013$industry, kff_2013$retail, useNA = "ifany")
table(kff_2013$industry, kff_2013$financial, useNA = "ifany")
table(kff_2013$industry, kff_2013$service, useNA = "ifany")
table(kff_2013$industry, kff_2013$government, useNA = "ifany")
table(kff_2013$industry, kff_2013$healthcare, useNA = "ifany")


## 2014 ##
# Step 1: Examine variable
table(kff_2014$industry, useNA = "ifany")

# Step 2: Clean variable (always create new variable!)
kff_2014$AgriMinConst<- ifelse(kff_2014$industry == 2, 1, 0)
kff_2014$manufacturing   <- ifelse(kff_2014$industry == 3,  1, 0)
kff_2014$transportutilcomms  <- ifelse(kff_2014$industry == 4,  1, 0)
kff_2014$wholesale       <- ifelse(kff_2014$industry == 5,  1, 0)
kff_2014$retail          <- ifelse(kff_2014$industry == 6,  1, 0)
kff_2014$financial       <- ifelse(kff_2014$industry == 7,  1, 0)
kff_2014$service         <- ifelse(kff_2014$industry == 8,  1, 0)
kff_2014$government      <- ifelse(kff_2014$industry == 9,  1, 0)
kff_2014$healthcare      <- ifelse(kff_2014$industry == 10, 1, 0)
  
# step 3: Confirm correct cleaning
table(kff_2014$industry, kff_2014$AgriMinConst, useNA = "ifany")
table(kff_2014$industry, kff_2014$manufacturing, useNA = "ifany")
table(kff_2014$industry, kff_2014$transportutilcomms, useNA = "ifany")
table(kff_2014$industry, kff_2014$wholesale, useNA = "ifany")
table(kff_2014$industry, kff_2014$retail, useNA = "ifany")
table(kff_2014$industry, kff_2014$financial, useNA = "ifany")
table(kff_2014$industry, kff_2014$service, useNA = "ifany")
table(kff_2014$industry, kff_2014$government, useNA = "ifany")
table(kff_2014$industry, kff_2014$healthcare, useNA = "ifany")

### 2015 ###
table(kff_2015$industry, useNA = "ifany")

kff_2015$AgriMinConst<- ifelse(kff_2015$industry == 2, 1, 0)
kff_2015$manufacturing   <- ifelse(kff_2015$industry == 3,  1, 0)
kff_2015$transportutilcomms  <- ifelse(kff_2015$industry == 4,  1, 0)
kff_2015$wholesale       <- ifelse(kff_2015$industry == 5,  1, 0)
kff_2015$retail          <- ifelse(kff_2015$industry == 6,  1, 0)
kff_2015$financial       <- ifelse(kff_2015$industry == 7,  1, 0)
kff_2015$service         <- ifelse(kff_2015$industry == 8,  1, 0)
kff_2015$government      <- ifelse(kff_2015$industry == 9,  1, 0)
kff_2015$healthcare      <- ifelse(kff_2015$industry == 10, 1, 0)

table(kff_2015$industry, kff_2015$AgriMinConst, useNA = "ifany")
table(kff_2015$industry, kff_2015$manufacturing, useNA = "ifany")
table(kff_2015$industry, kff_2015$transportutilcomms, useNA = "ifany")
table(kff_2015$industry, kff_2015$wholesale, useNA = "ifany")
table(kff_2015$industry, kff_2015$retail, useNA = "ifany")
table(kff_2015$industry, kff_2015$financial, useNA = "ifany")
table(kff_2015$industry, kff_2015$service, useNA = "ifany")
table(kff_2015$industry, kff_2015$government, useNA = "ifany")
table(kff_2015$industry, kff_2015$healthcare, useNA = "ifany")

### 2016 ###
table(kff_2016$industry, useNA = "ifany")

kff_2016$AgriMinConst<- ifelse(kff_2016$industry == 2, 1, 0)
kff_2016$manufacturing   <- ifelse(kff_2016$industry == 3,  1, 0)
kff_2016$transportutilcomms  <- ifelse(kff_2016$industry == 4,  1, 0)
kff_2016$wholesale       <- ifelse(kff_2016$industry == 5,  1, 0)
kff_2016$retail          <- ifelse(kff_2016$industry == 6,  1, 0)
kff_2016$financial       <- ifelse(kff_2016$industry == 7,  1, 0)
kff_2016$service         <- ifelse(kff_2016$industry == 8,  1, 0)
kff_2016$government      <- ifelse(kff_2016$industry == 9,  1, 0)
kff_2016$healthcare      <- ifelse(kff_2016$industry == 10, 1, 0)

table(kff_2016$industry, kff_2016$AgriMinConst, useNA = "ifany")
table(kff_2016$industry, kff_2016$manufacturing, useNA = "ifany")
table(kff_2016$industry, kff_2016$transportutilcomms, useNA = "ifany")
table(kff_2016$industry, kff_2016$wholesale, useNA = "ifany")
table(kff_2016$industry, kff_2016$retail, useNA = "ifany")
table(kff_2016$industry, kff_2016$financial, useNA = "ifany")
table(kff_2016$industry, kff_2016$service, useNA = "ifany")
table(kff_2016$industry, kff_2016$government, useNA = "ifany")
table(kff_2016$industry, kff_2016$healthcare, useNA = "ifany")


### 2017 ###
table(kff_2017$industry, useNA = "ifany")

kff_2017$AgriMinConst<- ifelse(kff_2017$industry == 2, 1, 0)
kff_2017$manufacturing   <- ifelse(kff_2017$industry == 3,  1, 0)
kff_2017$transportutilcomms  <- ifelse(kff_2017$industry == 4,  1, 0)
kff_2017$wholesale       <- ifelse(kff_2017$industry == 5,  1, 0)
kff_2017$retail          <- ifelse(kff_2017$industry == 6,  1, 0)
kff_2017$financial       <- ifelse(kff_2017$industry == 7,  1, 0)
kff_2017$service         <- ifelse(kff_2017$industry == 8,  1, 0)
kff_2017$government      <- ifelse(kff_2017$industry == 9,  1, 0)
kff_2017$healthcare      <- ifelse(kff_2017$industry == 10, 1, 0)

table(kff_2017$industry, kff_2017$AgriMinConst, useNA = "ifany")
table(kff_2017$industry, kff_2017$manufacturing, useNA = "ifany")
table(kff_2017$industry, kff_2017$transportutilcomms, useNA = "ifany")
table(kff_2017$industry, kff_2017$wholesale, useNA = "ifany")
table(kff_2017$industry, kff_2017$retail, useNA = "ifany")
table(kff_2017$industry, kff_2017$financial, useNA = "ifany")
table(kff_2017$industry, kff_2017$service, useNA = "ifany")
table(kff_2017$industry, kff_2017$government, useNA = "ifany")
table(kff_2017$industry, kff_2017$healthcare, useNA = "ifany")

### 2018 ###
table(kff_2018$industry, useNA = "ifany")

kff_2018$AgriMinConst<- ifelse(kff_2018$industry == 2, 1, 0)
kff_2018$manufacturing   <- ifelse(kff_2018$industry == 3,  1, 0)
kff_2018$transportutilcomms  <- ifelse(kff_2018$industry == 4,  1, 0)
kff_2018$wholesale       <- ifelse(kff_2018$industry == 5,  1, 0)
kff_2018$retail          <- ifelse(kff_2018$industry == 6,  1, 0)
kff_2018$financial       <- ifelse(kff_2018$industry == 7,  1, 0)
kff_2018$service         <- ifelse(kff_2018$industry == 8,  1, 0)
kff_2018$government      <- ifelse(kff_2018$industry == 9,  1, 0)
kff_2018$healthcare      <- ifelse(kff_2018$industry == 10, 1, 0)
                                   
table(kff_2018$industry, kff_2018$AgriMinConst, useNA = "ifany")
table(kff_2018$industry, kff_2018$manufacturing, useNA = "ifany")
table(kff_2018$industry, kff_2018$transportutilcomms, useNA = "ifany")
table(kff_2018$industry, kff_2018$wholesale, useNA = "ifany")
table(kff_2018$industry, kff_2018$retail, useNA = "ifany")
table(kff_2018$industry, kff_2018$financial, useNA = "ifany")
table(kff_2018$industry, kff_2018$service, useNA = "ifany")
table(kff_2018$industry, kff_2018$government, useNA = "ifany")
table(kff_2018$industry, kff_2018$healthcare, useNA = "ifany")

### 2019 ###
table(kff_2019$industry, useNA = "ifany")  

#10 (finance) not mentioned in codebook but is in dataset

kff_2019$AgriMinConst<- ifelse(kff_2019$industry == 2, 1, 0)
kff_2019$manufacturing   <- ifelse(kff_2019$industry == 3,  1, 0)
kff_2019$transportutilcomms  <- ifelse(kff_2019$industry == 4,  1, 0)
kff_2019$wholesale       <- ifelse(kff_2019$industry == 5,  1, 0)
kff_2019$retail          <- ifelse(kff_2019$industry == 6,  1, 0)
kff_2019$financial       <- ifelse(kff_2019$industry == 7,  1, 0)
kff_2019$service         <- ifelse(kff_2019$industry == 8,  1, 0)
kff_2019$government      <- ifelse(kff_2019$industry == 9,  1, 0)
kff_2019$healthcare      <- ifelse(kff_2019$industry == 10, 1, 0)

table(kff_2019$industry, kff_2019$AgriMinConst, useNA = "ifany")
table(kff_2019$industry, kff_2019$manufacturing, useNA = "ifany")
table(kff_2019$industry, kff_2019$transportutilcomms, useNA = "ifany")
table(kff_2019$industry, kff_2019$wholesale, useNA = "ifany")
table(kff_2019$industry, kff_2019$retail, useNA = "ifany")
table(kff_2019$industry, kff_2019$financial, useNA = "ifany")
table(kff_2019$industry, kff_2019$service, useNA = "ifany")
table(kff_2019$industry, kff_2019$government, useNA = "ifany")
table(kff_2019$industry, kff_2019$healthcare, useNA = "ifany")

### 2020 ###
table(kff_2020$industry, useNA = "ifany")
#10 (finance) not mentioned in codebook but is in dataset

kff_2020$AgriMinConst<- ifelse(kff_2020$industry == 2, 1, 0)
kff_2020$manufacturing   <- ifelse(kff_2020$industry == 3,  1, 0)
kff_2020$transportutilcomms  <- ifelse(kff_2020$industry == 4,  1, 0)
kff_2020$wholesale       <- ifelse(kff_2020$industry == 5,  1, 0)
kff_2020$retail          <- ifelse(kff_2020$industry == 6,  1, 0)
kff_2020$financial       <- ifelse(kff_2020$industry == 7,  1, 0)
kff_2020$service         <- ifelse(kff_2020$industry == 8,  1, 0)
kff_2020$government      <- ifelse(kff_2020$industry == 9,  1, 0)
kff_2020$healthcare      <- ifelse(kff_2020$industry == 10, 1, 0)

table(kff_2020$industry, kff_2020$AgriMinConst, useNA = "ifany")
table(kff_2020$industry, kff_2020$manufacturing, useNA = "ifany")
table(kff_2020$industry, kff_2020$transportutilcomms, useNA = "ifany")
table(kff_2020$industry, kff_2020$wholesale, useNA = "ifany")
table(kff_2020$industry, kff_2020$retail, useNA = "ifany")
table(kff_2020$industry, kff_2020$financial, useNA = "ifany")
table(kff_2020$industry, kff_2020$service, useNA = "ifany")
table(kff_2020$industry, kff_2020$government, useNA = "ifany")
table(kff_2020$industry, kff_2020$healthcare, useNA = "ifany")



### 2021 ###
table(kff_2021$industry, useNA = "ifany")
#10 (finance) not mentioned in codebook but is in dataset

kff_2021$AgriMinConst<- ifelse(kff_2021$industry == 2, 1, 0)
kff_2021$manufacturing   <- ifelse(kff_2021$industry == 3,  1, 0)
kff_2021$transportutilcomms  <- ifelse(kff_2021$industry == 4,  1, 0)
kff_2021$wholesale       <- ifelse(kff_2021$industry == 5,  1, 0)
kff_2021$retail          <- ifelse(kff_2021$industry == 6,  1, 0)
kff_2021$financial       <- ifelse(kff_2021$industry == 7,  1, 0)
kff_2021$service         <- ifelse(kff_2021$industry == 8,  1, 0)
kff_2021$government      <- ifelse(kff_2021$industry == 9,  1, 0)
kff_2021$healthcare      <- ifelse(kff_2021$industry == 10, 1, 0)

table(kff_2021$industry, kff_2021$AgriMinConst, useNA = "ifany")
table(kff_2021$industry, kff_2021$manufacturing, useNA = "ifany")
table(kff_2021$industry, kff_2021$transportutilcomms, useNA = "ifany")
table(kff_2021$industry, kff_2021$wholesale, useNA = "ifany")
table(kff_2021$industry, kff_2021$retail, useNA = "ifany")
table(kff_2021$industry, kff_2021$financial, useNA = "ifany")
table(kff_2021$industry, kff_2021$service, useNA = "ifany")
table(kff_2021$industry, kff_2021$government, useNA = "ifany")
table(kff_2021$industry, kff_2021$healthcare, useNA = "ifany")


### 2022 ###
table(kff_2022$industry, useNA = "ifany")

kff_2022$AgriMinConst<- ifelse(kff_2022$industry == 2, 1, 0)
kff_2022$manufacturing   <- ifelse(kff_2022$industry == 3,  1, 0)
kff_2022$transportutilcomms  <- ifelse(kff_2022$industry == 4,  1, 0)
kff_2022$wholesale       <- ifelse(kff_2022$industry == 5,  1, 0)
kff_2022$retail          <- ifelse(kff_2022$industry == 6,  1, 0)
kff_2022$financial       <- ifelse(kff_2022$industry == 7,  1, 0)
kff_2022$service         <- ifelse(kff_2022$industry == 8,  1, 0)
kff_2022$government      <- ifelse(kff_2022$industry == 9,  1, 0)
kff_2022$healthcare      <- ifelse(kff_2022$industry == 10, 1, 0)

table(kff_2022$industry, kff_2022$AgriMinConst, useNA = "ifany")
table(kff_2022$industry, kff_2022$manufacturing, useNA = "ifany")
table(kff_2022$industry, kff_2022$transportutilcomms, useNA = "ifany")
table(kff_2022$industry, kff_2022$wholesale, useNA = "ifany")
table(kff_2022$industry, kff_2022$retail, useNA = "ifany")
table(kff_2022$industry, kff_2022$financial, useNA = "ifany")
table(kff_2022$industry, kff_2022$service, useNA = "ifany")
table(kff_2022$industry, kff_2022$government, useNA = "ifany")
table(kff_2022$industry, kff_2022$healthcare, useNA = "ifany")

### 2023 ###
table(kff_2023$industry, useNA = "ifany")

kff_2023$AgriMinConst<- ifelse(kff_2023$industry == 2, 1, 0)
kff_2023$manufacturing   <- ifelse(kff_2023$industry == 3,  1, 0)
kff_2023$transportutilcomms  <- ifelse(kff_2023$industry == 4,  1, 0)
kff_2023$wholesale       <- ifelse(kff_2023$industry == 5,  1, 0)
kff_2023$retail          <- ifelse(kff_2023$industry == 6,  1, 0)
kff_2023$financial       <- ifelse(kff_2023$industry == 7,  1, 0)
kff_2023$service         <- ifelse(kff_2023$industry == 8,  1, 0)
kff_2023$government      <- ifelse(kff_2023$industry == 9,  1, 0)
kff_2023$healthcare      <- ifelse(kff_2023$industry == 10, 1, 0)

table(kff_2023$industry, kff_2023$AgriMinConst, useNA = "ifany")
table(kff_2023$industry, kff_2023$manufacturing, useNA = "ifany")
table(kff_2023$industry, kff_2023$transportutilcomms, useNA = "ifany")
table(kff_2023$industry, kff_2023$wholesale, useNA = "ifany")
table(kff_2023$industry, kff_2023$retail, useNA = "ifany")
table(kff_2023$industry, kff_2023$financial, useNA = "ifany")
table(kff_2023$industry, kff_2023$service, useNA = "ifany")
table(kff_2023$industry, kff_2023$government, useNA = "ifany")
table(kff_2023$industry, kff_2023$healthcare, useNA = "ifany")

### 2024 ###
table(kff_2024$industry, useNA = "ifany")

kff_2024$AgriMinConst<- ifelse(kff_2024$industry == 2, 1, 0)
kff_2024$manufacturing   <- ifelse(kff_2024$industry == 3,  1, 0)
kff_2024$transportutilcomms  <- ifelse(kff_2024$industry == 4,  1, 0)
kff_2024$wholesale       <- ifelse(kff_2024$industry == 5,  1, 0)
kff_2024$retail          <- ifelse(kff_2024$industry == 6,  1, 0)
kff_2024$financial       <- ifelse(kff_2024$industry == 7,  1, 0)
kff_2024$service         <- ifelse(kff_2024$industry == 8,  1, 0)
kff_2024$government      <- ifelse(kff_2024$industry == 9,  1, 0)
kff_2024$healthcare      <- ifelse(kff_2024$industry == 10, 1, 0)

table(kff_2024$industry, kff_2024$AgriMinConst, useNA = "ifany")
table(kff_2024$industry, kff_2024$manufacturing, useNA = "ifany")
table(kff_2024$industry, kff_2024$transportutilcomms, useNA = "ifany")
table(kff_2024$industry, kff_2024$wholesale, useNA = "ifany")
table(kff_2024$industry, kff_2024$retail, useNA = "ifany")
table(kff_2024$industry, kff_2024$financial, useNA = "ifany")
table(kff_2024$industry, kff_2024$service, useNA = "ifany")
table(kff_2024$industry, kff_2024$government, useNA = "ifany")
table(kff_2024$industry, kff_2024$healthcare, useNA = "ifany")


                                     
                                     
###### Categorized Percent of Workforce Earning $20,000 or Less #####
### 2003
table(kff_2003$loincome, useNA = "ifany") #examine variable

#cleaning variable
kff_2003$mostly_low_wage  <- ifelse(kff_2003$loincome == 2, 1, 0) #35% or more earn $20k or less per year
kff_2003$some_low_wage    <- ifelse(kff_2003$loincome == 1, 1, 0) #Less than 35% earn $20k or less per year

#confirm cleaning
table(kff_2003$loincome, kff_2003$mostly_low_wage, useNA = "ifany")
table(kff_2003$loincome, kff_2003$some_low_wage, useNA = "ifany")


### 2004
table(kff_2004$loincome,useNA = "ifany") #examine variable

kff_2004$mostly_low_wage  <- ifelse(kff_2004$loincome == 2, 1, 0) #35% or more earn $20k or less per year
kff_2004$some_low_wage    <- ifelse(kff_2004$loincome == 1, 1, 0) #Less than 35% earn $20k or less per year

table(kff_2004$loincome, kff_2004$mostly_low_wage, useNA = "ifany")
table(kff_2004$loincome, kff_2004$some_low_wage, useNA = "ifany")

#### NOTE: SOME FIRMS OVER 100%! DROP DROP DROP!!!

### 2005
table(kff_2005$loincome,useNA = "ifany") #examine variable

kff_2005$mostly_low_wage  <- ifelse(kff_2005$loincome == 2, 1, 0) #35% or more earn $20k or less per year
kff_2005$some_low_wage    <- ifelse(kff_2005$loincome == 1, 1, 0) #Less than 35% earn $20k or less per year

table(kff_2005$loincome, kff_2005$mostly_low_wage, useNA = "ifany")
table(kff_2005$loincome, kff_2005$some_low_wage, useNA = "ifany")


### 2006
table(kff_2006$loincome,useNA = "ifany") #examine variable

kff_2006$mostly_low_wage  <- ifelse(kff_2006$loincome == 2, 1, 0) #35% or more earn $20k or less per year
kff_2006$some_low_wage    <- ifelse(kff_2006$loincome == 1, 1, 0) #Less than 35% earn $20k or less per year

table(kff_2006$loincome, kff_2006$mostly_low_wage, useNA = "ifany")
table(kff_2006$loincome, kff_2006$some_low_wage, useNA = "ifany")


### 2007
table(kff_2007$loincome,useNA = "ifany") #examine variable

kff_2007$mostly_low_wage  <- ifelse(kff_2007$loincome == 2, 1, 0) #35% or more earn $21k or less per year
kff_2007$some_low_wage    <- ifelse(kff_2007$loincome == 1, 1, 0) #Less than 35% earn $21k or less per year

table(kff_2007$loincome, kff_2007$mostly_low_wage, useNA = "ifany")
table(kff_2007$loincome, kff_2007$some_low_wage, useNA = "ifany")


### 2008
table(kff_2008$loincome,useNA = "ifany") #examine variable

kff_2008$mostly_low_wage  <- ifelse(kff_2008$loincome == 2, 1, 0) #35% or more earn $22k or less per year
kff_2008$some_low_wage    <- ifelse(kff_2008$loincome == 1, 1, 0) #Less than 35% earn $22k or less per year

table(kff_2008$loincome, kff_2008$mostly_low_wage, useNA = "ifany")
table(kff_2008$loincome, kff_2008$some_low_wage, useNA = "ifany")


### 2009
table(kff_2009$loincome,useNA = "ifany") #examine variable

kff_2009$mostly_low_wage  <- ifelse(kff_2009$loincome == 2, 1, 0) #35% or more earn $21k or less per year
kff_2009$some_low_wage    <- ifelse(kff_2009$loincome == 1, 1, 0) #Less than 35% earn $21k or less per year

table(kff_2009$loincome, kff_2009$mostly_low_wage, useNA = "ifany")
table(kff_2009$loincome, kff_2009$some_low_wage, useNA = "ifany")


### 2010
table(kff_2010$loincome,useNA = "ifany") #examine variable

kff_2010$mostly_low_wage  <- ifelse(kff_2010$loincome == 2, 1, 0) #35% or more earn $23k or less per year
kff_2010$some_low_wage    <- ifelse(kff_2010$loincome == 1, 1, 0) #Less than 35% earn $23k or less per year

table(kff_2010$loincome, kff_2010$mostly_low_wage, useNA = "ifany")
table(kff_2010$loincome, kff_2010$some_low_wage, useNA = "ifany")


### 2011
table(kff_2011$loincome,useNA = "ifany") #examine variable

kff_2011$mostly_low_wage  <- ifelse(kff_2011$loincome == 2, 1, 0) #35% or more earn $23k or less per year
kff_2011$some_low_wage    <- ifelse(kff_2011$loincome == 1, 1, 0) #Less than 35% earn $23k or less per year

table(kff_2011$loincome, kff_2011$mostly_low_wage, useNA = "ifany")
table(kff_2011$loincome, kff_2011$some_low_wage, useNA = "ifany")


### 2012
table(kff_2012$loincome,useNA = "ifany") #examine

kff_2012$mostly_low_wage  <- ifelse(kff_2012$loincome == 2, 1, 0) #35% or more earn $24k or less per year
kff_2012$some_low_wage    <- ifelse(kff_2012$loincome == 1, 1, 0) #Less than 35% earn $24k or less per year

table(kff_2012$loincome, kff_2012$mostly_low_wage, useNA = "ifany")
table(kff_2012$loincome, kff_2012$some_low_wage, useNA = "ifany")


### 2013
table(kff_2013$loincome,useNA = "ifany") #examine

kff_2013$mostly_low_wage  <- ifelse(kff_2013$loincome == 2, 1, 0) #35% or more earn $24k or less per year
kff_2013$some_low_wage    <- ifelse(kff_2013$loincome == 1, 1, 0) #Less than 35% earn $24k or less per year

table(kff_2013$loincome, kff_2013$mostly_low_wage, useNA = "ifany")
table(kff_2013$loincome, kff_2013$some_low_wage, useNA = "ifany")



###### Categorized Percent of Workforce With High Incomes #####
### 2007
table(kff_2007$hiincome, useNA = "ifany") #examine variable

kff_2007$mostly_high_wage <- ifelse(kff_2007$hiincome == 2, 1, 0) #35% or more earn $50k or more per year
kff_2007$some_high_wage   <- ifelse(kff_2007$hiincome == 1, 1, 0) #Less than 35% earn $50k or more per year

table(kff_2007$hiincome, kff_2007$mostly_high_wage, useNA = "ifany")
table(kff_2007$hiincome, kff_2007$some_high_wage, useNA = "ifany")


### 2008
table(kff_2008$hiincome, useNA = "ifany") #examine variable

kff_2008$mostly_high_wage <- ifelse(kff_2008$hiincome == 2, 1, 0) #35% or more earn $50k or more per year
kff_2008$some_high_wage   <- ifelse(kff_2008$hiincome == 1, 1, 0) #Less than 35% earn $50k or more per year

table(kff_2008$hiincome, kff_2008$mostly_high_wage, useNA = "ifany")
table(kff_2008$hiincome, kff_2008$some_high_wage, useNA = "ifany")

### NOTE: 2009-2011,hiincome variable is missing/no measured


### 2012
table(kff_2012$hiincome, useNA = "ifany") #examine variable

kff_2012$mostly_high_wage <- ifelse(kff_2012$hiincome == 2, 1, 0) #35% or more earn $55k or more per year
kff_2012$some_high_wage   <- ifelse(kff_2012$hiincome == 1, 1, 0) #Less than 35% earn $55k or more per year

table(kff_2012$hiincome, kff_2012$mostly_high_wage, useNA = "ifany")
table(kff_2012$hiincome, kff_2012$some_high_wage, useNA = "ifany")


### 2013
table(kff_2013$hiincome, useNA = "ifany") #examine variable

kff_2013$mostly_high_wage <- ifelse(kff_2013$hiincome == 2, 1, 0) #35% or more earn $55k or more per year
kff_2013$some_high_wage   <- ifelse(kff_2013$hiincome == 1, 1, 0) #Less than 35% earn $55k or more per year

table(kff_2013$hiincome, kff_2013$mostly_high_wage, useNA = "ifany")
table(kff_2013$hiincome, kff_2013$some_high_wage, useNA = "ifany")




###### Categorized Percent of Workforce Age 26 or Younger #####
### 2007
table(kff_2007$age26, useNA = "ifany") #examine variable

kff_2007$mostly_young_workers <- ifelse(kff_2007$age26 == 2, 1, 0) #35% or more are age 26 or younger
kff_2007$some_young_workers   <- ifelse(kff_2007$age26 == 1, 1, 0) #Less than 35% are age 26 or younger

table(kff_2007$age26, kff_2007$mostly_young_workers, useNA = "ifany")
table(kff_2007$age26, kff_2007$some_young_workers, useNA = "ifany")


### 2008
table(kff_2008$age26, useNA = "ifany") #examine variable

kff_2008$mostly_young_workers <- ifelse(kff_2008$age26 == 2, 1, 0) #35% or more are age 26 or younger
kff_2008$some_young_workers   <- ifelse(kff_2008$age26 == 1, 1, 0) #Less than 35% are age 26 or younger

table(kff_2008$age26, kff_2008$mostly_young_workers, useNA = "ifany")
table(kff_2008$age26, kff_2008$some_young_workers, useNA = "ifany")


#### 2009
table(kff_2009$age26, useNA = "ifany") #examine variable

kff_2009$mostly_young_workers <- ifelse(kff_2009$age26 == 2, 1, 0) #35% or more are age 26 or younger
kff_2009$some_young_workers   <- ifelse(kff_2009$age26 == 1, 1, 0) #Less than 35% are age 26 or younger

table(kff_2009$age26, kff_2009$mostly_young_workers, useNA = "ifany")
table(kff_2009$age26, kff_2009$some_young_workers, useNA = "ifany")


### 2010
table(kff_2010$age26, useNA = "ifany") #examine variable

kff_2010$mostly_young_workers <- ifelse(kff_2010$age26 == 2, 1, 0) #35% or more are age 26 or younger
kff_2010$some_young_workers   <- ifelse(kff_2010$age26 == 1, 1, 0) #Less than 35% are age 26 or younger

table(kff_2010$age26, kff_2010$mostly_young_workers, useNA = "ifany")
table(kff_2010$age26, kff_2010$some_young_workers, useNA = "ifany")


### 2011
table(kff_2011$age26, useNA = "ifany") #examine variable

kff_2011$mostly_young_workers <- ifelse(kff_2011$age26 == 2, 1, 0) #35% or more are age 26 or younger
kff_2011$some_young_workers   <- ifelse(kff_2011$age26 == 1, 1, 0) #Less than 35% are age 26 or younger

table(kff_2011$age26, kff_2011$mostly_young_workers, useNA = "ifany")
table(kff_2011$age26, kff_2011$some_young_workers, useNA = "ifany")


### 2012
table(kff_2012$age26, useNA = "ifany") #examine variable

kff_2012$mostly_young_workers <- ifelse(kff_2012$age26 == 2, 1, 0) #35% or more are age 26 or younger
kff_2012$some_young_workers   <- ifelse(kff_2012$age26 == 1, 1, 0) #Less than 35% are age 26 or younger

table(kff_2012$age26, kff_2012$mostly_young_workers, useNA = "ifany")
table(kff_2012$age26, kff_2012$some_young_workers, useNA = "ifany")


### 2013
table(kff_2013$age26, useNA = "ifany") #examine variable

kff_2013$mostly_young_workers <- ifelse(kff_2013$age26 == 2, 1, 0) #35% or more are age 26 or younger
kff_2013$some_young_workers   <- ifelse(kff_2013$age26 == 1, 1, 0) #Less than 35% are age 26 or younger

table(kff_2013$age26, kff_2013$mostly_young_workers)
table(kff_2013$age26, kff_2013$some_young_workers)



###### size (of firm) #####

### 2003
# Step 1: Examine the Variable
table(kff_2003$size)

# Step 2: Clean the variable to create small, medium, and large firms
kff_2003$small_firm    <- ifelse(kff_2003$size <= 3,                      1, 0) #firms under 50 emps
kff_2003$medium_firm   <- ifelse(kff_2003$size >  3 & kff_2003$size < 6,  1, 0) #firms with emps from (50,1000)
kff_2003$large_firm    <- ifelse(kff_2003$size == 6,                      1, 0) #firms over 1000 emps

# Combine dummies into a single ordered factor
kff_2003$firm_size <- with(
  kff_2003,
  factor(
    ifelse(small_firm         == 1,  "Small",
           ifelse(medium_firm == 1,  "Medium",
           ifelse(large_firm  == 1,  "Large", NA_character_))),
    levels = c("Small", "Medium", "Large"),
    ordered = TRUE
  )
)

# Step 3: Confirm
# dummy variables
table(kff_2003$size, kff_2003$small_firm, useNA = "ifany")
table(kff_2003$size, kff_2003$medium_firm, useNA = "ifany")
table(kff_2003$size, kff_2003$large_firm, useNA = "ifany")

# categorical variable
table(kff_2003$size, kff_2003$firm_size)


### 2004
table(kff_2004$size) #check variable

#encode dummy variables for firm size
kff_2004$small_firm    <- ifelse(kff_2004$size <= 3,                      1, 0)
kff_2004$medium_firm   <- ifelse(kff_2004$size >  3 & kff_2004$size < 6,  1, 0)
kff_2004$large_firm    <- ifelse(kff_2004$size == 6,                      1, 0)

# Step 3: Confirm
table(kff_2004$size, kff_2004$small_firm, useNA = "ifany")
table(kff_2004$size, kff_2004$medium_firm, useNA = "ifany")
table(kff_2004$size, kff_2004$large_firm, useNA = "ifany")


### 2005
table(kff_2005$size) #check variable

kff_2005$small_firm    <- ifelse(kff_2005$size <= 3,                      1, 0)
kff_2005$medium_firm   <- ifelse(kff_2005$size >  3 & kff_2005$size < 6,  1, 0)
kff_2005$large_firm    <- ifelse(kff_2005$size == 6,                      1, 0)

table(kff_2005$size, kff_2005$small_firm, useNA = "ifany")
table(kff_2005$size, kff_2005$medium_firm, useNA = "ifany")
table(kff_2005$size, kff_2005$large_firm, useNA = "ifany")


### 2006
table(kff_2006$size) #check variable

kff_2006$small_firm    <- ifelse(kff_2006$size <= 3,                      1, 0)
kff_2006$medium_firm   <- ifelse(kff_2006$size >  3 & kff_2006$size < 6,  1, 0)
kff_2006$large_firm    <- ifelse(kff_2006$size == 6,                      1, 0)

table(kff_2006$size, kff_2006$small_firm, useNA = "ifany")
table(kff_2006$size, kff_2006$medium_firm, useNA = "ifany")
table(kff_2006$size, kff_2006$large_firm, useNA = "ifany")


### 2007
table(kff_2007$size)

kff_2007$small_firm    <- ifelse(kff_2007$size <= 3,                      1, 0)
kff_2007$medium_firm   <- ifelse(kff_2007$size >  3 & kff_2007$size < 6,  1, 0)
kff_2007$large_firm    <- ifelse(kff_2007$size == 6,                      1, 0)

table(kff_2007$size, kff_2007$small_firm, useNA = "ifany")
table(kff_2007$size, kff_2007$medium_firm, useNA = "ifany")
table(kff_2007$size, kff_2007$large_firm, useNA = "ifany")


### 2008
table(kff_2008$size)

kff_2008$small_firm    <- ifelse(kff_2008$size <= 3,                      1, 0)
kff_2008$medium_firm   <- ifelse(kff_2008$size >  3 & kff_2008$size < 6,  1, 0)
kff_2008$large_firm    <- ifelse(kff_2008$size == 6,                      1, 0)

table(kff_2008$size, kff_2008$small_firm, useNA = "ifany")
table(kff_2008$size, kff_2008$medium_firm, useNA = "ifany")
table(kff_2008$size, kff_2008$large_firm, useNA = "ifany")


### 2009
table(kff_2009$size)

kff_2009$small_firm    <- ifelse(kff_2009$size <= 3,                      1, 0)
kff_2009$medium_firm   <- ifelse(kff_2009$size >  3 & kff_2009$size < 6,  1, 0)
kff_2009$large_firm    <- ifelse(kff_2009$size == 6,                      1, 0)

table(kff_2009$size, kff_2009$small_firm, useNA = "ifany")
table(kff_2009$size, kff_2009$medium_firm, useNA = "ifany")
table(kff_2009$size, kff_2009$large_firm, useNA = "ifany")


### 2010
table(kff_2010$size)

kff_2010$small_firm    <- ifelse(kff_2010$size <= 3,                      1, 0)
kff_2010$medium_firm   <- ifelse(kff_2010$size >  3 & kff_2010$size < 6,  1, 0)
kff_2010$large_firm    <- ifelse(kff_2010$size == 6,                      1, 0)

table(kff_2010$size, kff_2010$small_firm, useNA = "ifany")
table(kff_2010$size, kff_2010$medium_firm, useNA = "ifany")
table(kff_2010$size, kff_2010$large_firm, useNA = "ifany")


### 2011
table(kff_2011$size)

kff_2011$small_firm    <- ifelse(kff_2011$size <= 3,                      1, 0)
kff_2011$medium_firm   <- ifelse(kff_2011$size >  3 & kff_2011$size < 6,  1, 0)
kff_2011$large_firm    <- ifelse(kff_2011$size == 6,                      1, 0)

table(kff_2011$size, kff_2011$small_firm, useNA = "ifany")
table(kff_2011$size, kff_2011$medium_firm, useNA = "ifany")
table(kff_2011$size, kff_2011$large_firm, useNA = "ifany")


### 2012
table(kff_2012$size)

kff_2012$small_firm    <- ifelse(kff_2012$size <= 3,                      1, 0)
kff_2012$medium_firm   <- ifelse(kff_2012$size >  3 & kff_2012$size < 6,  1, 0)
kff_2012$large_firm    <- ifelse(kff_2012$size == 6,                      1, 0)

table(kff_2012$size, kff_2012$small_firm, useNA = "ifany")
table(kff_2012$size, kff_2012$medium_firm, useNA = "ifany")
table(kff_2012$size, kff_2012$large_firm, useNA = "ifany")


### 2013
table(kff_2013$size)

kff_2013$small_firm    <- ifelse(kff_2013$size <= 3,                      1, 0)
kff_2013$medium_firm   <- ifelse(kff_2013$size >  3 & kff_2013$size < 6,  1, 0)
kff_2013$large_firm    <- ifelse(kff_2013$size == 6,                      1, 0)

table(kff_2013$size, kff_2013$small_firm, useNA = "ifany")
table(kff_2013$size, kff_2013$medium_firm, useNA = "ifany")
table(kff_2013$size, kff_2013$large_firm, useNA = "ifany")

### 2014
table(kff_2014$size)
kff_2014$small_firm    <- ifelse(kff_2014$size <= 3,                      1, 0)
kff_2014$medium_firm   <- ifelse(kff_2014$size >  3 & kff_2014$size < 6,  1, 0)
kff_2014$large_firm    <- ifelse(kff_2014$size == 6,                      1, 0)
table(kff_2014$size, kff_2014$small_firm, useNA = "ifany")
table(kff_2014$size, kff_2014$medium_firm, useNA = "ifany")
table(kff_2014$size, kff_2014$large_firm, useNA = "ifany")

### 2015
table(kff_2015$size)
kff_2015$small_firm    <- ifelse(kff_2015$size <= 3,                      1, 0)
kff_2015$medium_firm   <- ifelse(kff_2015$size >  3 & kff_2015$size < 6,  1, 0)
kff_2015$large_firm    <- ifelse(kff_2015$size == 6,                      1, 0)
table(kff_2015$size, kff_2015$small_firm, useNA = "ifany")
table(kff_2015$size, kff_2015$medium_firm, useNA = "ifany")
table(kff_2015$size, kff_2015$large_firm, useNA = "ifany")

### 2016
table(kff_2016$size)
kff_2016$small_firm    <- ifelse(kff_2016$size <= 3,                      1, 0)
kff_2016$medium_firm   <- ifelse(kff_2016$size >  3 & kff_2016$size < 6,  1, 0)
kff_2016$large_firm    <- ifelse(kff_2016$size == 6,                      1, 0)
table(kff_2016$size, kff_2016$small_firm, useNA = "ifany")
table(kff_2016$size, kff_2016$medium_firm, useNA = "ifany")
table(kff_2016$size, kff_2016$large_firm, useNA = "ifany")

###2017
table(kff_2017$size)
kff_2017$small_firm    <- ifelse(kff_2017$size <= 3,                      1, 0)
kff_2017$medium_firm   <- ifelse(kff_2017$size >  3 & kff_2017$size < 6,  1, 0)
kff_2017$large_firm    <- ifelse(kff_2017$size == 6,                      1, 0)
table(kff_2017$size, kff_2017$small_firm, useNA = "ifany")
table(kff_2017$size, kff_2017$medium_firm, useNA = "ifany")
table(kff_2017$size, kff_2017$large_firm, useNA = "ifany")

###2018
table(kff_2018$size)
kff_2018$small_firm    <- ifelse(kff_2018$size <= 3,                      1, 0)
kff_2018$medium_firm   <- ifelse(kff_2018$size >  3 & kff_2018$size < 6,  1, 0)
kff_2018$large_firm    <- ifelse(kff_2018$size == 6,                      1, 0)
table(kff_2018$size, kff_2018$small_firm, useNA = "ifany")
table(kff_2018$size, kff_2018$medium_firm, useNA = "ifany")
table(kff_2018$size, kff_2018$large_firm, useNA = "ifany")

###2019
table(kff_2019$size)
kff_2019$small_firm    <- ifelse(kff_2019$size <= 3,                      1, 0)
kff_2019$medium_firm   <- ifelse(kff_2019$size >  3 & kff_2019$size < 6,  1, 0)
kff_2019$large_firm    <- ifelse(kff_2019$size == 6,                      1, 0)
table(kff_2019$size, kff_2019$small_firm, useNA = "ifany")
table(kff_2019$size, kff_2019$medium_firm, useNA = "ifany")
table(kff_2019$size, kff_2019$large_firm, useNA = "ifany")

###2020
table(kff_2020$size)
kff_2020$small_firm    <- ifelse(kff_2020$size <= 3,                      1, 0)
kff_2020$medium_firm   <- ifelse(kff_2020$size >  3 & kff_2020$size < 6,  1, 0)
kff_2020$large_firm    <- ifelse(kff_2020$size == 6,                      1, 0)
table(kff_2020$size, kff_2020$small_firm, useNA = "ifany")
table(kff_2020$size, kff_2020$medium_firm, useNA = "ifany")
table(kff_2020$size, kff_2020$large_firm, useNA = "ifany")

###2021
table(kff_2021$size)
kff_2021$small_firm    <- ifelse(kff_2021$size <= 3,                      1, 0)
kff_2021$medium_firm   <- ifelse(kff_2021$size >  3 & kff_2021$size < 6,  1, 0)
kff_2021$large_firm    <- ifelse(kff_2021$size == 6,                      1, 0)
table(kff_2021$size, kff_2021$small_firm, useNA = "ifany")
table(kff_2021$size, kff_2021$medium_firm, useNA = "ifany")
table(kff_2021$size, kff_2021$large_firm, useNA = "ifany")

###2022
table(kff_2022$size)
kff_2022$small_firm    <- ifelse(kff_2022$size <= 3,                      1, 0)
kff_2022$medium_firm   <- ifelse(kff_2022$size >  3 & kff_2022$size < 6,  1, 0)
kff_2022$large_firm    <- ifelse(kff_2022$size == 6,                      1, 0)
table(kff_2022$size, kff_2022$small_firm, useNA = "ifany")
table(kff_2022$size, kff_2022$medium_firm, useNA = "ifany")
table(kff_2022$size, kff_2022$large_firm, useNA = "ifany")

###2023
table(kff_2023$size)
kff_2023$small_firm    <- ifelse(kff_2023$size <= 3,                      1, 0)
kff_2023$medium_firm   <- ifelse(kff_2023$size >  3 & kff_2023$size < 6,  1, 0)
kff_2023$large_firm    <- ifelse(kff_2023$size == 6,                      1, 0)
table(kff_2023$size, kff_2023$small_firm, useNA = "ifany")
table(kff_2023$size, kff_2023$medium_firm, useNA = "ifany")
table(kff_2023$size, kff_2023$large_firm, useNA = "ifany")

###2024
table(kff_2024$size)
kff_2024$small_firm    <- ifelse(kff_2024$size <= 3,                      1, 0)
kff_2024$medium_firm   <- ifelse(kff_2024$size >  3 & kff_2024$size < 6,  1, 0)
kff_2024$large_firm    <- ifelse(kff_2024$size == 6,                      1, 0)
table(kff_2024$size, kff_2024$small_firm, useNA = "ifany")
table(kff_2024$size, kff_2024$medium_firm, useNA = "ifany")
table(kff_2024$size, kff_2024$large_firm, useNA = "ifany")



###### Likelihood of Making a Change in the Next Year: Offer High Deductible Health Plan #####

###2004 (Note: defined as a deductible of more than $1000.)
#step 1: examine variable
summary(kff_2004$k11h)
table(kff_2004$k11h, useNA = "ifany")

#step 2: clean variable by creating a new variable
kff_2004$very_likely_next_year <- ifelse(kff_2004$k11h == 1,1,0)
kff_2004$sm_likely_next_year <- ifelse(kff_2004$k11h == 2,1,0)
kff_2004$not_too_likely_next_year <- ifelse(kff_2004$k11h == 3,1,0)
kff_2004$not_at_all_likely_next_year <- ifelse(kff_2004$k11h == 4,1,0)
kff_2004$dk_how_likely_next_year <- ifelse(kff_2004$k11h == 5,1,0)

#confirm correct cleaning
table(kff_2004$k11h, kff_2004$very_likely_next_year, useNA = "ifany")
table(kff_2004$k11h, kff_2004$sm_likely_next_year, useNA = "ifany")
table(kff_2004$k11h, kff_2004$not_too_likely_next_year, useNA = "ifany")
table(kff_2004$k11h, kff_2004$not_at_all_likely_next_year, useNA = "ifany")
table(kff_2004$k11h, kff_2004$dk_how_likely_next_year, useNA = "ifany")


###2005 (Note: defined as an annual deductible of at least $1,000 for single coverage and $2,000 for family coverage, with a health reimbursement arrangement in the next year?)
#step 1: examine variable
summary(kff_2005$k11h)

#step 2: clean variable by creating a new variable
kff_2004$very_likely_next_year <- ifelse(kff_2004$k11h == 1,1,0)
kff_2004$sm_likely_next_year <- ifelse(kff_2004$k11h == 2,1,0)
kff_2004$not_too_likely_next_year <- ifelse(kff_2004$k11h == 3,1,0)
kff_2004$not_at_all_likely_next_year <- ifelse(kff_2004$k11h == 4,1,0)
kff_2004$dk_how_likely_next_year <- ifelse(kff_2004$k11h == 5,1,0)

#confirm correct cleaning
table(kff_2004$k11h, kff_2004$very_likely_next_year, useNA = "ifany")
table(kff_2004$k11h, kff_2004$sm_likely_next_year, useNA = "ifany")
table(kff_2004$k11h, kff_2004$not_too_likely_next_year, useNA = "ifany")
table(kff_2004$k11h, kff_2004$not_too_likely_next_year, useNA = "ifany")
table(kff_2004$k11h, kff_2004$not_at_all_likely_next_year, useNA = "ifany")
table(kff_2004$k11h, kff_2004$dk_how_likely_next_year, useNA = "ifany")





####################################################################################
############              Phase 2: Data Merging        ############
####################################################################################

# Step 1: Create variable list that is consistent across all years 
my_varlist <- c("small_firm", "medium_firm", "large_firm",
                "AgriMinConst", "manufacturing", "transportutilcomms",
                "wholesale", "retail", "financial", "service",
                "government", "healthcare", "offers", "doesnt_offer")



# Step 2: Complete case information for all variables in varlsit in 2004
# kff_complete_case_2003 <- kff_2003 %>%
#   select(all_of(my_varlist)) %>%
#   filter(complete.cases(.))
# 
# kff_complete_case_2004 <- kff_2004 %>%
#   select(all_of(my_varlist)) %>%
#   filter(complete.cases(.))
# 
# kff_complete_case_2005 <- kff_2005 %>%
#   select(all_of(my_varlist)) %>%
#   filter(complete.cases(.))

kff_complete_case_2006 <- kff_2006 %>%
  select(all_of(my_varlist)) %>%
  filter(complete.cases(.))

kff_complete_case_2007 <- kff_2007 %>%
  select(all_of(my_varlist)) %>%
  filter(complete.cases(.))

kff_complete_case_2008 <- kff_2008 %>%
  select(all_of(my_varlist)) %>%
  filter(complete.cases(.))

kff_complete_case_2009 <- kff_2009 %>%
  select(all_of(my_varlist)) %>%
  filter(complete.cases(.))

kff_complete_case_2010 <- kff_2010 %>%
  select(all_of(my_varlist)) %>%
  filter(complete.cases(.))

kff_complete_case_2011 <- kff_2011 %>%
  select(all_of(my_varlist)) %>%
  filter(complete.cases(.))

#offers variable b8e not in dataset INVESTIGATE LATER
#kff_complete_case_2012 <- kff_2012 %>%
#  select(all_of(my_varlist)) %>%
#  filter(complete.cases(.))

kff_complete_case_2013 <- kff_2013 %>%
  select(all_of(my_varlist)) %>%
  filter(complete.cases(.))

kff_complete_case_2014 <- kff_2014 %>%
  select(all_of(my_varlist)) %>%
  filter(complete.cases(.))

kff_complete_case_2015 <- kff_2015 %>%
  select(all_of(my_varlist)) %>%
  filter(complete.cases(.))
  
kff_complete_case_2016 <- kff_2016 %>%
  select(all_of(my_varlist)) %>%
  filter(complete.cases(.))

kff_complete_case_2017 <- kff_2017 %>%
  select(all_of(my_varlist)) %>%
  filter(complete.cases(.))

kff_complete_case_2018 <- kff_2018 %>%
  select(all_of(my_varlist)) %>%
  filter(complete.cases(.))

kff_complete_case_2019 <- kff_2019 %>%
  select(all_of(my_varlist)) %>%
  filter(complete.cases(.))

kff_complete_case_2020 <- kff_2020 %>%
  select(all_of(my_varlist)) %>%
  filter(complete.cases(.))

kff_complete_case_2021 <- kff_2021 %>%
  select(all_of(my_varlist)) %>%
  filter(complete.cases(.))

kff_complete_case_2022 <- kff_2022 %>%
  select(all_of(my_varlist)) %>%
  filter(complete.cases(.))

kff_complete_case_2023 <- kff_2023 %>%
  select(all_of(my_varlist)) %>%
  filter(complete.cases(.))

kff_complete_case_2024 <- kff_2024 %>%
  select(all_of(my_varlist)) %>%
  filter(complete.cases(.))

# Step 3: Create Long Dataset merging all years
kff_long_all_years <- bind_rows(
#  kff_complete_case_2003 %>% mutate(year = 2003),
#  kff_complete_case_2004 %>% mutate(year = 2004),
#  kff_complete_case_2005 %>% mutate(year = 2005),
  kff_complete_case_2006 %>% mutate(year = 2006),
  kff_complete_case_2007 %>% mutate(year = 2007),
  kff_complete_case_2008 %>% mutate(year = 2008),
  kff_complete_case_2009 %>% mutate(year = 2009),
  kff_complete_case_2010 %>% mutate(year = 2010),
  kff_complete_case_2011 %>% mutate(year = 2011),
  #kff_complete_case_2012 %>% mutate(year = 2012), #offers variable missing in 2012 
  kff_complete_case_2013 %>% mutate(year = 2013),
  kff_complete_case_2014 %>% mutate(year = 2014),
  kff_complete_case_2015 %>% mutate(year = 2015),
  kff_complete_case_2016 %>% mutate(year = 2016),
  kff_complete_case_2017 %>% mutate(year = 2017),
  kff_complete_case_2018 %>% mutate(year = 2018),
  kff_complete_case_2019 %>% mutate(year = 2019),
  kff_complete_case_2020 %>% mutate(year = 2020),
  kff_complete_case_2021 %>% mutate(year = 2021),
  kff_complete_case_2022 %>% mutate(year = 2022),
  kff_complete_case_2023 %>% mutate(year = 2023),
  kff_complete_case_2024 %>% mutate(year = 2024)
)


####################################################################################
############              Phase 3: Descriptive statistics        ############
####################################################################################


# figure showing the proportion of firms offering HDHPs over time
hdhp_offering_trend <- kff_long_all_years %>%
  group_by(year) %>%
  summarize(
    total_firms = n(),
    firms_offering_hdhp = sum(offers),
    proportion_offering_hdhp = firms_offering_hdhp / total_firms
  )
# Plot the trend
ggplot(hdhp_offering_trend, aes(x = year, y = proportion_offering_hdhp)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Trend of Firms Offering HDHPs Over Time",
       x = "Year",
       y = "Proportion of Firms Offering HDHPs") +
  theme_minimal()


# plot proportion that don't offer HDHP over time
hdhp_not_offering_trend <- kff_long_all_years %>%
  group_by(year) %>%
  summarize(
    total_firms = n(),
    firms_not_offering_hdhp = sum(doesnt_offer),
    proportion_not_offering_hdhp = firms_not_offering_hdhp / total_firms
  )
# Plot the trend
ggplot(hdhp_not_offering_trend, aes(x = year, y = proportion_not_offering_hdhp)) +
  geom_line(color = "green") +
  geom_point(color = "orange") +
  labs(title = "Trend of Firms Not Offering HDHPs Over Time",
       x = "Year",
       y = "Proportion of Firms Not Offering HDHPs") +
  theme_minimal()





# plot the proportion of firms in each industry over time
industry_trend <- kff_long_all_years %>%
  group_by(year) %>%
  summarize(
    total_firms = n(),
    AgriMinConst_count = sum(AgriMinConst),
    manufacturing_count = sum(manufacturing),
    transportutilcomms_count = sum(transportutilcomms),
    wholesale_count = sum(wholesale),
    retail_count = sum(retail),
    financial_count = sum(financial),
    service_count = sum(service),
    government_count = sum(government),
    healthcare_count = sum(healthcare)
  ) %>%
  mutate(
    AgriMinConst_prop = AgriMinConst_count / total_firms,
    manufacturing_prop = manufacturing_count / total_firms,
    transportutilcomms_prop = transportutilcomms_count / total_firms,
    wholesale_prop = wholesale_count / total_firms,
    retail_prop = retail_count / total_firms,
    financial_prop = financial_count / total_firms,
    service_prop = service_count / total_firms,
    government_prop = government_count / total_firms,
    healthcare_prop = healthcare_count / total_firms
  ) %>%
  select(year, ends_with("_prop")) %>%
  pivot_longer(-year, names_to = "industry", values_to = "proportion")

# Plot the industry trend
ggplot(industry_trend, aes(x = year, y = proportion, color = industry)) +
  geom_line() +
  geom_point() +
  labs(title = "Proportion of Firms in Each Industry Over Time",
       x = "Year",
       y = "Proportion of Firms",
       color = "Industry") +
  theme_minimal()

# plot this proportion as a stacked line graph
ggplot(industry_trend, aes(x = year, y = proportion, fill = industry)) +
  geom_area(position = 'fill', alpha = 0.6) +
  labs(title = "Proportion of Firms in Each Industry Over Time",
       x = "Year",
       y = "Proportion of Firms",
       fill = "Industry") +
  theme_minimal()




# plot the proportion of firms by size over time
size_trend <- kff_long_all_years %>%
  group_by(year) %>%
  summarize(
    total_firms = n(),
    small_firm_count = sum(small_firm),
    medium_firm_count = sum(medium_firm),
    large_firm_count = sum(large_firm)
  ) %>%
  mutate(
    small_firm_prop = small_firm_count / total_firms,
    medium_firm_prop = medium_firm_count / total_firms,
    large_firm_prop = large_firm_count / total_firms
  ) %>%
  select(year, ends_with("_prop")) %>%
  pivot_longer(-year, names_to = "firm_size", values_to = "proportion")

# Plot the size trend
ggplot(size_trend, aes(x = year, y = proportion, color = firm_size)) +
  geom_line() +
  geom_point() +
  labs(title = "Proportion of Firms by Size Over Time",
       x = "Year",
       y = "Proportion of Firms",
       color = "Firm Size") +
  theme_minimal()



# plot this proportion as a stacked line graph
ggplot(size_trend, aes(x = year, y = proportion, fill = firm_size)) +
  geom_area(position = 'fill', alpha = 0.6) +
  labs(title = "Proportion of Firms by Size Over Time",
       x = "Year",
       y = "Proportion of Firms",
       fill = "Firm Size") +
  theme_minimal()




# calculate summary statistics for the long dataset
summary_stats <- kff_long_all_years %>%
  summarize(
    total_firms = n(),
    avg_small_firm = mean(small_firm),
    avg_medium_firm = mean(medium_firm),
    avg_large_firm = mean(large_firm),
    avg_AgriMinConst = mean(AgriMinConst),
    avg_manufacturing = mean(manufacturing),
    avg_transportutilcomms = mean(transportutilcomms),
    avg_wholesale = mean(wholesale),
    avg_retail = mean(retail),
    avg_financial = mean(financial),
    avg_service = mean(service),
    avg_government = mean(government),
    avg_healthcare = mean(healthcare),
    avg_offers = mean(offers),
    avg_doesnt_offer = mean(doesnt_offer)
  )

print(summary_stats)


####################################################################################
############              Phase 4: Regression Analysis        ############
####################################################################################


# Logistic regression model to predict the likelihood of offering HDHPs
hdhp_logistic_model <- glm(offers ~ small_firm + medium_firm  +
                             AgriMinConst + manufacturing + transportutilcomms +
                             wholesale + retail + financial +
                             government + healthcare + year,
                           data = kff_long_all_years,
                           family = binomial)
summary(hdhp_logistic_model)

# hlm with year as random intercept
hdhp_hlm_model <- glmer(offers ~ small_firm + medium_firm  +
                          AgriMinConst + manufacturing + transportutilcomms +
                          wholesale + retail + financial  +
                          government + healthcare + (1 | year),
                        data = kff_long_all_years,
                        family = binomial)
summary(hdhp_hlm_model)

# MUSE plots
# proportion of firms offering HDHPs by industry over time; separate graphs for each industry

