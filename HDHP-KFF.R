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
setwd("~/Library/CloudStorage/GoogleDrive-gugliem2@tcnj.edu/.shortcut-targets-by-id/14oLkrWtHW1NzX87aL0DxDGo_9Ysj-XBQ/HDHP/resources/KFF Data/Data")

# Daniel's command to set WD
setwd("G:/.shortcut-targets-by-id/14oLkrWtHW1NzX87aL0DxDGo_9Ysj-XBQ/HDHP/work")

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
kff_2021 <- read.csv('health benefits 2021.csv') 
kff_2022 <- read.csv('2022-11-28 health benefits 2022.csv') 
kff_2023 <- read.csv('2023-10-17 health benefits 2023.csv')
kff_2024 <- read.csv('2022-10-10 health benefits 2024.csv')

#### ERROR ERROR ERROR: 2024 not loading


### IN the working directory, 2021 and forward are CSV files. Update or fix.






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
kff_2003$offers <- ifelse(kff_2003$j3 == 1, 1, 0)
kff_2003$doesnt_offer <- ifelse(kff_2003$j3 == 2,1,0)
kff_2003$unsure_of_offer <- ifelse(kff_2003$j3 ==3,1,0)

# step 3: Confirm correct cleaning
table(kff_2003$j3, kff_2003$offers)
table(kff_2003$j3, kff_2003$doesnt_offer)
table(kff_2003$j3, kff_2003$unsure_of_offer)


### 2004
#Step 1: Examine variable
summary(kff_2004$j3) 

# Step 2: Clean variable (always create new variable!)
kff_2004$offers <- ifelse(kff_2004$j3 == 1, 1, 0)
kff_2004$doesnt_offer <- ifelse(kff_2004$j3 == 2,1,0)
kff_2004$unsure_of_offer <- ifelse(kff_2004$j3 ==3,1,0)

# step 3: Confirm correct cleaning
table(kff_2004$j3, kff_2004$offers)
table(kff_2004$j3, kff_2004$doesnt_offer)
table(kff_2004$j3, kff_2004$unsure_of_offer)

### 2005
#Step 1: Examine variable
summary(kff_2005$b8e) 

# Step 2: Clean variable (always create new variable!)
kff_2005$offers <- ifelse(kff_2005$b8e == 1, 1, 0)
kff_2005$doesnt_offer <- ifelse(kff_2005$b8e == 2,1,0)

# step 3: Confirm correct cleaning
table(kff_2005$b8e, kff_2005$offers)
table(kff_2005$b8e, kff_2005$doesnt_offer)


###2006
#Step 1: Examine variable
summary(kff_2006$b8e) 

# Step 2: Clean variable (always create new variable!)
kff_2006$offers <- ifelse(kff_2006$b8e == 1, 1, 0)
kff_2006$doesnt_offer <- ifelse(kff_2006$b8e == 2,1,0)

# step 3: Confirm correct cleaning
table(kff_2006$b8e, kff_2006$offers)
table(kff_2006$b8e, kff_2006$doesnt_offer)






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
table(kff_2004$k11h, kff_2004$very_likely_next_year)
table(kff_2004$k11h, kff_2004$sm_likely_next_year)
table(kff_2004$k11h, kff_2004$not_too_likely_next_year)
table(kff_2004$k11h, kff_2004$not_at_all_likely_next_year)
table(kff_2004$k11h, kff_2004$dk_how_likely_next_year)


###2005 (Note: defined as an annual deductible of at least $1,000 for single coverage and $2,000 for family coverage, with a health reimbursement arrangement in the next year?)
#step 1: examine variable
summary(kff_2004$k11h)

#step 2: clean variable by creating a new variable
kff_2004$very_likely_next_year <- ifelse(kff_2004$k11h == 1,1,0)
kff_2004$sm_likely_next_year <- ifelse(kff_2004$k11h == 2,1,0)
kff_2004$not_too_likely_next_year <- ifelse(kff_2004$k11h == 3,1,0)
kff_2004$not_at_all_likely_next_year <- ifelse(kff_2004$k11h == 4,1,0)
kff_2004$dk_how_likely_next_year <- ifelse(kff_2004$k11h == 5,1,0)

#confirm correct cleaning
table(kff_2004$k11h, kff_2004$very_likely_next_year)
table(kff_2004$k11h, kff_2004$sm_likely_next_year)
table(kff_2004$k11h, kff_2004$not_too_likely_next_year)
table(kff_2004$k11h, kff_2004$not_at_all_likely_next_year)
table(kff_2004$k11h, kff_2004$dk_how_likely_next_year)



###### Clean Percent of Workers with Health Benefits Covered in HDHP #####

### 2006
#examine variable by showing summary stats (min, 1st quartile, median, mean, 3rd quartile, max, and number of missing values (NA))
summary(kff_2006$b12e)

kff_2006$percent_hdhp <- kff_2006$b12e #clean data by creating new variable 

#confirm correct cleaning by checking that the new variable matches the original (difference should be 0 for all observations)
kff_2006$test_percent_hdhp <- kff_2006$b12e - kff_2006$percent_hdhp
summary(kff_2006$test_percent_hdhp) #print summary stats


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




###### Clean industry #####

### 2003 ###
##industry##
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
table(kff_2003$industry, kff_2003$MinConst)
table(kff_2003$industry, kff_2003$manufacturing)
table(kff_2003$industry, kff_2003$transportutilcomms)
table(kff_2003$industry, kff_2003$wholesale)
table(kff_2003$industry, kff_2003$retail)
table(kff_2003$industry, kff_2003$financial)
table(kff_2003$industry, kff_2003$service)
table(kff_2003$industry, kff_2003$government)
table(kff_2003$industry, kff_2003$healthcare)



### 2004 ###
##industry##
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
table(kff_2004$industry, kff_2004$MinConst)
table(kff_2004$industry, kff_2004$manufacturing)
table(kff_2004$industry, kff_2004$transportutilcomms)
table(kff_2004$industry, kff_2004$wholesale)
table(kff_2004$industry, kff_2004$retail)
table(kff_2004$industry, kff_2004$financial)
table(kff_2004$industry, kff_2004$service)
table(kff_2004$industry, kff_2004$government)
table(kff_2004$industry, kff_2004$healthcare)



### 2005 ###
##industry##
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
table(kff_2005$industry, kff_2005$MinConst)
table(kff_2005$industry, kff_2005$manufacturing)
table(kff_2005$industry, kff_2005$transportutilcomms)
table(kff_2005$industry, kff_2005$wholesale)
table(kff_2005$industry, kff_2005$retail)
table(kff_2005$industry, kff_2005$financial)
table(kff_2005$industry, kff_2005$service)
table(kff_2005$industry, kff_2005$government)
table(kff_2005$industry, kff_2005$healthcare)



### 2006 ###
# Step 1: Examine variable
table(kff_2006$industry, useNA = "ifany")

# Step 2: Clean variable (always create new variable!)
kff_2006$AgriMinConst <- ifelse(kff_2006$indust2 == 1,1,0) #experimental, wholesales counts agriculture?
kff_2006$MinConst <- ifelse(kff_2006$industry == 1 | kff_2006$industry == 2, 1, 0)
kff_2006$manufacturing <- ifelse(kff_2006$industry == 3, 1, 0)
kff_2006$transportutilcomms <- ifelse(kff_2006$industry == 4, 1, 0)
kff_2006$wholesale <- ifelse(kff_2006$industry == 5, 1, 0)
kff_2006$retail <- ifelse(kff_2006$industry == 6, 1, 0)
kff_2006$financial <- ifelse(kff_2006$industry == 7, 1, 0)
kff_2006$service <- ifelse(kff_2006$industry == 8, 1, 0)
kff_2006$government <- ifelse(kff_2006$industry == 9, 1, 0)
kff_2006$healthcare <- ifelse(kff_2006$industry == 10, 1, 0)

# step 3: Confirm correct cleaning
table(kff_2006$industry, kff_2006$AgriMinConst) #experimental
table(kff_2006$industry, kff_2006$MinConst)
table(kff_2006$industry, kff_2006$manufacturing)
table(kff_2006$industry, kff_2006$transportutilcomms)
table(kff_2006$industry, kff_2006$wholesale)
table(kff_2006$industry, kff_2006$retail)
table(kff_2006$industry, kff_2006$financial)
table(kff_2006$industry, kff_2006$service)
table(kff_2006$industry, kff_2006$government)
table(kff_2006$industry, kff_2006$healthcare)

### 2007 ###
# Step 1: Examine variable
table(kff_2007$industry, useNA = "ifany")

# Step 2: Clean variable (always create new variable!)
kff_2007$MinConst <- ifelse(kff_2007$industry == 1 | kff_2007$industry == 2, 1, 0)
kff_2007$manufacturing <- ifelse(kff_2007$industry == 3, 1, 0)
kff_2007$transportutilcomms <- ifelse(kff_2007$industry == 4, 1, 0)
kff_2007$wholesale <- ifelse(kff_2007$industry == 5, 1, 0)
kff_2007$retail <- ifelse(kff_2007$industry == 6, 1, 0)
kff_2007$financial <- ifelse(kff_2007$industry == 7, 1, 0)
kff_2007$service <- ifelse(kff_2007$industry == 8, 1, 0)
kff_2007$government <- ifelse(kff_2007$industry == 9, 1, 0)
kff_2007$healthcare <- ifelse(kff_2007$industry == 10, 1, 0)

# step 3: Confirm correct cleaning
table(kff_2007$industry, kff_2007$MinConst)
table(kff_2007$industry, kff_2007$manufacturing)
table(kff_2007$industry, kff_2007$transportutilcomms)
table(kff_2007$industry, kff_2007$wholesale)
table(kff_2007$industry, kff_2007$retail)
table(kff_2007$industry, kff_2007$financial)
table(kff_2007$industry, kff_2007$service)
table(kff_2007$industry, kff_2007$government)
table(kff_2007$industry, kff_2007$healthcare)

### 2008 ###
# Step 1: Examine variable
table(kff_2008$industry, useNA = "ifany")

# Step 2: Clean variable (always create new variable!)
kff_2008$MinConst <- ifelse(kff_2008$industry == 1 | kff_2008$industry == 2, 1, 0)
kff_2008$manufacturing <- ifelse(kff_2008$industry == 3, 1, 0)
kff_2008$transportutilcomms <- ifelse(kff_2008$industry == 4, 1, 0)
kff_2008$wholesale <- ifelse(kff_2008$industry == 5, 1, 0)
kff_2008$retail <- ifelse(kff_2008$industry == 6, 1, 0)
kff_2008$financial <- ifelse(kff_2008$industry == 7, 1, 0)
kff_2008$service <- ifelse(kff_2008$industry == 8, 1, 0)
kff_2008$government <- ifelse(kff_2008$industry == 9, 1, 0)
kff_2008$healthcare <- ifelse(kff_2008$industry == 10, 1, 0)

# step 3: Confirm correct cleaning
table(kff_2008$industry, kff_2008$MinConst)
table(kff_2008$industry, kff_2008$manufacturing)
table(kff_2008$industry, kff_2008$transportutilcomms)
table(kff_2008$industry, kff_2008$wholesale)
table(kff_2008$industry, kff_2008$retail)
table(kff_2008$industry, kff_2008$financial)
table(kff_2008$industry, kff_2008$service)
table(kff_2008$industry, kff_2008$government)
table(kff_2008$industry, kff_2008$healthcare)

### 2009 ###
# Step 1: Examine variable
table(kff_2009$industry, useNA = "ifany")

# Step 2: Clean variable (always create new variable!)
kff_2009$MinConst <- ifelse(kff_2009$industry == 1 | kff_2009$industry == 2, 1, 0)
kff_2009$manufacturing <- ifelse(kff_2009$industry == 3, 1, 0)
kff_2009$transportutilcomms <- ifelse(kff_2009$industry == 4, 1, 0)
kff_2009$wholesale <- ifelse(kff_2009$industry == 5, 1, 0)
kff_2009$retail <- ifelse(kff_2009$industry == 6, 1, 0)
kff_2009$financial <- ifelse(kff_2009$industry == 7, 1, 0)
kff_2009$service <- ifelse(kff_2009$industry == 8, 1, 0)
kff_2009$government <- ifelse(kff_2009$industry == 9, 1, 0)
kff_2009$healthcare <- ifelse(kff_2009$industry == 10, 1, 0)

# step 3: Confirm correct cleaning
table(kff_2009$industry, kff_2009$MinConst)
table(kff_2009$industry, kff_2009$manufacturing)
table(kff_2009$industry, kff_2009$transportutilcomms)
table(kff_2009$industry, kff_2009$wholesale)
table(kff_2009$industry, kff_2009$retail)
table(kff_2009$industry, kff_2009$financial)
table(kff_2009$industry, kff_2009$service)
table(kff_2009$industry, kff_2009$government)
table(kff_2009$industry, kff_2009$healthcare)


### 2010 ###
# Step 1: Examine variable
table(kff_2010$industry, useNA = "ifany")

# Step 2: Clean variable (always create new variable!)
kff_2010$MinConst <- ifelse(kff_2009$industry == 1 | kff_2010$industry == 2, 1, 0)
kff_2010$manufacturing <- ifelse(kff_2010$industry == 3, 1, 0)
kff_2010$transportutilcomms <- ifelse(kff_2010$industry == 4, 1, 0)
kff_2010$wholesale <- ifelse(kff_2010$industry == 5, 1, 0)
kff_2010$retail <- ifelse(kff_2010$industry == 6, 1, 0)
kff_2010$financial <- ifelse(kff_2010$industry == 7, 1, 0)
kff_2010$service <- ifelse(kff_2010$industry == 8, 1, 0)
kff_2010$government <- ifelse(kff_2010$industry == 9, 1, 0)
kff_2010$healthcare <- ifelse(kff_2010$industry == 10, 1, 0)

# step 3: Confirm correct cleaning
table(kff_2010$industry, kff_2010$MinConst)
table(kff_2010$industry, kff_2010$manufacturing)
table(kff_2010$industry, kff_2010$transportutilcomms)
table(kff_2010$industry, kff_2010$wholesale)
table(kff_2010$industry, kff_2010$retail)
table(kff_2010$industry, kff_2010$financial)
table(kff_2010$industry, kff_2010$service)
table(kff_2010$industry, kff_2010$government)
table(kff_2010$industry, kff_2010$healthcare)


## 2011 ##
# Step 1: Examine variable
table(kff_2011$industry, useNA = "ifany")

# Step 2: Clean variable (always create new variable!)
kff_2011$MinConst <- ifelse(kff_2011$industry == 1 | kff_2011$industry == 2, 1, 0)
kff_2011$manufacturing <- ifelse(kff_2011$industry == 3, 1, 0)
kff_2011$transportutilcomms <- ifelse(kff_2011$industry == 4, 1, 0)
kff_2011$wholesale <- ifelse(kff_2011$industry == 5, 1, 0)
kff_2011$retail <- ifelse(kff_2011$industry == 6, 1, 0)
kff_2011$financial <- ifelse(kff_2011$industry == 7, 1, 0)
kff_2011$service <- ifelse(kff_2011$industry == 8, 1, 0)
kff_2011$government <- ifelse(kff_2011$industry == 9, 1, 0)
kff_2011$healthcare <- ifelse(kff_2011$industry == 10, 1, 0)

# step 3: Confirm correct cleaning
table(kff_2011$industry, kff_2011$MinConst)
table(kff_2011$industry, kff_2011$manufacturing)
table(kff_2011$industry, kff_2011$transportutilcomms)
table(kff_2011$industry, kff_2011$wholesale)
table(kff_2011$industry, kff_2011$retail)
table(kff_2011$industry, kff_2011$financial)
table(kff_2011$industry, kff_2011$service)
table(kff_2011$industry, kff_2011$government)
table(kff_2011$industry, kff_2011$healthcare)

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
table(kff_2012$industry, kff_2012$AgriMinConst)
table(kff_2012$industry, kff_2012$manufacturing)
table(kff_2012$industry, kff_2012$transportutilcomms)
table(kff_2012$industry, kff_2012$wholesale)
table(kff_2012$industry, kff_2012$retail)
table(kff_2012$industry, kff_2012$financial)
table(kff_2012$industry, kff_2012$service)
table(kff_2012$industry, kff_2012$government)
table(kff_2012$industry, kff_2012$healthcare)


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
table(kff_2013$industry, kff_2013$AgriMinConst)
table(kff_2013$industry, kff_2013$manufacturing)
table(kff_2013$industry, kff_2013$transportutilcomms)
table(kff_2013$industry, kff_2013$wholesale)
table(kff_2013$industry, kff_2013$retail)
table(kff_2013$industry, kff_2013$financial)
table(kff_2013$industry, kff_2013$service)
table(kff_2013$industry, kff_2013$government)
table(kff_2013$industry, kff_2013$healthcare)


## 2014 ##
# Step 1: Examine variable
table(kff_2014$industry, useNA = "ifany")

# Step 2: Clean variable (always create new variable!)
kff_2014$construction    <- ifelse(kff_2014$industry == 2,  1, 0)
kff_2014$manufacturing   <- ifelse(kff_2014$industry == 3,  1, 0)
kff_2014$transportutilcomms  <- ifelse(kff_2014$industry == 4,  1, 0)
kff_2014$wholesale       <- ifelse(kff_2014$industry == 5,  1, 0)
kff_2014$retail          <- ifelse(kff_2014$industry == 6,  1, 0)
kff_2014$financial       <- ifelse(kff_2014$industry == 7,  1, 0)
kff_2014$service         <- ifelse(kff_2014$industry == 8,  1, 0)
kff_2014$government      <- ifelse(kff_2014$industry == 9,  1, 0)
kff_2014$healthcare      <- ifelse(kff_2014$industry == 10, 1, 0)
  
# step 3: Confirm correct cleaning
table(kff_2014$industry, kff_2014$construction)
table(kff_2014$industry, kff_2014$manufacturing)
table(kff_2014$industry, kff_2014$transportutilcomms)
table(kff_2014$industry, kff_2014$wholesale)
table(kff_2014$industry, kff_2014$retail)
table(kff_2014$industry, kff_2014$financial)
table(kff_2014$industry, kff_2014$service)
table(kff_2014$industry, kff_2014$government)
table(kff_2014$industry, kff_2014$healthcare)


###### Clean for size (of industry) #####

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
table(kff_2003$size, kff_2003$small_firm)
table(kff_2003$size, kff_2003$medium_firm)
table(kff_2003$size, kff_2003$large_firm)

# categorical variable
table(kff_2003$size, kff_2003$firm_size)


### 2004
table(kff_2004$size) #check variable

#encode dummy variables for firm size
kff_2004$small_firm    <- ifelse(kff_2004$size <= 3,                      1, 0)
kff_2004$medium_firm   <- ifelse(kff_2004$size >  3 & kff_2004$size < 6,  1, 0)
kff_2004$large_firm    <- ifelse(kff_2004$size == 6,                      1, 0)

# Step 3: Confirm
table(kff_2004$size, kff_2004$small_firm)
table(kff_2004$size, kff_2004$medium_firm)
table(kff_2004$size, kff_2004$large_firm)


### 2005
table(kff_2005$size) #check variable

kff_2005$small_firm    <- ifelse(kff_2005$size <= 3,                      1, 0)
kff_2005$medium_firm   <- ifelse(kff_2005$size >  3 & kff_2005$size < 6,  1, 0)
kff_2005$large_firm    <- ifelse(kff_2005$size == 6,                      1, 0)

table(kff_2005$size, kff_2005$small_firm)
table(kff_2005$size, kff_2005$medium_firm)
table(kff_2005$size, kff_2005$large_firm)


### 2006
table(kff_2006$size) #check variable

kff_2006$small_firm    <- ifelse(kff_2006$size <= 3,                      1, 0)
kff_2006$medium_firm   <- ifelse(kff_2006$size >  3 & kff_2006$size < 6,  1, 0)
kff_2006$large_firm    <- ifelse(kff_2006$size == 6,                      1, 0)

table(kff_2006$size, kff_2006$small_firm)
table(kff_2006$size, kff_2006$medium_firm)
table(kff_2006$size, kff_2006$large_firm)


### 2007
table(kff_2007$size)

kff_2007$small_firm    <- ifelse(kff_2007$size <= 3,                      1, 0)
kff_2007$medium_firm   <- ifelse(kff_2007$size >  3 & kff_2007$size < 6,  1, 0)
kff_2007$large_firm    <- ifelse(kff_2007$size == 6,                      1, 0)

table(kff_2007$size, kff_2007$small_firm)
table(kff_2007$size, kff_2007$medium_firm)
table(kff_2007$size, kff_2007$large_firm)


### 2008
table(kff_2008$size)

kff_2008$small_firm    <- ifelse(kff_2008$size <= 3,                      1, 0)
kff_2008$medium_firm   <- ifelse(kff_2008$size >  3 & kff_2008$size < 6,  1, 0)
kff_2008$large_firm    <- ifelse(kff_2008$size == 6,                      1, 0)

table(kff_2008$size, kff_2008$small_firm)
table(kff_2008$size, kff_2008$medium_firm)
table(kff_2008$size, kff_2008$large_firm)


### 2009
table(kff_2009$size)

kff_2009$small_firm    <- ifelse(kff_2009$size <= 3,                      1, 0)
kff_2009$medium_firm   <- ifelse(kff_2009$size >  3 & kff_2009$size < 6,  1, 0)
kff_2009$large_firm    <- ifelse(kff_2009$size == 6,                      1, 0)

table(kff_2009$size, kff_2009$small_firm)
table(kff_2009$size, kff_2009$medium_firm)
table(kff_2009$size, kff_2009$large_firm)


### 2010
table(kff_2010$size)

kff_2010$small_firm    <- ifelse(kff_2010$size <= 3,                      1, 0)
kff_2010$medium_firm   <- ifelse(kff_2010$size >  3 & kff_2010$size < 6,  1, 0)
kff_2010$large_firm    <- ifelse(kff_2010$size == 6,                      1, 0)

table(kff_2010$size, kff_2010$small_firm)
table(kff_2010$size, kff_2010$medium_firm)
table(kff_2010$size, kff_2010$large_firm)


### 2011
table(kff_2011$size)

kff_2011$small_firm    <- ifelse(kff_2011$size <= 3,                      1, 0)
kff_2011$medium_firm   <- ifelse(kff_2011$size >  3 & kff_2011$size < 6,  1, 0)
kff_2011$large_firm    <- ifelse(kff_2011$size == 6,                      1, 0)

table(kff_2011$size, kff_2011$small_firm)
table(kff_2011$size, kff_2011$medium_firm)
table(kff_2011$size, kff_2011$large_firm)


### 2012
table(kff_2012$size)

kff_2012$small_firm    <- ifelse(kff_2012$size <= 3,                      1, 0)
kff_2012$medium_firm   <- ifelse(kff_2012$size >  3 & kff_2012$size < 6,  1, 0)
kff_2012$large_firm    <- ifelse(kff_2012$size == 6,                      1, 0)

table(kff_2012$size, kff_2012$small_firm)
table(kff_2012$size, kff_2012$medium_firm)
table(kff_2012$size, kff_2012$large_firm)


### 2013
table(kff_2013$size)

kff_2013$small_firm    <- ifelse(kff_2013$size <= 3,                      1, 0)
kff_2013$medium_firm   <- ifelse(kff_2013$size >  3 & kff_2013$size < 6,  1, 0)
kff_2013$large_firm    <- ifelse(kff_2013$size == 6,                      1, 0)

table(kff_2013$size, kff_2013$small_firm)
table(kff_2013$size, kff_2013$medium_firm)
table(kff_2013$size, kff_2013$large_firm)




####################################################################################
############              Phase 2: Data Merging        ############
####################################################################################

# Step 1: Create variable list that is consistent across all years 
my_varlist <- c("small_firm", "medium_firm", "large_firm",
                "MinConst", "manufacturing", "transportutilcomms",
                "wholesale", "retail", "financial", "service",
                "government", "healthcare", "offers", "doesnt_offer", "unsure_of_offer")



# Step 2: Complete case information for all variables in varlsit in 2004
kff_complete_case_2003 <- kff_2003 %>%
  select(all_of(my_varlist)) %>%
  filter(complete.cases(.))

kff_complete_case_2004 <- kff_2004 %>%
  select(all_of(my_varlist)) %>%
  filter(complete.cases(.))

kff_complete_case_2005 <- kff_2005 %>%
  select(all_of(my_varlist)) %>%
  filter(complete.cases(.))

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

kff_complete_case_2012 <- kff_2012 %>%
  select(all_of(my_varlist)) %>%
  filter(complete.cases(.))

kff_complete_case_2013 <- kff_2013 %>%
  select(all_of(my_varlist)) %>%
  filter(complete.cases(.))

kff_complete_case_2014 <- kff_2014 %>%
  select(all_of(my_varlist)) %>%
  filter(complete.cases(.))





# Step 3: Create Long Dataset merging all years
kff_wide_all_years <- bind_rows(
  kff_complete_case_2003 %>% mutate(year = 2003),
  kff_complete_case_2004 %>% mutate(year = 2004),
  kff_complete_case_2005 %>% mutate(year = 2005),
  kff_complete_case_2006 %>% mutate(year = 2006),
  kff_complete_case_2007 %>% mutate(year = 2007),
  kff_complete_case_2008 %>% mutate(year = 2008),
  kff_complete_case_2009 %>% mutate(year = 2009),
  kff_complete_case_2010 %>% mutate(year = 2010),
  kff_complete_case_2011 %>% mutate(year = 2011),
  kff_complete_case_2012 %>% mutate(year = 2012),
  kff_complete_case_2013 %>% mutate(year = 2013),
  kff_complete_case_2014 %>% mutate(year = 2014)
)
