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


### IN the working directory, 2021 and forward are CSV files. Update or fix.






####################################################################################
############              Phase 1: variable Cleaning        ############
####################################################################################

# follow three steps of cleaning data for each variable for each year
# step 1: examine the variable
# Step 2: Clean the variable (always create a new variable!)
# Step 3: Confirm cleaning was correct


###### Clean Percent of Workers with Health Benefits Covered in HDHP #####

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
kff_2003$mining <- ifelse(kff_2003$industry == 1, 1, 0)
kff_2003$construction <- ifelse(kff_2003$industry == 2, 1, 0)
kff_2003$manufacturing <- ifelse(kff_2003$industry == 3, 1, 0)
kff_2003$transportation <- ifelse(kff_2003$industry == 4, 1, 0)
kff_2003$wholesale <- ifelse(kff_2003$industry == 5, 1, 0)
kff_2003$retail <- ifelse(kff_2003$industry == 6, 1, 0)
kff_2003$financial <- ifelse(kff_2003$industry == 7, 1, 0)
kff_2003$service <- ifelse(kff_2003$industry == 8, 1, 0)
kff_2003$government <- ifelse(kff_2003$industry == 9, 1, 0)
kff_2003$healthcare <- ifelse(kff_2003$industry == 10, 1, 0)

# step 3: Confirm correct cleaning
table(kff_2003$industry, kff_2003$mining)
table(kff_2003$industry, kff_2003$construction)
table(kff_2003$industry, kff_2003$manufacturing)
table(kff_2003$industry, kff_2003$transportation)
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
kff_2004$mining <- ifelse(kff_2004$industry == 1, 1, 0)
kff_2004$construction <- ifelse(kff_2004$industry == 2, 1, 0)
kff_2004$manufacturing <- ifelse(kff_2004$industry == 3, 1, 0)
kff_2004$transportation <- ifelse(kff_2004$industry == 4, 1, 0)
kff_2004$wholesale <- ifelse(kff_2004$industry == 5, 1, 0)
kff_2004$retail <- ifelse(kff_2004$industry == 6, 1, 0)
kff_2004$financial <- ifelse(kff_2004$industry == 7, 1, 0)
kff_2004$service <- ifelse(kff_2004$industry == 8, 1, 0)
kff_2004$government <- ifelse(kff_2004$industry == 9, 1, 0)
kff_2004$healthcare <- ifelse(kff_2004$industry == 10, 1, 0)

# step 3: Confirm correct cleaning
table(kff_2004$industry, kff_2004$mining)
table(kff_2004$industry, kff_2004$construction)
table(kff_2004$industry, kff_2004$manufacturing)
table(kff_2004$industry, kff_2004$transportation)
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
kff_2005$mining <- ifelse(kff_2005$industry == 1, 1, 0)
kff_2005$construction <- ifelse(kff_2005$industry == 2, 1, 0)
kff_2005$manufacturing <- ifelse(kff_2005$industry == 3, 1, 0)
kff_2005$transportation <- ifelse(kff_2005$industry == 4, 1, 0)
kff_2005$wholesale <- ifelse(kff_2005$industry == 5, 1, 0)
kff_2005$retail <- ifelse(kff_2005$industry == 6, 1, 0)
kff_2005$financial <- ifelse(kff_2005$industry == 7, 1, 0)
kff_2005$service <- ifelse(kff_2005$industry == 8, 1, 0)
kff_2005$government <- ifelse(kff_2005$industry == 9, 1, 0)
kff_2005$healthcare <- ifelse(kff_2005$industry == 10, 1, 0)

# step 3: Confirm correct cleaning
table(kff_2005$industry, kff_2005$mining)
table(kff_2005$industry, kff_2005$construction)
table(kff_2005$industry, kff_2005$manufacturing)
table(kff_2005$industry, kff_2005$transportation)
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
kff_2006$mining <- ifelse(kff_2006$industry == 1, 1, 0)
kff_2006$construction <- ifelse(kff_2006$industry == 2, 1, 0)
kff_2006$manufacturing <- ifelse(kff_2006$industry == 3, 1, 0)
kff_2006$transportation <- ifelse(kff_2006$industry == 4, 1, 0)
kff_2006$wholesale <- ifelse(kff_2006$industry == 5, 1, 0)
kff_2006$retail <- ifelse(kff_2006$industry == 6, 1, 0)
kff_2006$financial <- ifelse(kff_2006$industry == 7, 1, 0)
kff_2006$service <- ifelse(kff_2006$industry == 8, 1, 0)
kff_2006$government <- ifelse(kff_2006$industry == 9, 1, 0)
kff_2006$healthcare <- ifelse(kff_2006$industry == 10, 1, 0)

# step 3: Confirm correct cleaning
table(kff_2006$industry, kff_2006$mining)
table(kff_2006$industry, kff_2006$construction)
table(kff_2006$industry, kff_2006$manufacturing)
table(kff_2006$industry, kff_2006$transportation)
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
kff_2007$mining <- ifelse(kff_2007$industry == 1, 1, 0)
kff_2007$construction <- ifelse(kff_2007$industry == 2, 1, 0)
kff_2007$manufacturing <- ifelse(kff_2007$industry == 3, 1, 0)
kff_2007$transportation <- ifelse(kff_2007$industry == 4, 1, 0)
kff_2007$wholesale <- ifelse(kff_2007$industry == 5, 1, 0)
kff_2007$retail <- ifelse(kff_2007$industry == 6, 1, 0)
kff_2007$financial <- ifelse(kff_2007$industry == 7, 1, 0)
kff_2007$service <- ifelse(kff_2007$industry == 8, 1, 0)
kff_2007$government <- ifelse(kff_2007$industry == 9, 1, 0)
kff_2007$healthcare <- ifelse(kff_2007$industry == 10, 1, 0)

# step 3: Confirm correct cleaning
table(kff_2007$industry, kff_2007$mining)
table(kff_2007$industry, kff_2007$construction)
table(kff_2007$industry, kff_2007$manufacturing)
table(kff_2007$industry, kff_2007$transportation)
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
kff_2008$mining <- ifelse(kff_2008$industry == 1, 1, 0)
kff_2008$construction <- ifelse(kff_2008$industry == 2, 1, 0)
kff_2008$manufacturing <- ifelse(kff_2008$industry == 3, 1, 0)
kff_2008$transportation <- ifelse(kff_2008$industry == 4, 1, 0)
kff_2008$wholesale <- ifelse(kff_2008$industry == 5, 1, 0)
kff_2008$retail <- ifelse(kff_2008$industry == 6, 1, 0)
kff_2008$financial <- ifelse(kff_2008$industry == 7, 1, 0)
kff_2008$service <- ifelse(kff_2008$industry == 8, 1, 0)
kff_2008$government <- ifelse(kff_2008$industry == 9, 1, 0)
kff_2008$healthcare <- ifelse(kff_2008$industry == 10, 1, 0)

# step 3: Confirm correct cleaning
table(kff_2008$industry, kff_2008$mining)
table(kff_2008$industry, kff_2008$construction)
table(kff_2008$industry, kff_2008$manufacturing)
table(kff_2008$industry, kff_2008$transportation)
table(kff_2008$industry, kff_2008$wholesale)
table(kff_2008$industry, kff_2008$retail)
table(kff_2008$industry, kff_2008$financial)
table(kff_2008$industry, kff_2008$service)
table(kff_2008$industry, kff_2008$government)
table(kff_2008$industry, kff_2008$healthcare)






## 2014 ##
# Step 1: Examine variable
table(kff_2014$industry, useNA = "ifany")

# Step 2: Clean variable (always create new variable!)
kff_2014$construction <- ifelse(kff_2014$industry == 2, 1, 0)
kff_2014$manufacturing <- ifelse(kff_2014$industry == 3, 1, 0)
kff_2014$transportation <- ifelse(kff_2014$industry == 4, 1, 0)
kff_2014$wholesale <- ifelse(kff_2014$industry == 5, 1, 0)
kff_2014$retail <- ifelse(kff_2014$industry == 6, 1, 0)
kff_2014$financial <- ifelse(kff_2014$industry == 7, 1, 0)
kff_2014$service <- ifelse(kff_2014$industry == 8, 1, 0)
kff_2014$government <- ifelse(kff_2014$industry == 9, 1, 0)
kff_2014$healthcare <- ifelse(kff_2014$industry == 10, 1, 0)
  
# step 3: Confirm correct cleaning
table(kff_2014$industry, kff_2014$construction)
table(kff_2014$industry, kff_2014$manufacturing)
table(kff_2014$industry, kff_2014$transportation)
table(kff_2014$industry, kff_2014$wholesale)
table(kff_2014$industry, kff_2014$retail)
table(kff_2014$industry, kff_2014$financial)
table(kff_2014$industry, kff_2014$service)
table(kff_2014$industry, kff_2014$government)
table(kff_2014$industry, kff_2014$healthcare)





####################################################################################
############              Phase 2: Data Merging        ############
####################################################################################

# Step2 1: Complete case information for each year (drop all missing cases)




# Step 2: Create Wide Dataset merging all years




# Step 3: Create Long Dataset merging all years