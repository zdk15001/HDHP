## Project:  HDHP: Diffusion Index, Muse 2025
# Located:   ELSA HDHP Folder, Kline Project
# File Name: kline(working)HDHP_diffusion.R
# Date:      Last updated 2025_6_19
# Who:       Zachary Kline, Mina Guglietta, and Daniel Baron


####################################################################################
############              Pre-Analysis: settings, packages, and data    ############
####################################################################################

### Settings + Packages
setwd("/projects/kline-lab/HDHP")

#install.packages("dplyr")
#install.packages("readxl")
#install.packages("janitor")
#install.packages("ggplot2")

library(dplyr)
library(readxl)
library(janitor)
library(ggplot2)


### Load and filter data 
employee_benefits_data = read_excel(
  "employee-benefits-in-the-united-states-dataset.xlsx", 
  sheet = "Data") %>%
  clean_names()

#Gets rows with HDHP participation percent
HD_participate_percent = employee_benefits_data %>%
  filter(
    provision_code == 432,
    ownership_code %in% c(2, 3)) %>%
  mutate(
    year = as.numeric(year),
    estimate = as.numeric(estimate)
  )

##Industry specific
HD_participate_percent_industry = HD_participate_percent %>%
  filter(
    characteristic_category == "All workers",
    industry_code != 000000
  )

#Industry: Private Only
HD_participate_percent_industry_private = HD_participate_percent_industry %>%
  filter(
    ownership_code == 2
  )

#Industry: Public Only
HD_participate_percent_industry_public = HD_participate_percent_industry %>%
  filter(
    ownership_code == 3
  )

##Occupation specific
HD_participate_percent_occupation = HD_participate_percent %>%
  filter(
    characteristic_category == "All workers",
    occupation_code != 000000
  )

#Occupation: Private Only
HD_participate_percent_occupation_private = HD_participate_percent_occupation %>%
  filter(
    ownership_code == 2
  )

#Occupation: Public Only
HD_participate_percent_occupation_public = HD_participate_percent_occupation %>%
  filter(
    ownership_code == 3
  )

####################################################################################
############              Descriptive Statistics    ############
####################################################################################




### Facet wrap percentage with HDHP in every industry
ggplot(HD_participate_percent_industry, aes(x = year, y = estimate, group = industry)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ industry) +
  labs(
    title = "HDHP Usage by Industry (2014–2024)",
    x = "Year",
    y = "Percent of workers with HDHP"
  ) +
  theme_minimal()


##### Industry 1: Finance and Insurance
# Overall: Finance and insurance
ggplot(
  HD_participate_percent_industry %>%
    filter(industry == "Finance and insurance"),
  aes(x = year, y = estimate)
) +
  geom_line() +
  geom_point() +
  scale_x_continuous( breaks = 2014:2024) +
  geom_text(aes(label = estimate), vjust = -1.5, size = 3) +
  labs(
    title = "HDHP Usage: Finance and insurance", 
    x = "Year", 
    y = "Percent of workers with HDHP"
  ) +
  theme_minimal()


# Industry subgroup 1: Credit intermediation
ggplot(
  HD_participate_percent_industry %>%
    filter(industry == "Credit intermediation"),
  aes(x = year, y = estimate)
) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 2014:2024) +
  geom_text(aes(label = estimate), vjust = -1.5, size = 3) +
  labs(
    title = "HDHP Usage: Credit intermediation", 
    x = "Year", 
    y = "Percent of workers with HDHP"
  ) +
  theme_minimal()

# Industry 2: Education and health services
ggplot(
  HD_participate_percent_industry %>%
    filter(industry == "Education and health services"),
  aes(x = year, y = estimate)
) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 2014:2024) +
  geom_text(aes(label = estimate), vjust = -1.5, size = 3) +
  labs(
    title = "HDHP Usage: Education and health services", 
    x = "Year", 
    y = "Percent of workers with HDHP"
  ) +
  theme_minimal()

# Industry 3: Educational services
ggplot(
  HD_participate_percent_industry %>%
    filter(industry == "Educational services"),
  aes(x = year, y = estimate)
) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 2014:2024) +
  geom_text(aes(label = estimate), vjust = -1.5, size = 3) +
  labs(
    title = "HDHP Usage: Educational services", 
    x = "Year", 
    y = "Percent of workers with HDHP"
  ) +
  theme_minimal()


# Industry 5: Health care and social assistance
ggplot(
  HD_participate_percent_industry %>%
    filter(industry == "Health care and social assistance"),
  aes(x = year, y = estimate)
) +
  geom_line() +
  geom_point() +
  scale_x_continuous( breaks = 2014:2024) +
  geom_text(aes(label = estimate), vjust = -1.5, size = 3) +
  labs(
    title = "HDHP Usage: Health care and social assistance", 
    x = "Year", 
    y = "Percent of workers with HDHP"
  ) +
  theme_minimal()

# Industry 6: Information 
ggplot(
  HD_participate_percent_industry %>%
    filter(industry == "Information"),
  aes(x = year, y = estimate)
) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 2014:2024) +
  geom_text(aes(label = estimate), vjust = -1.5, size = 3) +
  labs(
    title = "HDHP Usage: Information", 
    x = "Year", 
    y = "Percent of workers with HDHP"
  ) +
  theme_minimal()

# Industry 7: Insurance carriers
ggplot(
  hHD_participate_percent_industry %>%
    filter(industry == "Insurance carriers"),
  aes(x = year, y = estimate)
) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 2014:2024) +
  geom_text(aes(label = estimate), vjust = -1.5, size = 3) +
  labs(
    title = "HDHP Usage: Insurance carriers", 
    x = "Year", 
    y = "Percent of workers with HDHP"
  ) +
  theme_minimal()

# Industry 8: Junior colleges, colleges, universities, and professional schools
ggplot(
  HD_participate_percent_industry %>%
    filter(industry == "Junior colleges, colleges, universities, and professional schools"),
  aes(x = year, y = estimate)
) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 2014:2024) +
  geom_text(aes(label = estimate), vjust = -1.5, size = 3) +
  labs(
    title = "HDHP Usage: Junior colleges, colleges, universities, and professional schools", 
    x = "Year", 
    y = "Percent of workers with HDHP"
  ) +
  theme_minimal()

# Industry 9: Manufacturing
ggplot(
  HD_participate_percent_industry %>%
    filter(industry == "Manufacturing"),
  aes(x = year, y = estimate)
) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 2014:2024) +
  geom_text(aes(label = estimate), vjust = -1.5, size = 3) +
  labs(
    title = "HDHP Usage: Manufacturing", 
    x = "Year", 
    y = "Percent of workers with HDHP"
  ) +
  theme_minimal()

# Industry 10: Retail trade 
ggplot(
  HD_participate_percent_industry %>%
    filter(industry == "Retail trade"),
  aes(x = year, y = estimate)
) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 2014:2024) +
  geom_text(aes(label = estimate), vjust = -1.5, size = 3) +
  labs(
    title = "HDHP Usage: Retail trade", 
    x = "Year", 
    y = "Percent of workers with HDHP"
  ) +
  theme_minimal()

# Industry 11: Trade, transportation, and utilities
ggplot(
  HD_participate_percent_industry %>%
    filter(industry == "Trade, transportation, and utilities"),
  aes(x = year, y = estimate)
) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 2014:2024) +
  geom_text(aes(label = estimate), vjust = -1.5, size = 3) +
  labs(
    title = "HDHP Usage: Trade, transportation, and utilities", 
    x = "Year", 
    y = "Percent of workers with HDHP"
  ) +
  theme_minimal()



### TESTING with functions

plot_hdhd_trend <- function(data, industry_name) {
  ggplot(
    data %>% filter(industry == industry_name),
    aes(x = year, y = estimate)
  ) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = 2014:2024) +
    geom_text(aes(label = estimate), vjust = -1.5, size = 3) +
    labs(
      title = paste("HDHP Usage:", industry_name), 
      x = "Year", 
      y = "Percent of workers with HDHP"
    ) +
    theme_minimal()
}


#for manufacturing
plot_hdhd_trend(HD_participate_percent_industry, 
                "Manufacturing")

####################################################################################
############              Creation of Index                             ############
####################################################################################

# define the survey years that are missing from the dataset (i.e. no data collected those years)
missing_years <- c(2016, 2019, 2022)

# extract a list of all unique industries represented in the dataset
industries <- unique(HD_participate_percent_industry$industry)

# build a complete grid of (industry × missing year) combinations
# this creates the skeleton of rows we’ll add to pad the data with NA values
na_rows <- expand.grid(
  industry = industries,   # every industry gets a row for each missing year
  year = missing_years     # each of the missing years will be represented
)

# get the names of all columns in the original dataset so we can match the structure exactly
cols <- names(HD_participate_percent_industry)

# for every column that exists in the original but is missing from na_rows — add it and fill with NA
# this ensures bind_rows() won't throw errors about mismatched structures
for (col in cols) {
  if (!col %in% names(na_rows)) {
    na_rows[[col]] <- NA  # fill with NA — doesn’t matter what type (we’ll fix it below)
  }
}

# explicitly set the 'estimate' column to numeric NA — makes sure math operations won’t break
na_rows$estimate <- NA_real_

# assign a default value to 'characteristic_category' so filler rows still pass any future filters
na_rows$characteristic_category <- "All workers"

# reorder the columns in na_rows to match the exact column order of the original dataframe
na_rows <- na_rows[, cols]

# combine the padded NA rows with the original dataset
# this keeps original rows intact while filling in the holes so lag logic won’t skip over missing years
HD_with_NA <- bind_rows(
  HD_participate_percent_industry,  # original survey data
  na_rows                           # added NA-patched rows
) %>%
  arrange(industry, year)  # sort within industry chronologically — needed for lag to work correctly

# now we calculate lagged estimates and flag if an increase happened across 1, 2, or 3-year gaps
industry_sorted <- HD_with_NA %>%
  group_by(industry) %>%  # group by industry so lag calculations stay within each industry
  mutate(
    
    # -- 1-year lag comparison --
    prev1 = lag(estimate, 1),  # get estimate from 1 year before (within same industry)
    increased1 = if_else(
      is.na(prev1) | is.na(estimate), NA_real_,  # if either value is missing → return NA
      if_else(estimate > prev1, 1, 0)            # if current estimate > previous → flag as 1 (increase), else 0
    ),
    
    # -- 2-year lag comparison --
    prev2 = lag(estimate, 2),  # go back 2 years
    increased2 = if_else(
      is.na(prev2) | is.na(estimate), NA_real_,  # same missing value check
      if_else(estimate > prev2, 1, 0)            # same increase flag logic
    ),
    
    # -- 3-year lag comparison --
    prev3 = lag(estimate, 3),  # go back 3 years
    increased3 = if_else(
      is.na(prev3) | is.na(estimate), NA_real_,
      if_else(estimate > prev3, 1, 0)
    )
  ) %>%
  ungroup()  # ungroup so we don’t accidentally carry grouping into later summaries

# summarize the 1-year diffusion index across all industries for each year
diffusion_index_1 <- industry_sorted %>%
  group_by(year) %>%  # group by year to calculate year-level diffusion
  summarize(
    diffusion_index = mean(increased1, na.rm = TRUE) * 100,  # % of industries that saw a 1-year increase
    count = sum(!is.na(increased1))                          # number of valid comparisons used
  )
print(diffusion_index_1)  #print the 1-year diffusion index table

# same summary for 2-year lag
diffusion_index_2 <- industry_sorted %>%
  group_by(year) %>%
  summarize(
    diffusion_index = mean(increased2, na.rm = TRUE) * 100,  # % of industries that increased vs. 2 years prior
    count = sum(!is.na(increased2))
  )
print(diffusion_index_2)  # print it

# same for 3-year lag
diffusion_index_3 <- industry_sorted %>%
  group_by(year) %>%
  summarize(
    diffusion_index = mean(increased3, na.rm = TRUE) * 100,  # % of industries showing 3-year improvement
    count = sum(!is.na(increased3))
  )
print(diffusion_index_3)  #print 3 year lag index



####################################################################################
############              Plot Diffusion Indices                              ############
####################################################################################

# convert year to numeric to avoid factor issues in plotting
diffusion_index_1$year <- as.numeric(as.character(diffusion_index_1$year))

# plot 1-year diffusion index over time
plot(
  diffusion_index_1$year, diffusion_index_1$diffusion_index,  # x = year, y = 1-year diffusion %
  type = "p",  # points only to start
  col = "blue",  # blue points for clarity
  pch = 19,  # solid circle points
  xlab = "Year",  # x-axis label
  ylab = "Diffusion Index (%)",  # y-axis label
  main = "1-Year Diffusion Index Over Time",  # plot title
  ylim = c(0, 100)  # fix y-axis range from 0 to 100
)

# draw lines connecting points, ignoring gaps (NA rows)
lines(clean_1$year, clean_1$diffusion_index, col = "blue", type = "l")

# add count labels just above points
text(
  clean_1$year, clean_1$diffusion_index + 4,
  labels = clean_1$count, col = "blue", cex = 0.8
)

grid()  # add gridlines for readability


# plot 2-year diffusion index over time
plot(
  diffusion_index_2$year, diffusion_index_2$diffusion_index,  # x = year, y = 2-year diffusion %
  type = "p",  # points only
  col = "darkgreen",  # dark green points for contrast
  pch = 19,  # solid circle points
  xlab = "Year",  # x-axis label
  ylab = "Diffusion Index (%)",  # y-axis label
  main = "2-Year Diffusion Index Over Time",  # plot title
  ylim = c(0, 100)  # fix y-axis range
)

# connect points with lines ignoring NA gaps
lines(clean_2$year, clean_2$diffusion_index, col = "darkgreen", type = "l")

# add count labels above points
text(
  clean_2$year, clean_2$diffusion_index + 4,
  labels = clean_2$count, col = "darkgreen", cex = 0.8
)

grid()  # add gridlines


# plot 3-year diffusion index over time
plot(
  diffusion_index_3$year, diffusion_index_3$diffusion_index,  # x = year, y = 3-year diffusion %
  type = "p",  # points only
  col = "purple",  # purple points for contrast
  pch = 19,  # solid circle points
  xlab = "Year",  # x-axis label
  ylab = "Diffusion Index (%)",  # y-axis label
  main = "3-Year Diffusion Index Over Time",  # plot title
  ylim = c(0, 100)  # fix y-axis range
)

# draw lines connecting points ignoring NA gaps
lines(clean_3$year, clean_3$diffusion_index, col = "purple", type = "l")

# add count labels above points
text(
  clean_3$year, clean_3$diffusion_index + 4,
  labels = clean_3$count, col = "purple", cex = 0.8
)

grid()  # add gridlines


####################################################################################
###########       Index and Plot for Private Sectors             ############
####################################################################################

# define the years missing from the survey data — these years have no data but we want to include them for proper diffusion index calculation
missing_years <- c(2016, 2019, 2022)

# pull out all unique industries from the private occupation dataset — we need every group to add filler rows correctly
private_industries <- unique(HD_participate_percent_occupation_private$industry)

# create a full grid of all combinations between private industries and the missing years
# this sets up empty rows (NAs) so our time series is continuous for all industries
na_rows_private <- expand.grid(#start of expand.grid
  industry = private_industries,  # every private industry to cover them all
  year = missing_years             # the missing years to fill in those blanks
)#end of expand.grid

# grab the column names from the original dataset to keep the new filler rows consistent
cols_private <- names(HD_participate_percent_occupation_private)

# loop through each original column name...
for (col in cols_private) { #start of for loop
  # ...and if it's not already in the filler rows dataframe, add it as a column full of NAs
  # this ensures the structure of filler rows matches the original data exactly
  if (!col %in% names(na_rows_private)) {#start of if statement
    na_rows_private[[col]] <- NA  # fill with NA as placeholder
  }#end of if statement
}#end of for loop

# specifically set columns that need proper types or default values
na_rows_private$estimate <- NA_real_                    # estimate needs to be numeric NA explicitly
na_rows_private$characteristic_category <- "All workers" # fill this column with default category text

# reorder columns in the filler rows to match exactly the original dataframe's order
na_rows_private <- na_rows_private[, cols_private]

# now combine the original dataset with the filler rows to patch those missing years
# this keeps all original data AND ensures continuity for lag calculations
HD_private_with_NA <- bind_rows(#start of bind rows
  HD_participate_percent_occupation_private,  # the real data
  na_rows_private                             # the filler rows with NAs
) %>%  #end of bind rows statement
  arrange(industry, year)  # sort by industry then year to prep for lag calculations

# group by industry so lag calculations happen inside each industry’s timeline, not mixing with others
industry_sorted_private <- HD_private_with_NA %>%
  group_by(industry) %>%  # keep each industry separate for proper lagging
  mutate( #start of mutate statement
    # calculate 1-year lag of the estimate to compare current year vs previous year
    prev1 = lag(estimate, 1),
    # create a flag that marks 1 if current estimate increased compared to previous year, 0 if not,
    # or NA if either value is missing — this tells us if diffusion is happening
    increased1 = if_else(
      is.na(prev1) | is.na(estimate), NA_real_,  # if missing data, flag as NA
      if_else(estimate > prev1, 1, 0)            # flag 1 if increased, else 0
    ),#end of mutate statement
    
    # same logic for 2-year lag: compare current to 2 years ago
    prev2 = lag(estimate, 2),
    increased2 = if_else(#start of else if
      is.na(prev2) | is.na(estimate), NA_real_, # if missing data, flag as NA
      if_else(estimate > prev2, 1, 0) # flag 1 if increased, else 0
    ), #end of else if 
    
    # same for 3-year lag: compare current to 3 years ago
    prev3 = lag(estimate, 3),
    increased3 = if_else( #start of else if
      is.na(prev3) | is.na(estimate), NA_real_,  # if missing data, flag as NA
      if_else(estimate > prev3, 1, 0)   # flag 1 if increased, else 0
    ) #end of else if 
  ) %>%# end of mutate statement
  ungroup()  # remove grouping so later steps can work on the whole dataset easily

# summarize the diffusion index for 1-year lag by year
# this calculates the % of industries that saw an increase vs last year, plus the count of industries with valid data used in this calculation
diffusion_index_1_private <- industry_sorted_private %>%
  group_by(year) %>%
  summarize( #start of summarize
    diffusion_index = mean(increased1, na.rm = TRUE) * 100,  # percent increased industries
    count = sum(!is.na(increased1))                          # number of industries counted
  ) #end of summarize
print(diffusion_index_1_private)  #print diffusion index

# repeat for 2-year lag diffusion index
diffusion_index_2_private <- industry_sorted_private %>%
  group_by(year) %>%
  summarize( #start of summarize
    diffusion_index = mean(increased2, na.rm = TRUE) * 100,
    count = sum(!is.na(increased2))
  ) #end of summarize
print(diffusion_index_2_private)  # print 2-year lag summary

# repeat for 3-year lag diffusion index
diffusion_index_3_private <- industry_sorted_private %>%
  group_by(year) %>%
  summarize( #start of summarize
    diffusion_index = mean(increased3, na.rm = TRUE) * 100,
    count = sum(!is.na(increased3))
  )#end of summarize
print(diffusion_index_3_private)  # print 3-year lag summary


##PLOT FOR DIFFUSION INDEX FOR PRIVATE SECTORS##

# plot 1-year diffusion index for private occupations over time
plot(
  diffusion_index_1_private$year,  # x-axis = year
  diffusion_index_1_private$diffusion_index,  # y-axis = calculated diffusion % for 1-year lag
  type = "p",  # just plotting points first (we’ll add connecting lines next)
  col = "blue",  # blue = consistent with 1-year lag color used before
  pch = 19,  # solid circle points — visually clear and consistent
  xlab = "Year",  # x-axis label
  ylab = "Diffusion Index (%)",  # y-axis label
  main = "1-Year Diffusion Index (Private Occupation)",  # plot title
  ylim = c(0, 100)  # fix y-axis range for interpretability across all plots
)

# connect the dots — BUT only where we have valid (non-NA) data
lines(
  diffusion_index_1_private %>% filter(!is.na(diffusion_index)) %>% pull(year),  # x-values without NA
  diffusion_index_1_private %>% filter(!is.na(diffusion_index)) %>% pull(diffusion_index),  # corresponding y-values
  col = "blue",  # same color for consistency
  type = "l"  # lines only — so this doesn't redraw points
)

# add text labels showing sample size for each year's index calculation
text(
  diffusion_index_1_private$year,  # label x-positions = years
  diffusion_index_1_private$diffusion_index + 4,  # bump y-position upward so labels don't overlap points
  labels = diffusion_index_1_private$count,  # text = # of industries used in that year's index
  col = "blue",  # label color = same as line/point
  cex = 0.8  # shrink font size slightly for readability
)

grid()  # overlay gridlines to make patterns easier to see



# plot 2-year diffusion index for private occupations
plot(
  diffusion_index_2_private$year,  # x-axis = year
  diffusion_index_2_private$diffusion_index,  # y-axis = calculated 2-year lag diffusion %
  type = "p",  # just points for now
  col = "darkgreen",  # dark green = new color for 2-year lag
  pch = 19,  # solid dots again — consistency matters
  xlab = "Year",
  ylab = "Diffusion Index (%)",
  main = "2-Year Diffusion Index (Private Occupation)",  # update plot title for 2-year lag
  ylim = c(0, 100)  # same y-limits to keep visual scale consistent
)

# draw line between non-NA points to avoid nonsense connections over missing data
lines(
  diffusion_index_2_private %>% filter(!is.na(diffusion_index)) %>% pull(year),
  diffusion_index_2_private %>% filter(!is.na(diffusion_index)) %>% pull(diffusion_index),
  col = "darkgreen",
  type = "l"
)

# add sample size above each point
text(
  diffusion_index_2_private$year,
  diffusion_index_2_private$diffusion_index + 4,
  labels = diffusion_index_2_private$count,
  col = "darkgreen",
  cex = 0.8
)

grid()  # gridlines to help the brain track movement over time



# plot 3-year diffusion index for private occupations
plot(
  diffusion_index_3_private$year,  # x = year
  diffusion_index_3_private$diffusion_index,  # y = diffusion % for 3-year lag
  type = "p",  # points first
  col = "purple",  # purple = distinctive + consistent with earlier graphs
  pch = 19,  # solid dots = same style
  xlab = "Year",
  ylab = "Diffusion Index (%)",
  main = "3-Year Diffusion Index (Private Occupation)",  # updated title
  ylim = c(0, 100)
)

# connect only valid data points to avoid lines over NA gaps
lines(
  diffusion_index_3_private %>% filter(!is.na(diffusion_index)) %>% pull(year),
  diffusion_index_3_private %>% filter(!is.na(diffusion_index)) %>% pull(diffusion_index),
  col = "purple",
  type = "l"
)

# label each point with sample size used to compute index
text(
  diffusion_index_3_private$year,
  diffusion_index_3_private$diffusion_index + 4,
  labels = diffusion_index_3_private$count,
  col = "purple",
  cex = 0.8
)

grid()  # wrap with gridlines for clean aesthetic and better comparison

  
####################################################################################
############              Creation of diffusion curve    ############
####################################################################################

# Create diffusion curve for Industry



