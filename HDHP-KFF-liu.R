####################################################################################
############       Liu & Sydnor (2022) 2015 Replication        ############
####################################################################################

library(dplyr)
library(tidyr)
library(haven)

# Step 1: Start with the 2015 KFF data already loaded in the main project
ls2015 <- kff_2015

# Step 2: Give every firm a unique ID 
# each row in this dataset represents one firm

ls2015 <- ls2015 %>%
  mutate(
    firm = row_number()
  )

# Step 3: Identify which firms offer an HDHP 
# in the original data: 1 or 2 = firm offers HDHP 
# we turn that into 1 = offers HDHP and 0 = does not offer HDHP 

ls2015 <- ls2015 %>%
  mutate(
    hdhp3 = if_else(hdhp %in% c(1, 2), 1, 0)
  )

# Step 4: Identify what combination of plans each firm offers
# the variables hmo, ppo, and pos tell us whether the firm offers an HMO, PPO or POS plan
# pty = plan type combination

ls2015 <- ls2015 %>%
  mutate(
    pty = case_when(
      
      # Only HDHP
      hdhp3 == 1 & hmo == 0 & ppo == 0 & pos == 0 ~ 1,
      
      # Only HMO
      hdhp3 == 0 & hmo == 1 & ppo == 0 & pos == 0 ~ 2,
      
      # Only PPO
      hdhp3 == 0 & hmo == 0 & ppo == 1 & pos == 0 ~ 3,
      
      # Only POS
      hdhp3 == 0 & hmo == 0 & ppo == 0 & pos == 1 ~ 4,
      
      # HDHP + HMO
      hdhp3 == 1 & hmo == 1 & ppo == 0 & pos == 0 ~ 5,
      
      # HDHP + PPO
      hdhp3 == 1 & hmo == 0 & ppo == 1 & pos == 0 ~ 6,
      
      # HDHP + POS
      hdhp3 == 1 & hmo == 0 & ppo == 0 & pos == 1 ~ 7,
      
      # HMO + PPO
      hdhp3 == 0 & hmo == 1 & ppo == 1 & pos == 0 ~ 8,
      
      # HMO + POS
      hdhp3 == 0 & hmo == 1 & ppo == 0 & pos == 1 ~ 9,
      
      # PPO + POS
      hdhp3 == 0 & hmo == 0 & ppo == 1 & pos == 1 ~ 10,
      
      # HMO + PPO + POS
      hdhp3 == 0 & hmo == 1 & ppo == 1 & pos == 1 ~ 11,
      
      # HDHP + PPO + POS
      hdhp3 == 1 & hmo == 0 & ppo == 1 & pos == 1 ~ 12,
      
      # HDHP + HMO + POS
      hdhp3 == 1 & hmo == 1 & ppo == 0 & pos == 1 ~ 13,
      
      # HDHP + HMO + PPO
      hdhp3 == 1 & hmo == 1 & ppo == 1 & pos == 0 ~ 14,
      
      # HDHP + HMO + PPO + POS
      hdhp3 == 1 & hmo == 1 & ppo == 1 & pos == 1 ~ 15,
      
      # Anything that does not fit the combinations above
      TRUE ~ 0
    )
  )

# Step 5: Keep only firms offering HDHP and exactly ONE other plan
# We only want firms with 
# pty = 5 -> HDHP + HMO
# pty = 6 -> HDHP + PPO 
# pty = 7 -> HDHP + POS

ls2015 <- ls2015 %>%
  filter(pty %in% c(5, 6, 7))

# Check how many firms remain (the Liu and Sydnor sample should have 417 firms)

cat(
  "Firms offering HDHP + exactly one other plan:",
  nrow(ls2015),
  "\n"
)

# Step 6: Remove firms with missing HSA/HRA contribution information
# variables g43 and g41 contain information about employer contributions 
# if both are missing, we do not have the contribution information needed for analysis, so we remove firms where both are missing
# we keep firms where at least one is available 

ls2015 <- ls2015 %>%
  filter(
    !(is.na(g43) & is.na(g41))
  )

# Step 7: Remove firms with missing policy information
# e6, f6, and d6 contain policy information for the non-HDHP plan slots
# g6 contains the corresponding HDHP policy information 
# so we remove a firm when 
# all of d6, e6, and f6 are missing OR g6 is missing (so we need policy info for both the non HDHP and the HDHP)

ls2015 <- ls2015 %>%
  filter(
    !(
      (is.na(e6) & is.na(f6) & is.na(d6))
      |
        is.na(g6)
    )
  )


# Step 8: Remove firms with missing MOOP information 
# MOOP - maximum out-of-pocket limit
# g9 (HDHP MOOP information), e9, d9, and f9 are the possible non-HDHP MOOP variables
# so we remove a firm when
# g9 is missing OR ALL of e9, d9, and f9 are missing (so we need MOOP info for both plans)

ls2015 <- ls2015 %>%
  filter(
    !(
      is.na(g9)
      |
        (is.na(e9) & is.na(d9) & is.na(f9))
    )
  )

# Step 9: Remove firms with unknown or very high MOOP 
# Liu and Sydnor remove firms when the MOOP info is unknown or above $6,850 (limit was the relevant HDHP legal cap for 2015)
# which variable is checked depends on the firm's plan combination 
# pty = 5 -> use d9 / d9bx
# pty = 6 --> use e9 / e9bx
# pty = 7 --> use f9 / f9bx
# g9 / g9bx are used for the HDHP.

# HDHP + PPO
ls2015 <- ls2015 %>%
  filter(
    !(
      (e9 == 2 | e9bx > 6850) &
        pty == 6
    )
  )


# HDHP + HMO
ls2015 <- ls2015 %>%
  filter(
    !(
      (d9 == 2 | d9bx > 6850) &
        pty == 5
    )
  )


# HDHP + POS
ls2015 <- ls2015 %>%
  filter(
    !(
      (f9 == 2 | f9bx > 6850) &
        pty == 7
    )
  )


# HDHP itself
ls2015 <- ls2015 %>%
  filter(
    !(g9 == 2 | g9bx > 6850)
  )


# Step 10: Remove firms with too many plan-policy blocks filled in
# a firm in this sample should have info for 1 non HDHP plan and one HDHP plan
# d6, e6, and f6 are the possible slots for the non HDHP plan
# so we remove firms where MORE THAN ONE of those non HDHP slots is filled in 

ls2015 <- ls2015 %>%
  filter(
    !(
      !is.na(e6) &
        (!is.na(d6) | !is.na(f6))
    )
  )

ls2015 <- ls2015 %>%
  filter(
    !(
      !is.na(d6) &
        (!is.na(e6) | !is.na(f6))
    )
  )

ls2015 <- ls2015 %>%
  filter(
    !(
      !is.na(f6) &
        (!is.na(d6) | !is.na(e6))
    )
  )

# Step 11: Remove one known data-entry problem 
# the original stata code specifically removes firm 560 because the same two plans were entered twice

ls2015 <- ls2015 %>%
  filter(firm != 560)

# Step 12: Check sample size

cat(
  "Firms remaining after these exclusions:",
  nrow(ls2015),
  "\n"
)
