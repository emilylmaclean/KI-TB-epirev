## To make a new GitHub repo to save this
# library(usethis)
# use_github()


#### DATA CLEANING FOR KIRIBATI TB NOTIFICATION DATA 

## Last updated on 18 December 2023

### Load packages
library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)
library(readxl)
library(janitor)
library(skimr)
library(purrr)



#### CLEAN DATA SO WE CAN DO ANALYSES

#### Load long and wide spreadsheet of all cases 
kiribati <- read.csv("C:/Users/emac7945/OneDrive - The University of Sydney (Staff)/USyd_Projects/Analysis_Kiribati/KI-TB-epirev/Data to use/20231218_kiribati TB cases_EM_no names.csv")

kir <- kiribati

skim(kir)
str(kir)


##
#### VARIABLE: `yr`
##

## `yr` columns - clean up to `year`
kir$year <- kir$yr
kir <- kir %>% 
  select(-yr)



##
#### NEW VARIABLE: `tb_no_year`
##

## `tb_no_year` should be paste0 of tb_no and yr
# If row doesn't have tb_no, then it's not a row that includes data - removed n=1924 rows
# Check contents of rows without a TB number
check <- kir %>%
  filter(is.na(tb_no))
rm(check)

# Run
kir <- kir %>% 
  filter(!is.na(tb_no))

## Need to add 0s before tb_no so there will always be 4 digits
kir$tb_no <- str_pad(kir$tb_no, 4, pad = "0")

# Then paste all together
kir$tb_no_year <- paste0(kir$year, kir$tb_no, sep = "")
head(kir$tb_no_year)

# Re-order dataset so `tb_no_year` is first column
kir <- kir %>%
  select(tb_no_year, everything())



##
#### VARIABLE: `date_registered` (date registered by NTP)
##

## Clean dates as much as possible first 
# Inspect
head(kir$date_registered)

# Use lubridate (n=5909 failed to parse)
kir$registered1 <- lubridate::dmy(kir$date_registered)

# Janitor package can help convert 'numeric' dates to date format (n=630 failed to parse)
kir <- kir %>%
  mutate(registered2 = janitor::convert_to_date(date_registered, 
                                           string_conversion_failure = "warning"))

# Create a new variable based on `registered1` and `registered2`
# Need to convert to characters so copying is more reliable (dates don't copy well)
kir <- kir %>%
  mutate(across(c(registered1, registered2), as.character))

# New variable to take dates from `registered1` whenever possible, if it's NA then `registered2`
kir$registered3 <- ifelse(is.na(kir$registered1), 
                          kir$registered2, 
                          kir$registered1)

## Assign value of `date_started` to be correct `registered3`, convert to YMD, drop working vars
kir <- kir %>%
  mutate(date_registered = registered3) %>%
  mutate(date_registered = lubridate::ymd(date_registered)) %>%
  select(-c(registered1, registered2, registered3))
str(kir)

table(kir$date_registered)

# Table of cases with missing `date_registered` (n=19)
miss_datereg <- kir %>%
  filter(is.na(date_registered))



##
#### VARIABLE: `date_started` (date treatment started)
##

# Clean dates as much as possible first - use lubridate
kir$start1 <- lubridate::dmy(kir$date_started)

# Janitor package can help convert 'numeric' dates to date format
kir <- kir %>%
  mutate(start2 = janitor::convert_to_date(date_started, 
                                           string_conversion_failure = "warning"))

# Create a new variable based on `start1` and `start2`
# Need to convert to characters so copying is more reliable (dates don't copy well)
kir <- kir %>%
  mutate(across(c(start1, start2), as.character))

# New variable to take dates from start1 whenever possible, if it's NA then `start2`
kir$start3 <- ifelse(is.na(kir$start1), kir$start2, kir$start1)

## Assign value of `date_started` to be correct `start3`, convert to YMD, drop working vars
kir <- kir %>%
  mutate(date_started = start3) %>%
  mutate(date_started = lubridate::ymd(date_started)) %>%
  select(-c(start1, start2, start3))
str(kir)

table(kir$date_started)

## Table of cases with missing treatment start dates (n=34)
miss_datestart <- kir %>%
  filter(is.na(date_started))



##
#### VARIABLES: TREATMENT OUTCOME AND TREATMENT OUTCOME DATES
##


##
#### `outcome_cured` (date)

head(kir$outcome_cured)

# Clean dates as much as possible first - use lubridate (n=1160 failed to parse)
kir$cured1 <- lubridate::dmy(kir$outcome_cured)

# Janitor package can help convert 'numeric' dates to date format (n=125 failed to parse)
kir <- kir %>%
  mutate(cured2 = janitor::convert_to_date(outcome_cured, 
                                           string_conversion_failure = "warning"))

# Create a new variable based on `cured1` and `cured2`
# Need to convert to characters so copying is more reliable (dates don't copy well)
kir <- kir %>%
  mutate(across(c(cured1, cured2), as.character))

# New variable to take dates from `cured1` whenever possible, if it's NA then `cured2`
kir$cured3 <- ifelse(is.na(kir$cured1), 
                     kir$cured2, 
                     kir$cured1)

## Assign value of `date_cured` to be corrected `cured3`, convert to YMD, drop working vars
kir <- kir %>%
  mutate(date_cured = cured3) %>%
  mutate(date_cured = lubridate::ymd(date_cured)) %>%
  select(-c(cured1, cured2, cured3))
str(kir)

table(kir$date_cured)



##
#### `outcome_complete` (date)

head(kir$outcome_complete)

# Clean dates as much as possible first - use lubridate (n=2620 failed to parse)
kir$complete1 <- lubridate::dmy(kir$outcome_complete)

# Janitor package can help convert 'numeric' dates to date format (n=350 failed to parse)
kir <- kir %>%
  mutate(complete2 = janitor::convert_to_date(outcome_complete, 
                                           string_conversion_failure = "warning"))

# Create a new variable based on `complete1` and `complete2`
# Need to convert to characters so copying is more reliable (dates don't copy well)
kir <- kir %>%
  mutate(across(c(complete1, complete2), as.character))

# New variable to take dates from `complete1` whenever possible, if it's NA then `complete2`
kir$complete3 <- ifelse(is.na(kir$complete1), 
                     kir$complete2, 
                     kir$complete1)

## Assign value of `date_complete` to be correct `complete3`, convert to YMD, drop working vars
kir <- kir %>%
  mutate(date_complete = complete3) %>%
  mutate(date_complete = lubridate::ymd(date_complete)) %>%
  select(-c(complete1, complete2, complete3))
str(kir)

table(kir$date_complete)


##
#### `outcome_failure` (date)

head(kir$outcome_failure)

# Clean dates as much as possible first - use lubridate (n=14 failed to parse)
kir$failure1 <- lubridate::dmy(kir$outcome_failure)

# Janitor package can help convert 'numeric' dates to date format (n=4 failed to parse)
kir <- kir %>%
  mutate(failure2 = janitor::convert_to_date(outcome_failure, 
                                              string_conversion_failure = "warning"))

# Create a new variable based on `failure1` and `failure1`
# Need to convert to characters so copying is more reliable (dates don't copy well)
kir <- kir %>%
  mutate(across(c(failure1, failure2), as.character))

# New variable to take dates from `failure1` whenever possible, if it's NA then `failure2`
kir$failure3 <- ifelse(is.na(kir$failure1), 
                        kir$failure2, 
                        kir$failure1)

## Assign value of `date_failure` to be correct `failure3`, convert to YMD, drop working vars
kir <- kir %>%
  mutate(date_failure = failure3) %>%
  mutate(date_failure = lubridate::ymd(date_failure)) %>%
  select(-c(failure1, failure2, failure3))
str(kir)

table(kir$date_failure)


##
#### `outcome_died` (date)

head(kir$outcome_died)

# Clean dates as much as possible first - use lubridate (n=14 failed to parse)
kir$died1 <- lubridate::dmy(kir$outcome_died)

# Janitor package can help convert 'numeric' dates to date format (n=4 failed to parse)
kir <- kir %>%
  mutate(died2 = janitor::convert_to_date(outcome_died, 
                                             string_conversion_failure = "warning"))

# Create a new variable based on `died1` and `died1`
# Need to convert to characters so copying is more reliable (dates don't copy well)
kir <- kir %>%
  mutate(across(c(died1, died2), as.character))

# New variable to take dates from `died1` whenever possible, if it's NA then `died2`
kir$died3 <- ifelse(is.na(kir$died1), 
                       kir$died2, 
                       kir$died1)

## Assign value of `date_died` to be correct `died3`, convert to YMD, drop working vars
kir <- kir %>%
  mutate(date_died = died3) %>%
  mutate(date_died = lubridate::ymd(date_died)) %>%
  select(-c(died1, died2, died3))
str(kir)

table(kir$date_died)


##
#### `outcome_ltfu` (date)

head(kir$outcome_ltfu)

# Clean dates as much as possible first - use lubridate (n=282 failed to parse)
kir$ltfu1 <- lubridate::dmy(kir$outcome_ltfu)

# Janitor package can help convert 'numeric' dates to date format (n=91 failed to parse)
kir <- kir %>%
  mutate(ltfu2 = janitor::convert_to_date(outcome_ltfu, 
                                          string_conversion_failure = "warning"))

# Create a new variable based on `ltfu1` and `ltfu1`
# Need to convert to characters so copying is more reliable (dates don't copy well)
kir <- kir %>%
  mutate(across(c(ltfu1, ltfu2), as.character))

# New variable to take dates from `died1` whenever possible, if it's NA then `ltfu2`
kir$ltfu3 <- ifelse(is.na(kir$ltfu1), 
                    kir$ltfu2, 
                    kir$ltfu1)

## Assign value of `date_ltfu` to be correct `ltfu3`, convert to YMD, drop working vars
kir <- kir %>%
  mutate(date_ltfu = ltfu3) %>%
  mutate(date_ltfu = lubridate::ymd(date_ltfu)) %>%
  select(-c(ltfu1, ltfu2, ltfu3))
str(kir)

table(kir$date_ltfu)


##
#### `outcome_tf_out` (date)

head(kir$outcome_tf_out)

# Clean dates as much as possible first - use lubridate (n=29 failed to parse)
kir$tf_out1 <- lubridate::dmy(kir$outcome_tf_out)

# Janitor package can help convert 'numeric' dates to date format (n=14 failed to parse)
kir <- kir %>%
  mutate(tf_out2 = janitor::convert_to_date(outcome_tf_out, 
                                          string_conversion_failure = "warning"))

# Create a new variable based on `tf_out1` and `tf_out1`
# Need to convert to characters so copying is more reliable (dates don't copy well)
kir <- kir %>%
  mutate(across(c(tf_out1, tf_out2), as.character))

# New variable to take dates from `tf_out1` whenever possible, if it's NA then `tf_out2`
kir$tf_out3 <- ifelse(is.na(kir$tf_out1), 
                    kir$tf_out2, 
                    kir$tf_out1)

## Assign value of `date_tf_out` to be correct `tf_out3`, convert to YMD, drop working vars
kir <- kir %>%
  mutate(date_tf_out = tf_out3) %>%
  mutate(date_tf_out = lubridate::ymd(date_tf_out)) %>%
  select(-c(tf_out1, tf_out2, tf_out3))
str(kir)

table(kir$date_tf_out)


### Make one new variable for treatment outcome
# Hierarchy in order of outcomes below (i.e., if >1 outcome reported)

kir <- kir %>%
  mutate(treatment_outcome = case_when(!is.na(outcome_cured) ~ 'cured',
                                       !is.na(outcome_complete) ~ 'complete',
                                       !is.na(outcome_died) ~ 'died',
                                       !is.na(outcome_failure) ~ 'failure',
                                       !is.na(outcome_ltfu) ~ 'ltfu',
                                       !is.na(outcome_tf_out) ~ 'transfered',
                                       !is.na(outcome_not_evaluated_2017) ~ 'not_evald',
                                       TRUE ~ NA))


### Update the variable names we already have
# i.e., now that treatment outcome date variables are more accurately named
kir <- kir %>%
  mutate(outcome_not_evaluated_2017 = ifelse(!is.na(outcome_not_evaluated_2017), '1', '0')) %>%
  mutate(outcome_tf_out = ifelse(!is.na(outcome_tf_out), '1', '0')) %>%
  mutate(outcome_ltfu = ifelse(!is.na(outcome_ltfu), '1', '0')) %>%
  mutate(outcome_failure = ifelse(!is.na(outcome_failure), '1', '0')) %>%
  mutate(outcome_died = ifelse(!is.na(outcome_died), '1', '0')) %>%
  mutate(outcome_complete = ifelse(!is.na(outcome_complete), '1', '0'))  %>%
  mutate(outcome_cured = ifelse(!is.na(outcome_cured), '1', '0')) 
  
## Re-order so all data re: treatment and treatment outcomes and dates are alongside each other
kir <- kir %>%
  select(-treatment_unit, -regimen, -regimen_fld_2018, -regimen_sld_2018,
         -date_started, -treatment_outcome, 
         -outcome_cured, -date_cured, -outcome_complete, -date_complete,
         -outcome_died, -date_died, -outcome_failure, -date_failure,
         -outcome_ltfu, -date_ltfu, -outcome_tf_out, -date_tf_out,
         -outcome_not_evaluated_2017, -outcome_sld_2018,
         everything(), 
         treatment_unit, regimen, regimen_fld_2018, regimen_sld_2018,
         date_started, treatment_outcome, 
         outcome_cured, date_cured, outcome_complete, date_complete, 
         outcome_died, date_died, outcome_failure, date_failure, 
         outcome_ltfu, date_ltfu, outcome_tf_out, date_tf_out,
         outcome_not_evaluated_2017, outcome_sld_2018)



##
#### VARIABLES: SMEAR MICROSCOPY
##

##  These variables all hold multiple pieces of data, e.g., `sm_0_2018`

## Want to extract relevant info and store in '2023' variables
# Simplify variable containing mutliple pieces of information
# Extract test result
# Extract lab number if none provided (look for open bracket, look for closing bracket, grab middle)
# Extract date
# Remove old variables

##
#### `sm_0_2018`: data to `sm_0_2023` variables
kir <- kir %>%
  mutate(sm_0_2018 = str_to_lower(sm_0_2018)) %>%
  mutate(sm_0_result = str_to_lower(sm_0_result)) %>%
  mutate(sm_0_2018 = str_replace_all(sm_0_2018, " ", "")) %>%
  mutate(sm_0_date_2023 = str_extract(sm_0_2018, "\\d+/\\d+/\\d+")) %>%
  mutate(sm_0_labno_2023 = ifelse(is.na(sm_0_labno),
                                  str_extract(sm_0_2018, "(?<=\\().+?(?=\\))"),
                                  sm_0_labno)) %>%
  mutate(sm_0_result_2023 = ifelse(is.na(sm_0_result),
                                   str_extract(sm_0_2018, "nd|n.d|neg|sc|1\\+|2\\+|3\\+|\\d+afb/100field|\\d+afb100field|\\d+afb/100"),
                                   sm_0_result)) %>%
  select(-c(sm_0_2018, sm_0_result, sm_0_labno))

##
#### `sm_2_2018`: data to `sm_2_2023` variables
kir <- kir %>%
  mutate(sm_2_2018 = str_to_lower(sm_2_2018)) %>%
  mutate(sm_2_result = str_to_lower(sm_2_result)) %>%
  mutate(sm_2_2018 = str_replace_all(sm_2_2018, " ", "")) %>%
  mutate(sm_2_date_2023 = str_extract(sm_2_2018, "\\d+/\\d+/\\d+")) %>%
  mutate(sm_2_labno_2023 = ifelse(is.na(sm_2_labno),
                                  str_extract(sm_2_2018, "(?<=\\().+?(?=\\))"),
                                  sm_2_labno)) %>%
  mutate(sm_2_result_2023 = ifelse(is.na(sm_2_result),
                                   str_extract(sm_2_2018, "nd|n.d|neg|sc|tx|1\\+|2\\+|3\\+|\\d+afb/100field|\\d+afb100field|\\d+afb/100"),
                                   sm_2_result)) %>%
  select(-c(sm_2_2018, sm_2_result, sm_2_labno))

##
#### `sm_5_2018`: data to `sm_5_2023` variables
kir <- kir %>%
  mutate(sm_5_2018 = str_to_lower(sm_5_2018)) %>%
  mutate(sm_5_result = str_to_lower(sm_5_result)) %>%
  mutate(sm_5_2018 = str_replace_all(sm_5_2018, " ", "")) %>%
  mutate(sm_5_date_2023 = str_extract(sm_5_2018, "\\d+/\\d+/\\d+")) %>%
  mutate(sm_5_labno_2023 = ifelse(is.na(sm_5_labno),
                                  str_extract(sm_5_2018, "(?<=\\().+?(?=\\))"),
                                  sm_5_labno)) %>%
  mutate(sm_5_result_2023 = ifelse(is.na(sm_5_result),
                                   str_extract(sm_5_2018, "nd|n.d|neg|sc|tx|1\\+|2\\+|3\\+|\\d+afb/100field|\\d+afb100field|\\d+afb/100"),
                                   sm_5_result)) %>%
  select(-c(sm_5_2018, sm_5_result, sm_5_labno))

##
#### `sm_6`: data to `sm_6_2023` variables
kir <- kir %>%
  mutate(sm_6_result_2023 = sm_6_result) %>%
  mutate(sm_6_labno_2023 = sm_6_labno) %>%
  select(-c(sm_6_result, sm_6_labno))

##
#### `sm_8`: data to `sm_8_2023`` variables
kir <- kir %>%
  mutate(sm_8_result_2023 = sm_8_result) %>%
  mutate(sm_8_labno_2023 = sm_8_labno) %>%
  select(-c(sm_8_result, sm_8_labno))

##
#### `sm_end_2018`: data to `sm_end_2023` variables
kir <- kir %>%
  mutate(sm_end_2018 = str_to_lower(sm_end_2018)) %>%
  mutate(sm_end_2018 = str_replace_all(sm_end_2018, " ", "")) %>%
  mutate(sm_end_date_2023 = str_extract(sm_end_2018, "\\d+/\\d+/\\d+")) %>%
  mutate(sm_end_labno_2023 = str_extract(sm_end_2018, "(?<=\\().+?(?=\\))")) %>%
  mutate(sm_end_result_2023 = str_extract(sm_end_2018, "nd|n.d|neg|sc|tx|1\\+|2\\+|3\\+|\\d+afb/100field|\\d+afb100field|\\d+afb/100")) %>%
  select(-c(sm_end_2018))



##
#### VARIABLES: CULTURE
##

## These variables are mostly empty

## Will make generic code for general case, i.e., for the future
# Want to extract relevant info and store in '2023' variables

##
#### `c_0_2018`: data to `c_0_2023` variables
kir <- kir %>%
  mutate(c_0_2018 = str_to_lower(c_0_2018)) %>%
  mutate(c_0_2018 = str_replace_all(c_0_2018, " ", "")) %>%
  mutate(c_0_date_2023 = str_extract(c_0_2018, "\\d+/\\d+/\\d+")) %>%
  mutate(c_0_labno_2023 = str_extract(c_0_2018, "(?<=\\().+?(?=\\))")) %>%
  mutate(c_0_result_2023 = str_extract(c_0_2018, "nd|n.d|n/d|neg|sc|1\\+|2\\+|3\\+|\\d+afb/100field|\\d+afb100field|\\d+afb/100")) %>%
  select(-c(c_0_2018))

##
#### `c_2_2018`: data to `c_2_2023` variables
kir <- kir %>%
  mutate(c_2_2018 = str_to_lower(c_2_2018)) %>%
  mutate(c_2_2018 = str_replace_all(c_2_2018, " ", "")) %>%
  mutate(c_2_date_2023 = str_extract(c_2_2018, "\\d+/\\d+/\\d+")) %>%
  mutate(c_2_labno_2023 = str_extract(c_2_2018, "(?<=\\().+?(?=\\))")) %>%
  mutate(c_2_result_2023 = str_extract(c_2_2018, "nd|n.d|n/d|neg|sc|1\\+|2\\+|3\\+|\\d+afb/100field|\\d+afb100field|\\d+afb/100")) %>%
  select(-c(c_2_2018))

##
#### `c_5_2018`: data to `c_5_2023` variables
kir <- kir %>%
  mutate(c_5_2018 = str_to_lower(c_5_2018)) %>%
  mutate(c_5_2018 = str_replace_all(c_5_2018, " ", "")) %>%
  mutate(c_5_date_2023 = str_extract(c_5_2018, "\\d+/\\d+/\\d+")) %>%
  mutate(c_5_labno_2023 = str_extract(c_5_2018, "(?<=\\().+?(?=\\))")) %>%
  mutate(c_5_result_2023 = str_extract(c_5_2018, "nd|n.d|n/d|neg|sc|1\\+|2\\+|3\\+|\\d+afb/100field|\\d+afb100field|\\d+afb/100")) %>%
  select(-c(c_5_2018))

##
#### `c_end_2018`: data to c_end_2023 variables
kir <- kir %>%
  mutate(c_end_2018 = str_to_lower(c_end_2018)) %>%
  mutate(c_end_2018 = str_replace_all(c_end_2018, " ", "")) %>%
  mutate(c_end_date_2023 = str_extract(c_end_2018, "\\d+/\\d+/\\d+")) %>%
  mutate(c_end_labno_2023 = str_extract(c_end_2018, "(?<=\\().+?(?=\\))")) %>%
  mutate(c_end_result_2023 = str_extract(c_end_2018, "nd|n.d|n/d|neg|sc|1\\+|2\\+|3\\+|\\d+afb/100field|\\d+afb100field|\\d+afb/100")) %>%
  select(-c(c_end_2018))


##
#### VARIABLES: GENEXPERT MTB/RIF 
##


## I don't know what all the difference results mean, so for now:
## Extract date and delete those characters
## Extract lab number and delete those characters
## Remaining values are the 'results'

##
#### `gx_0_2018`: data to `gx_0_2023` variables
kir <- kir %>%
  mutate(gx_0_2018 = str_to_lower(gx_0_2018)) %>%
  mutate(gx_0_2018 = str_replace_all(gx_0_2018, " ", "")) %>%
  mutate(gx_0_date_2023 = str_extract(gx_0_2018, "\\d+/\\d+/\\d+")) %>%
  mutate(gx_0_2018 = str_replace(gx_0_2018, "\\d+/\\d+/\\d+", "")) %>%
  mutate(gx_0_labno_2023 = str_extract(gx_0_2018, "(?<=\\()[0-9]+?(?=\\))")) %>%
  mutate(gx_0_2018 = str_replace_all(gx_0_2018, "\\(\\d+\\)", "")) %>%
  mutate(gx_0_result_2023 = gx_0_2018) %>%
  select(-c(gx_0_2018))



##
#### VARIABLES: BACTERIOLOGICAL CONFIRMATION OR CLINICAL DIAGNOSIS INDICATION
##

## i.e., `disease_site_bc_2017` and `disease_site_cd_2017`

##
### Update variable to indicate disease site was bacteriologically confirmed
kir$disease_site_bc <- kir$disease_site_bc_2017
kir <- kir %>%
  select(-disease_site_bc_2017)

table(kir$disease_site_bc, useNA = 'ifany')

##
#### Update variable to indicate disease site was clinically diagnosed
kir$disease_site_cd <- kir$disease_site_cd_2017 
kir <- kir %>% 
  select(-disease_site_cd_2017)

table(kir$disease_site_cd, useNA = 'ifany')


## Harmonise disease site variables
# Ensure `disease_site_pulm_2018` is always a clear description (not "1")
# Fix inconsistencies in `disease_site_bc`
kir <- kir %>%
  mutate(disease_site_bc = str_to_lower(disease_site_bc)) %>%
  mutate(disease_site_pulm_2018 = str_to_lower(disease_site_pulm_2018)) %>%
  mutate(disease_site_cd = str_to_lower(disease_site_cd)) %>%
  mutate(disease_site_ep_2018 = str_to_lower(disease_site_ep_2018)) %>%
  mutate(disease_site = str_to_lower(disease_site)) %>%
  mutate(disease_site_pulm_2018 = replace(disease_site_pulm_2018,
                                          disease_site_pulm_2018 == "1",
                                          'ptb')) %>%
  mutate(disease_site_bc = replace(disease_site_bc,
                                        disease_site_bc == 'bd',
                                        'bc')) 
  

##
#### Update `disease_site`; all info in one variable 

## i.e., `disease_site`, `disease_site_pulm_2018`, `disease_site_ep_2018`

## Create one variable to indicate TB disease site
# Add `disease_site_pulm_2018` and `disease_site_ep_2018` to `disease_site` if NA only
table(kir$disease_site, useNA = 'ifany')

kir <- kir %>%
  mutate(disease_site = ifelse(is.na(disease_site),
                               disease_site_pulm_2018,
                               disease_site)) %>%
  mutate(disease_site = ifelse(is.na(disease_site),
                               disease_site_ep_2018,
                               disease_site)) 

table(kir$disease_site, useNA = 'ifany')
  
  
## Need to clean up categories of `disease_site`
kir <- kir %>%
  mutate(disease_site = str_to_lower(disease_site)) %>%
  mutate(disease_site = str_trim(disease_site)) %>%
  mutate(disease_site = recode(disease_site,
                               "tb hip" = "hip",
                               "menigitis" = "meningitis",
                               "miliary tb" = "miliary",
                               "mliary" = "miliary",
                              # "ptb -ve" = "ptb",
                               "ptb +ve" = "ptb",
                               "spine" = "spinal",
                               "tb spine" = "spinal",
                               "tb athritis" = "tb arthritis", 
                               "tb scrotum" = "scrotum"))
kir$disease_site[grepl("abd|adb", kir$disease_site, ignore.case = F)] <- "abdominal"
table(kir$disease_site)


## Create factor variable indicating PTB or EPTB
kir <- kir %>%
  mutate(disease_site_f = case_when(disease_site == "ptb" ~ 1,
                                    disease_site != "ptb" ~ 2))

kir$disease_site_f <- factor(kir$disease_site_f, 
                             levels = 1:2, 
                             labels = c("ptb",
                                        "eptb"))

table(kir$disease_site_f, useNA = 'ifany')


## *need further input from JH for collapsing categories

# Re-arrange for clarity
kir <- kir %>%
  select(tb_no_year, date_registered, month_ref, tb_no, name, sex, age, address, disease_site, 
         disease_site_bc, disease_site_cd, disease_site_pulm_2018, disease_site_ep_2018, everything())


##
#### VARIABLES: TREATMENT CATEGORIES 
##

##
#### `cat_new` : Treatment category - new
# Probably safe to assume that both '1' and 'N' mean new
table(kir$cat_new, useNA = 'ifany')
kir <- kir %>%
  mutate(cat_new = recode(cat_new,
                        'N' = '1'))

##
#### `cat_relapse` : Treatment category - relapse
# Probably safe to assume that both '1' and 'R' mean relapse
table(kir$cat_relapse, useNA = 'ifany')
kir <- kir %>%
  mutate(cat_relapse = recode(cat_relapse,
                              'R' = '1'))

##
#### `cat_failure` : Treatment category - treatment after failure
# Probably safe to assume that both '1' and 'TAF' mean treatment after failure
table(kir$cat_failure, useNA = 'ifany')
kir <- kir %>%
  mutate(cat_failure = recode(cat_failure,
                              'TAF' = '1'))

##
#### `cat_rad` : Treatment category - re-treatment after default (Is this the same as `cat_failure`?)
table(kir$cat_rad, useNA = 'ifany')

##
#### `cat_tf_in` : Treatment category - transfer in [free text]
table(kir$cat_tf_in, useNA = 'ifany')

##
#### `cat_others` : Treatment category - others
# Probably safe to assume '1' and 'O' mean 'other' category
table(kir$cat_others, useNA = 'ifany')
kir <- kir %>%
  mutate(cat_others = recode(cat_others,
                             'O' = '1'))



##
#### VARIABLE: KEY AFFECTED POPULATIONS AND HIGH RISK GROUPS STATUS
##

# Free text descriptions - could help with Diabetes variable
table(kir$hr_group_2021)



##
#### VARIABLE: DIABETES RELATED 
##

## i.e., `dm_status_2018`, `dm_tested_2020`
# Need clarity on what these mean
table(kir$dm_status_2018)

kir$dm_status_2018[kir$dm_status_2018 == 'N/A'] = NA

kir <- kir %>%
  mutate(dm_status_2018 = str_to_upper(dm_status_2018))

## Numeric values - can truncate
table(kir$dm_tested_2020)

kir <- kir %>%
  mutate(across(c('dm_tested_2020'), as.numeric)) %>%
  mutate(across(c('dm_tested_2020'), round, 4))



test <- kir %>%
  select(tb_no_year, hr_group_2021, dm_status_2018, dm_tested_2020)



##
#### VARIABLES: HIV RELATED 
##

##
#### `hiv_status_2018` (HIV status at time of diagnosis)

# Need clarity on what these mean
table(kir$hiv_status_2018, useNA = 'ifany')


##
#### `hiv_art_2018` (ART status) 

# This variable is entirely blank
table(kir$hiv_art_2018)


##
#### `hiv_cpt_2018` (cotrimoxazole preventive treatment status) 

# This variable is entirely blank
table(kir$hiv_cpt_2018)


##
#### VARIABLE: HEPATITIS B 
##

## i.e., hbv_status_2022 (Hepatitis B virus status)
# Need clarity on what these mean
table(kir$hbv_status_2022)


##
#### VARIABLE: DRUG RESISTANCE
##

# Check
table(kir$drtb_status_2018)
		
kir <- kir %>%
  mutate(drtb_status_2018 = recode(drtb_status_2018,
                                   '(R/R)' = 'RR',
                                   'RR/MDR 15/7/19' = 'RR/MDR'
                                   ))
	
## 
#### VARIABLE: WORKER AT HEALTH CARE FACILITIES
##

# Check
table(kir$hcw_2022, useNA = 'ifany')	

kir <- kir %>%
  mutate(hcw_2022 = recode(hcw_2022,
                           'Y' = '1'))

##
#### VARIABLE: DETECTED AS PART OF OUTREACH ACTIVITIES?
##

# Names of outreach activities 
table(kir$outreach_2022)




#### Re-arrange remarks column before exporting
kir <- kir %>%
  relocate(remarks, .after = everything()) 


#### Write clean-ish .csv
write.csv(kir, "231218_Kiribati TB notifications_cleaner.csv", row.names = F)

#### interjection - creating data dictionary ####
# variable is a column
# values of variable each listed in one row below column header
# e.g., treatment_unit will have 92 rows of values


library(labelled)

labelled::look_for(kir, details = 'full') %>%
  as_tibble()


unique(kir$sex)
unique(kir$treatment_unit)

dict <- list()















##### Basic analyses ####

### Examine annual TB rate per 10000 0

## Make dataset of just cases
cases_year <- kir %>%
  group_by(year) %>%
  summarise(n = n()) %>%
  mutate(propn = prop.table(n)) 
print(cases_year, n=24)

## Add column of population each year (World Bank)
# Import data from World Bank on population
kiri_pop <- read.csv("230712_Kiribati pop (World Bank).csv")
kiri_pop <- kiri_pop[ , 43:67]

kiri_pop_l <- kiri_pop %>%
  pivot_longer(cols = starts_with("X"))

cases_year$population <- kiri_pop_l$value

# Calculate rate per 100,000
cases_year <- cases_year %>%
  mutate(rate = n/population*10^5)

## Write .csv of cases per year
#write.csv(cases_year, "230712_TB cases in Kiribati per 100k.csv", row.names = F)

### Graph cases per 100,000 each year
ggplot(cases_year, 
       aes(x = year, y = rate)) + 
  geom_point(size = 4,
             color = "darkred",
             shape = "diamond") +
  xlab("Year") +
  ylab("Rate per 100,000") +
  ggtitle("Rate of TB per 100,000 in Kiribati, 1998-2022") +
  ylim(200, 500) +
  geom_vline(xintercept = 2020) +
  annotate(geom = "text", x = 2020, y = 460, label = "COVID-19",
                 color = "navy")

### Tabular analysis of EPTB cases each year
tab_site_year <- kir %>%
  group_by(year, disease_site_f) %>%
    summarise(n = n(),
            sum_na = sum(is.na(disease_site_f))) %>%
  mutate(propn = prop.table(n)) 
print(tab_site_year , n=50)


## Change data to a form ggplot2 wants
# Make long table wide so each year has own row
tab_site_year_wide <- tab_site_year %>%
  pivot_wider(id_cols = "year",
              names_from = disease_site_f,
              values_from = c(n, propn)) 

# Clean up and add row of total case count
tab_site_year_wide <- tab_site_year_wide %>%
  mutate(propn_EPTB = round(propn_EPTB, 3)) %>%
  mutate(propn_PTB = round(propn_PTB, 3)) %>%
  mutate(total_cases = sum(n_PTB, n_EPTB))

## Write .csv of cases per site
#write.csv(tab_site_year_wide, "230712_TB cases by site.csv", row.names = F)


#~#~# Export table to Word #~#~#
library(flextable)
ft <- flextable(data = tab_site_year_wide) %>% 
  theme_zebra %>% 
  autofit
ft

# Create a temp file
tmp <- tempfile(fileext = ".docx")

# Create a .docx file
library(officer)
read_docx() %>% 
  body_add_flextable(ft) %>% 
  print(target = tmp)

# open word document - find in AppData\Local\Temp\name 
browseURL(tmp)
#~#~#


### Graph cases and by EPTB and PTB each year

## Plot of overall cases over time
ggplot(tab_site_year_wide,
       aes(x = year, y = total_cases)) +
  geom_point(size = 4, shape = 19) +
  geom_smooth(linetype = "dashed",
              fill = "grey",
              colour = "darkgreen",
              alpha = 0.4) +
  labs(title = "Count of TB cases in Kiribati, 1998-2022",
       x = "Year",
       y = "Count") +
  xlim(1998, 2022) +
  ylim(0, 600) 

## Plot of EPTB vs PTB over time
library(RColorBrewer)

ggplot(kir,
       aes(x = year, fill = disease_site_f)) +
  geom_histogram(binwidth = 1,
                 alpha=0.4,
                 color = "black") +
  xlab("Year") +
  ylab("Count") +
  ggtitle("TB cases in Kiribati by type, 1998-2022") +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(fill = "Disease type")

  
#
  
table(kir$year)






### check population, make graph of rate of cases per quarter?



