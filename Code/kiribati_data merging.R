#### DATA MERGING FOR KIRIBATI TB NOTIFICATION DATA  

## Last updated on 18 December 2023
## Ensure it's most recent TB registry being uploaded
## Check all worksheets for 'extra' empty cells - these make extra variable

### Load packages
library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)
library(readxl)
library(janitor)
library(skimr)
library(purrr)



#### MERGE ALL SPREAD SHEETS OF WORKBOOK INTO ONE DATAFRAME 
### Use `rio` package - can select sheets by index
## Need to select sheets (sheet names are numbers, which is a problem for computations)
## Keep sheet names as identifier, bind sheets into one large dataframe

library(rio)

## Use `rio` package to create list of dfs, select sheets by index (26 sheets)
allsheets <- import_list("TB Register 230615-cleaning 230908 pw_open.xlsx", 
                         which = c(1:26))


## Make all data into characters so they can be bound together (all same class)
allsheets <- map(allsheets, ~ .x %>%
                   mutate(across(everything(), as.character)))


## Assign sheet names to each dataframe in the list (i.e., as sheet identifiers)
# Within the list of dfs, paste 'df_' prefix to sheet name (year)
# Extract each sheet in the list, with dr_year as identifier
for (i in seq_along(allsheets)) {
  assign(paste0("df_", names(allsheets)[i]), {
    d <- allsheets[[i]]
    d$yr <- names(allsheets)[i] # make a new var to indicate which year each row is from (from sheet name)
    d # return
  })
}


# Check the df is entirely characters
skim(df_2020)


## Bind rows using dplyr function bind_rows (will bind even if columns don't match)
# To bind, columns with same titles need to be same class
# That's why we previously had converted everything to characters

## ATTN: `TB Register 230615-cleaning 230908 pw`: 2005 workbook sheet has n=1564 variables 
# Delete extra columns and then bind rows


## Bind all data into one dataframe
kiribati_df <- bind_rows(df_1998, df_1999, df_2000, df_2001,
                         df_2002, df_2003, df_2004, df_2005,
                         df_2006, df_2007, df_2008, df_2009,
                         df_2010, df_2011, df_2012, df_2013,
                         df_2014, df_2015, df_2016, df_2017,
                         df_2018, df_2019, df_2020, df_2021,
                         df_2022, df_2023,
                         .id = NULL)


## Read in column names files
wbnames <- read.csv("databasenames.csv")

## Merge the names with all the data we have!
kiribati2 <- merge(kiribati_df, wbnames, all = T)

colnames(kiribati2) # Now have n=94 columns

# Check for deprecated columns - not there anymore
table(kiribati2$`(deprecated column)`) #tx outcomes
table(kiribati2$...36) # empty
table(kiribati2$...37) #tx outcomes


### Write .csv of the complete dataframe: TB cases 1998 to 2023 in Kiribati
write.csv(kiribati2, "20231218_kiribati TB cases_EM.csv", row.names = F)

