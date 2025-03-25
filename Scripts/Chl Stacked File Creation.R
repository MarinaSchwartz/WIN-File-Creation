### Project Notes ----
#Before running, Change Month in file name for Stacked
#This script is for making a stacked file for use in Project Coast File Creation Code
#this is still writing dates YY not YYYY - can't solve here - fixed in Creation code

### WD and Packages ----

setwd("~/Documents/GitHub/WIN File Creation")
library(tidyverse)
library(readxl)
library(dplyr)
library(readr)
library(lubridate)
library(purrr)

### Read in Data ----

Stacked <- read_xlsx("Data/LW/CHL and Secchi WIN Stacked.xlsx", sheet = 1, col_types = "text")

### Data Prep

#filter out rows not CHL
CHL <- Stacked %>%
  filter(ADAPT_Analyte_ID %in% c("WIN-001","FL-BIO-004"))

CHL <- CHL %>%
  mutate(Result_Comments = trimws(as.character(Result_Comments))) %>%  # Ensure clean character values
  filter(Result_Comments != "Redo" | is.na(Result_Comments))  # Keep NAs
# Handle the Activity_Start_Date column

 CHL$Activity_Start_Date <- as.character(CHL$Activity_Start_Date)  # Ensure it's a character

# Check if Activity_Start_Date is numeric (Excel serial date)
if (any(grepl("^[0-9.]+$", CHL$Activity_Start_Date))) {
  # Convert numeric Excel serial date to Date, correcting the 1900 leap year bug
  CHL$Activity_Start_Date <- as.numeric(CHL$Activity_Start_Date)
  
  # Excel serial number conversion: If it's greater than or equal to 60, we subtract 1 day due to the leap year bug.
  #CHL$Activity_Start_Date <- ifelse(CHL$Activity_Start_Date >= 60, CHL$Activity_Start_Date - 1, CHL$Activity_Start_Date)
  
  # Convert serial date to Date, applying the correct origin (1904-01-01)
  CHL$Activity_Start_Date <- as.Date(CHL$Activity_Start_Date, origin = "1904-01-01")  # Correct origin
  CHL$Activity_Start_Date <- format(CHL$Activity_Start_Date, "%m/%d/%Y")  # Format as MM/DD/YYYY
} else {
  # If it's a text-based date, assume it is in 'MM/DD/YY' format and reformat it
  CHL$Activity_Start_Date <- as.Date(CHL$Activity_Start_Date, format = "%m/%d/%y")  # Parse date
  CHL$Activity_Start_Date <- format(CHL$Activity_Start_Date, "%m/%d/%Y")  # Format as MM/DD/YYYY
}

# Convert the activity time column 
CHL$Activity_Start_Time <- as.character(CHL$Activity_Start_Time)  # Ensure time is a character

# If time is stored as numeric (fraction of a day), convert it back to time format
if(any(grepl("^[0-9.]+$", CHL$Activity_Start_Time))) {  # Check if the column has numbers
  CHL$Activity_Start_Time <- as.numeric(CHL$Activity_Start_Time) * 86400  # Convert to seconds
  CHL$Activity_Start_Time <- format(as.POSIXct(CHL$Activity_Start_Time, origin = "1970-01-01", tz = "UTC"), "%I:%M:%S %p")
}

# Convert the prep time column 
CHL$Preparation_Time <- as.character(CHL$Preparation_Time)  # Ensure time is a character

# If time is stored as numeric (fraction of a day), convert it back to time format
if(any(grepl("^[0-9.]+$", CHL$Preparation_Time))) {  # Check if the column has numbers
  CHL$Preparation_Time <- as.numeric(CHL$Preparation_Time) * 86400  # Convert to seconds
  CHL$Preparation_Time <- format(as.POSIXct(CHL$Preparation_Time, origin = "1970-01-01", tz = "UTC"), "%I:%M:%S %p")
}

# Convert the analysis time column 
CHL$Analysis_Time <- as.character(CHL$Analysis_Time)  # Ensure time is a character

# If time is stored as numeric (fraction of a day), convert it back to time format
if(any(grepl("^[0-9.]+$", CHL$Analysis_Time))) {  # Check if the column has numbers
  CHL$Analysis_Time <- as.numeric(CHL$Analysis_Time) * 86400  # Convert to seconds
  CHL$Analysis_Time <- format(as.POSIXct(CHL$Analysis_Time, origin = "1970-01-01", tz = "UTC"), "%I:%M:%S %p")
}

# Handle the Preparation_Date column

 CHL$Preparation_Date <- as.character(CHL$Preparation_Date)  # Ensure it's a character

# Check if Preparation_Date is numeric (Excel serial date)
if (any(grepl("^[0-9.]+$", CHL$Preparation_Date))) {
  # Convert numeric Excel serial date to Date, correcting the 1900 leap year bug
  CHL$Preparation_Date <- as.numeric(CHL$Preparation_Date)
  
  # Excel serial number conversion: If it's greater than or equal to 60, we subtract 1 day due to the leap year bug. THis was returning dates that were 4 years early, hence the 1904
  #CHL$Preparation_Date <- ifelse(CHL$Preparation_Date >= 60, CHL$Preparation_Date - 1, CHL$Preparation_Date)
  
  # Convert serial date to Date, applying the correct origin (1904-01-01)
  CHL$Preparation_Date <- as.Date(CHL$Preparation_Date, origin = "1904-01-01")  # Correct origin
  CHL$Preparation_Date <- format(CHL$Preparation_Date, "%m/%d/%Y")  # Format as MM/DD/YYYY
} else {
  # If it's a text-based date, assume it is in 'MM/DD/YY' format and reformat it
  CHL$Preparation_Date <- as.Date(CHL$Preparation_Date, format = "%m/%d/%y")  # Parse date
  CHL$Preparation_Date <- format(CHL$Preparation_Date, "%m/%d/%Y")  # Format as MM/DD/YYYY
}

# Handle the Analysis_Date column

CHL$Analysis_Date <- as.character(CHL$Analysis_Date)  # Ensure it's a character

# Check if Analysis_Date is numeric (Excel serial date)
if (any(grepl("^[0-9.]+$", CHL$Analysis_Date))) {
  # Convert numeric Excel serial date to Date, correcting the 1900 leap year bug
  CHL$Analysis_Date <- as.numeric(CHL$Analysis_Date)
  
  # Excel serial number conversion: If it's greater than or equal to 60, we subtract 1 day due to the leap year bug.
  #CHL$Analysis_Date <- ifelse(CHL$Analysis_Date >= 60, CHL$Analysis_Date - 1, CHL$Analysis_Date)
  
  # Convert serial date to Date, applying the correct origin (1900-01-01)
  CHL$Analysis_Date <- as.Date(CHL$Analysis_Date, origin = "1904-01-01")  # Correct origin
  CHL$Analysis_Date <- format(CHL$Analysis_Date, "%m/%d/%Y")  # Format as MM/DD/YYYY
} else {
  # If it's a text-based date, assume it is in 'MM/DD/YY' format and reformat it
  CHL$Analysis_Date <- as.Date(CHL$Analysis_Date, format = "%m/%d/%y")  # Parse date
  CHL$Analysis_Date <- format(CHL$Analysis_Date, "%m/%d/%Y")  # Format as MM/DD/YYYY
}

#Round result value and total depth
CHL$Org_Result_Value <- as.numeric(CHL$Org_Result_Value)
CHL$Org_Result_Value <- round(CHL$Org_Result_Value, 0)
CHL$Total_Depth <- as.numeric(CHL$Total_Depth)
CHL$Total_Depth <- round(CHL$Total_Depth, 2)


### Export File ----

write.table(CHL, file = "Data/LW/CHL_STACKED.csv", sep = ",", na = "", row.names = FALSE, quote = FALSE)


