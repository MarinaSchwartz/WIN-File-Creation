### Project Notes ----
#Before running, change file path at start and end of code
#This script is for making a stacked file for use in  File Creation Code
#this is still writing dates YY not YYYY - can't solve here - fixed in Creation code

### WD and Packages ----

library(readxl) # For reading Excel files
library(dplyr) # For data manipulation
library(purrr) # For functional programming (map functions)
library(lubridate) # For date-time manipulation

### Read in Data ----

folder_path <- "/Users/mevanskeene/Desktop/WIN Uploads 2025/TP"

# Exclude temp files (~$) and select .xlsm files
file_list <- list.files(
  path = folder_path, 
  pattern = "^[^~].*\\.xlsx$",  
  full.names = TRUE
)

# Function to read an Excel sheet with all columns as text
read_as_text <- function(file) {
  col_names <- read_excel(file, sheet = "Phosphorus-Total", n_max = 1) %>% names()
  
  # Read the entire sheet with all columns as text
  df <- read_excel(file, sheet = "Phosphorus-Total", col_types = rep("text", length(col_names)))
  
  # Convert the prep time column 
  df$Preparation_Time <- as.character(df$Preparation_Time) # Ensure time is a character
  
  df <- df %>%
    mutate(Result_Comments = trimws(as.character(Result_Comments))) %>%  # Ensure clean character values
    filter(Result_Comments != "Redo" | is.na(Result_Comments))  # Keep NAs
  
  # If time is stored as numeric (fraction of a day), convert it back to time format
  if(any(grepl("^[0-9.]+$", df$Preparation_Time))) {  # Check if the column has numbers
    df$Preparation_Time <- as.numeric(df$Preparation_Time) * 86400  # Convert to seconds
    df$Preparation_Time <- format(as.POSIXct(df$Preparation_Time, origin = "1970-01-01", tz = "UTC"), "%I:%M:%S %p")
  }
  
  # Convert the analysis time column 
  df$Analysis_Time <- as.character(df$Analysis_Time)  # Ensure time is a character
  
  # If time is stored as numeric (fraction of a day), convert it back to time format
  if(any(grepl("^[0-9.]+$", df$Analysis_Time))) {  # Check if the column has numbers
    df$Analysis_Time <- as.numeric(df$Analysis_Time) * 86400  # Convert to seconds
    df$Analysis_Time <- format(as.POSIXct(df$Analysis_Time, origin = "1970-01-01", tz = "UTC"), "%I:%M:%S %p")
  }
  
  # Handle the Preparation_Date column
  
  # df$Preparation_Date <- as.character(df$Preparation_Date)  # Ensure it's a character
  
  # Check if Preparation_Date is numeric (Excel serial date)
  if (any(grepl("^[0-9.]+$", df$Preparation_Date))) {
    # Convert numeric Excel serial date to Date, correcting the 1900 leap year bug
    df$Preparation_Date <- as.numeric(df$Preparation_Date)
    
    # Excel serial number conversion: If it's greater than or equal to 60, we subtract 1 day due to the leap year bug. THis was returning dates that were 4 years early, hence the 1904
    #df$Preparation_Date <- ifelse(df$Preparation_Date >= 60, df$Preparation_Date - 1, df$Preparation_Date)
    
    # Convert serial date to Date, applying the correct origin (1904-01-01)
    df$Preparation_Date <- as.Date(df$Preparation_Date, origin = "1904-01-01")  # Correct origin
    df$Preparation_Date <- format(df$Preparation_Date, "%m/%d/%Y")  # Format as MM/DD/YYYY
  } else {
    # If it's a text-based date, assume it is in 'MM/DD/YY' format and reformat it
    df$Preparation_Date <- as.Date(df$Preparation_Date, format = "%m/%d/%y")  # Parse date
    df$Preparation_Date <- format(df$Preparation_Date, "%m/%d/%Y")  # Format as MM/DD/YYYY
  }
  
  # Handle the Analysis_Date column
  
  # df$Analysis_Date <- as.character(df$Analysis_Date)  # Ensure it's a character
  
  # Check if Analysis_Date is numeric (Excel serial date)
  if (any(grepl("^[0-9.]+$", df$Analysis_Date))) {
    # Convert numeric Excel serial date to Date, correcting the 1900 leap year bug
    df$Analysis_Date <- as.numeric(df$Analysis_Date)
    
    # Excel serial number conversion: If it's greater than or equal to 60, we subtract 1 day due to the leap year bug.
    #df$Analysis_Date <- ifelse(df$Analysis_Date >= 60, df$Analysis_Date - 1, df$Analysis_Date)
    
    # Convert serial date to Date, applying the correct origin (1904-01-01)
    df$Analysis_Date <- as.Date(df$Analysis_Date, origin = "1904-01-01")  # Correct origin
    df$Analysis_Date <- format(df$Analysis_Date, "%m/%d/%Y")  # Format as MM/DD/YYYY
  } else {
    # If it's a text-based date, assume it is in 'MM/DD/YY' format and reformat it
    df$Analysis_Date <- as.Date(df$Analysis_Date, format = "%m/%d/%y")  # Parse date
    df$Analysis_Date <- format(df$Analysis_Date, "%m/%d/%Y")  # Format as MM/DD/YYYY
  }
  
  # Handle the Activity_Start_Date column
  
  # df$Activity_Start_Date <- as.character(df$Activity_Start_Date)  # Ensure it's a character
  
  # Check if Activity_Start_Date is numeric (Excel serial date)
  if (any(grepl("^[0-9.]+$", df$Activity_Start_Date))) {
    # Convert numeric Excel serial date to Date, correcting the 1900 leap year bug
    df$Activity_Start_Date <- as.numeric(df$Activity_Start_Date)
    
    # Excel serial number conversion: If it's greater than or equal to 60, we subtract 1 day due to the leap year bug.
    #df$Activity_Start_Date <- ifelse(df$Activity_Start_Date >= 60, df$Activity_Start_Date - 1, df$Activity_Start_Date)
    
    # Convert serial date to Date, applying the correct origin (1904-01-01)
    df$Activity_Start_Date <- as.Date(df$Activity_Start_Date, origin = "1904-01-01")  # Correct origin
    df$Activity_Start_Date <- format(df$Activity_Start_Date, "%m/%d/%Y")  # Format as MM/DD/YYYY
  } else {
    # If it's a text-based date, assume it is in 'MM/DD/YY' format and reformat it
    df$Activity_Start_Date <- as.Date(df$Activity_Start_Date, format = "%m/%d/%y")  # Parse date
    df$Activity_Start_Date <- format(df$Activity_Start_Date, "%m/%d/%Y")  # Format as MM/DD/YYYY
  }
  
 # Round Abs and Result_Value to proper decimal places
  
  df$'Abs' <- as.numeric(df$'Abs')
  df$'Abs' <- round(df$'Abs', 5)
  
  df$'Result_Value(ug/L)' <- as.numeric(df$'Result_Value(ug/L)')
  df$'Result_Value(ug/L)' <- round(df$'Result_Value(ug/L)', 0)
  
  return(df)
}


# Read and combine all valid files
combined_data <- file_list %>%
  map_df(read_as_text)




write.table(combined_data, file = "Data/LW/TP_STACKED.csv", sep = ",", na = "", row.names = FALSE, quote = FALSE)

