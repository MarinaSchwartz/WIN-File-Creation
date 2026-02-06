# Project Notes ----

   
  #For sampling agency names:
    
    #which lakes are actually St. Andrews, or is it just all Bay county like XT did it?

    #Sampling Agency Names : ST. ANDREW BAY RESOURCE MANAGEMENT ASSOCIATION INC. or Florida Lakewatch (Project ID: 21FLKWAT) or
    #CHOCTAWHATCHEE BASIN ALLIANCE (Project ID: BAYRALLY) 

  #Non-matches and recycling:

    #Subset these non matches and add them to a non match folder like always? Yes, for each upload, keep a running file for all non matches 
    #and code in a concat with non match before joining wiht secchi?
    #Also add by joining this secchi with old non matches for each run through this code
    #Since chls SHOULD be running about the same time as other parameters, just ignore non-matches? juice worth the squeeze??


#Before running this code, 
    #Copy the new version of files to Data folder using Data Import names
    #Adjust column names to work with code
    #rows without activity times from secchi and rows without filtering times from CHL files (both cor and un) will not go into WIN... 
    #What to do with these? Subset out?
    #Change output file names to new upload date



### WD and Packages ----

setwd("~/Documents/GitHub/WIN File Creation")
library(tidyverse)
library(readxl)
library(dplyr)
library(readr)
library(lubridate)
library(purrr)

### Data Import ---- 


Mon_ID <- read_csv("Data/LW/Lakewatch Monitoring Location ID.csv", col_types = cols(
  Preparation_Time = col_character(),
  Analysis_Time = col_character(),
))

TP <- read_csv("Data/LW/TP_Q3.csv", col_types = cols(
  Preparation_Time = col_character(),
  Analysis_Time = col_character(),
))

TP_NonMatch <- read_csv("Data/LW/Unmatched_TP_Q2.csv", col_types = cols(
  Preparation_Time = col_character(),
  Analysis_Time = col_character(),
))

TN <- read_csv("Data/LW/TN_Q3.csv", col_types = cols(
  Preparation_Time = col_character(),
  Analysis_Time = col_character(),
))

TN_NonMatch <- read_csv("Data/LW/Unmatched_TN_Q2.csv", col_types = cols(
  Preparation_Time = col_character(),
  Analysis_Time = col_character(),
))

Color <- read_csv("Data/LW/Color_Q3.csv", col_types = cols(
  Preparation_Time = col_character(),
  Analysis_Time = col_character(),
))

Color_NonMatch <- read_csv("Data/LW/Unmatched_Color_Q2.csv", col_types = cols(
  Preparation_Time = col_character(),
  Analysis_Time = col_character(),
))

Cond <- read_csv("Data/LW/Cond_Q3.csv", col_types = cols(
  Preparation_Time = col_character(),
  Analysis_Time = col_character(),
))

Cond_NonMatch <- read_csv("Data/LW/Unmatched_Cond_Q2.csv", col_types = cols(
  Preparation_Time = col_character(),
  Analysis_Time = col_character(),
))

CHLcor <- read_csv("Data/LW/CHLcor_Q3.csv", col_types = cols(
  Preparation_Time = col_character(),
  Analysis_Time = col_character(),
  Station = col_character(),
  Activity_Start_Date = col_character(),
))

CHLun <- read_csv("Data/LW/CHLun_Q3.csv", col_types = cols(
  Preparation_Time = col_character(),
  Analysis_Time = col_character(),
  Station = col_character(),
  Activity_Start_Date = col_character(),
))

Secchi <- read_csv("Data/LW/Secchi_Q3.csv", col_types = cols(
  Activity_Start_Date = col_character()))




### Functions ----

## FOR ALL PARAMETERS ##

## Define lakes and counties for CBA
cba_lake_names <- c("Allen", "Alligator", "Bass Lake", "Big Red Fish", "Camp Creek", "Campbell", 
                    "Deer", "Draper", "Eastern", "Eastern North", "Fuller", "Grayton", 
                    "Little Red Fish", "Morris", "Mullet Creek-1", "Mullet Creek-2", 
                    "Mullet Creek-3", "Oyster", "Powell", "Roberts", "Stallworth", 
                    "Swift Creek-1", "Swift Creek-2", "Swift Creek-3", "Swift Creek-4", 
                    "Tresca", "Western", "Western Northeast")

cba_county_names <- c("Walton", "Okaloosa", "Washington", "Holmes")

# Function to update Sampling_Agency_Name
change_sampling_names <- function(Lake, County) {
  if (is.na(Lake) || is.na(County)) {
    return(NA)  # or a fallback like "Unknown"
  } else if (str_detect(Lake, regex("CBA", ignore_case = TRUE))) {
    return("CHOCTAWHATCHEE BASIN ALLIANCE")
  } else if (Lake %in% cba_lake_names & County %in% cba_county_names) {
    return("CHOCTAWHATCHEE BASIN ALLIANCE")
  } else if (County == "Bay") {
    return("ST. ANDREW BAY RESOURCE MANAGEMENT ASSOCIATION INC.")
  } else {
    return("Florida Lakewatch")
  }
}

# Apply function to update Sampling_Agency_Name in the dataset
# df <- df %>%
#   mutate(Sampling_Agency_Name = mapply(change_sampling_names, Lake, County))



# For changing the Project ID
change_project_id<- function(Project_ID, Sampling_Agency_Name) {
  Sampling_Agency_Name <- ifelse(grepl("CHOCTAWHATCHEE BASIN ALLIANCE", Sampling_Agency_Name, ignore.case = FALSE), 
                                 "BAYRALLY", 
                                 "21FLKWAT")
  return(Sampling_Agency_Name)
}

## FOR SECCHI ONLY ##


#For blank org result value, org result value should = "Not Reported".
not_reported_secchi <- function(Org_Result_Value){
  ifelse(is.na(Org_Result_Value), "Not Reported", Org_Result_Value)
}


#change Total_Depth to = Org_Result_Value If Value_Qualifier = S
change_total_depth <- function(Total_Depth, Value_Qualifier, Org_Result_Value){
  ifelse(Value_Qualifier == "S", Org_Result_Value, Total_Depth)
}



### SECCHI ----

#change format of dates
# v2<- c(Secchi$Activity_Start_Date)
# x2 <- lubridate::parse_date_time(v2,'"%m%d%y"')
# format(x2, format = '%m/%d/%Y')


Secchi_d <- Secchi%>%
  filter(!str_detect(Station, regex("MLBK|MBLK|CCV|MS|DUP|LFB", ignore_case = TRUE)))%>%
  #mutate('Activity_Start_Date'=format(x2, format = '%m/%d/%Y')) %>%
  mutate(Org_Analyte_Name = "Depth, Secchi Disk Depth")%>%
  mutate('Org_Result_Value' = not_reported_secchi(Org_Result_Value))%>%
  mutate(Station = as.character(Station)) %>%
  mutate(Lake_County_Sta = paste(Lake, County, Station)) %>%
  mutate(Org_Result_Value = not_reported_secchi(Org_Result_Value)) %>%
  mutate(Total_Depth = change_total_depth(Total_Depth,WIN_Value_Qualifier, Org_Result_Value)) %>%
  mutate(Sampling_Agency_Name = mapply(change_sampling_names, Lake, County)) %>%
  mutate(Project_ID = change_project_id(Project_ID, Sampling_Agency_Name))
  
  
#JOIN WITH BASE FILE FOR MONITORING LOCATION ID 

Mon_ID = Mon_ID %>%
  mutate(Station = as.character(Station)) %>%
  mutate(Lake_County_Sta = paste(Lake, County, Station)) 

  
Secchi_t  <- Secchi_d %>% 
  inner_join(Mon_ID, by = c("Lake_County_Sta")) %>% 
  select(-c("Lake.y", "County.y", "Station.y")) %>% 
  rename("Lake" = "Lake.x", "County" = "County.x", "Station" = "Station.x")



#ADD COLUMNS

Secchi_WIN <- Secchi_t

Secchi_WIN$Project_ID = paste0(Secchi_t$"Project_ID")
Secchi_WIN$Sampling_Agency_Name = paste0(Secchi_t$"Sampling_Agency_Name")
Secchi_WIN$Matrix <- "AQUEOUS-Surface Water"
Secchi_WIN$Monitoring_Location_ID = paste0(Secchi_t$Monitoring_Location_ID)
Secchi_WIN$Activity_ID = paste0(Secchi_WIN$Monitoring_Location_ID,"-",Secchi_t$Activity_Start_Date,"F")
Secchi_WIN$ADAPT_Analyte_ID = paste("WIN-010")
Secchi_WIN$Org_Analyte_Name = paste("Depth, Secchi Disk Depth")
Secchi_WIN$Activity_Type = paste("Field")
Secchi_WIN$Sample_Collection_Type = paste("Field Testing-Discrete")
Secchi_WIN$Sample_Collection_Equipment = paste("Misc Field Device")
Secchi_WIN$Activity_Depth = paste("")
Secchi_WIN$Activity_Depth_Unit = paste("")
Secchi_WIN$Analysis_Method = paste("FDEP FT1700")
Secchi_WIN$Sample_Fraction = paste("")
Secchi_WIN$Preparation_Date_Time = paste0(" ")
Secchi_WIN$Preparation_Time_Zone = paste0(" ")
Secchi_WIN$Analysis_Date_Time = paste0(" ")
Secchi_WIN$Analysis_Time_Zone = paste0(" ")
Secchi_WIN$Activity_Date_Time = paste0(Secchi_t$Activity_Start_Date," ",Secchi_t$Activity_Start_Time)
Secchi_WIN$Org_MDL = paste("")
Secchi_WIN$Org_PQL = paste("")
Secchi_WIN$Dilution = paste("")
Secchi_WIN$Org_Detection_Unit = paste("")
Secchi_WIN$Value_Qualifier = paste0(Secchi_WIN$WIN_Value_Qualifier)
Secchi_WIN$Result_Comments = paste(" ")
Secchi_WIN$Result_Value_Type_Name = paste("Actual")
Secchi_WIN$Lab_ID = paste("21FLKWAT")
Secchi_WIN$Lab_Accreditation_Authority = paste("None")
Secchi_WIN$Lab_Sample_ID = paste0(Secchi_WIN$Activity_ID)



#Reorder columns
Secchi_Print <- Secchi_WIN[,c("Project_ID","Sampling_Agency_Name","Matrix",
                              "Monitoring_Location_ID","Activity_ID","ADAPT_Analyte_ID","Org_Analyte_Name",
                              "Activity_Type","Sample_Collection_Type","Sample_Collection_Equipment",
                              "Activity_Depth","Activity_Depth_Unit","Total_Depth",
                              "Total_Depth_Unit","Analysis_Method","Sample_Fraction",
                              "Preparation_Date_Time","Preparation_Time_Zone","Analysis_Date_Time",
                              "Analysis_Time_Zone","Activity_Date_Time","Activity_Time_Zone",
                              "Org_Result_Value","Org_Result_Unit","Org_MDL",
                              "Org_PQL","Org_Detection_Unit","Value_Qualifier",
                              "Result_Comments","Result_Value_Type_Name","Dilution",
                              "Lab_ID","Lab_Accreditation_Authority","Lab_Sample_ID")]


#Subset Secchi for join with other parameters

Secchi_Join <- Secchi_t[,c("County", "Lake", "Activity_Start_Date", "Activity_Start_Time", "Station", "Activity_Time_Zone", "Monitoring_Location_ID")]


### Change date to avoid overwriting older files


CBA_Secchi <- Secchi_Print %>% filter(Sampling_Agency_Name == "CHOCTAWHATCHEE BASIN ALLIANCE")
write.table(CBA_Secchi, file = "Output/CBA_Secchi_Q3_2025.txt", sep = "|", na = "", row.names = FALSE, quote = FALSE)

LW_Secchi <- Secchi_Print %>%
  filter(Sampling_Agency_Name %in% c("Florida Lakewatch", "ST. ANDREW BAY RESOURCE MANAGEMENT ASSOCIATION INC."))
write.table(LW_Secchi, file = "Output/LW_Secchi2_Q3_2025.txt", sep = "|", na = "", row.names = FALSE, quote = FALSE)



### TP ----


#bind rows from TP and TP_NonMatch
TP <- rbind(TP, TP_NonMatch)

#delete duplicate rows
TP <- TP[!duplicated(TP), ]

# #change format of dates
# v5<- c(TP$Activity_Start_Date)
# x5 <- lubridate::parse_date_time(v5,'"%m%d%y"')
# format(x5, format = '%m/%d/%Y')
# 
# v105<- c(TP$Preparation_Date)
# x105 <- lubridate::parse_date_time(v105,'"%m%d%y"')
# format(x105, format = '%m/%d/%Y')
# 
# v106<- c(TP$Analysis_Date)
# x106 <- lubridate::parse_date_time(v106,'"%m%d%y"')
# format(x106, format = '%m/%d/%Y')

TP_t = TP%>%
  filter(!str_detect(Station, regex("MLBK|MBLK|CCV|MS|DUP|LFB", ignore_case = TRUE)))%>%
  # mutate('Activity_Start_Date'=format(x5, format = '%m/%d/%Y')) %>%
  # mutate('Preparation_Date'=format(x105, format = '%m/%d/%Y')) %>%
  # mutate('Analysis_Date'=format(x106, format = '%m/%d/%Y')) %>%
  mutate('WIN_Value_Qualifier' = ifelse(WIN_Value_Qualifier %in% c("T", "TI"), "U", 
                                        ifelse(WIN_Value_Qualifier == "QTI", "QU", WIN_Value_Qualifier)))%>%
  mutate(`Result_Value(ug/L)` = ifelse(`Result_Value(ug/L)` < 6, 6, `Result_Value(ug/L)`)) %>%
  mutate(Sampling_Agency_Name = mapply(change_sampling_names, Lake, County))%>%
  mutate(Project_ID = change_project_id(Project_ID, Sampling_Agency_Name))


#join with Secchi for start time and subset non-matches

# Perform the full join
TP_Time <- inner_join(TP_t, Secchi_Join, 
                     by = c("County", "Lake", "Activity_Start_Date", "Station"))

# Get rows in TP_t that did not match any row in Secchi_Join
unmatched_TP <- anti_join(TP_t, Secchi_Join, 
                            by = c("County", "Lake", "Activity_Start_Date", "Station"))
unmatched_TP <- unmatched_TP %>%
  select(-c(Sampling_Agency_Name, Project_ID))
# Get rows in Secchi_Join that did not match any row in TP_t
#unmatched_Secchi_Join <- anti_join(Secchi_Join, TP_t, 
                                  # by = c("County", "Lake", "Activity_Start_Date", "Station"))

# View or export the unmatched rows if needed
# View(unmatched_TP_t)
# View(unmatched_Secchi_Join)

# Write to CSV if needed
write.csv(unmatched_TP, "/Users/mevanskeene/Documents/GitHub/WIN File Creation/Output/Non-Matches/Unmatched_TP_Q3.csv", na = "", row.names = FALSE)
#write.csv(unmatched_Secchi_Join, "/Users/mevanskeene/Documents/GitHub/WIN File Creation/Output/Non-Matches/Unmatched_Secchi_Join.csv", na = "",  row.names = FALSE)

#Add columns

TP_WIN <- TP_Time 

TP_WIN$Project_ID = paste0(TP_Time$'Project_ID')
TP_WIN$Sampling_Agency_Name = paste0(TP_Time$'Sampling_Agency_Name')
TP_WIN$Matrix <- "AQUEOUS-Surface Water"
TP_WIN$Monitoring_Location_ID = paste0(TP_Time$Monitoring_Location_ID)
TP_WIN$Activity_ID = paste0(TP_WIN$Monitoring_Location_ID,"-",TP_Time$Activity_Start_Date,"S")
TP_WIN$ADAPT_Analyte_ID = paste("1910")
TP_WIN$Org_Analyte_Name = paste("Phosphorus- Total")
TP_WIN$Activity_Type = paste("Sample")
TP_WIN$Sample_Collection_Type = paste("Direct Grab")
TP_WIN$Sample_Collection_Equipment = paste("Water Bottle")
TP_WIN$Activity_Depth = paste("0.3")
TP_WIN$Activity_Depth_Unit = paste("m")
TP_WIN$Total_Depth = paste("")
TP_WIN$Total_Depth_Unit = paste("")
TP_WIN$Analysis_Method = paste("LAKEWATCH-TP")
TP_WIN$Sample_Fraction = paste("Total")
TP_WIN$Preparation_Date_Time = paste0(TP_Time$Preparation_Date," ",TP_Time$Preparation_Time)
TP_WIN$Analysis_Date_Time = paste0(TP_Time$Analysis_Date," ",TP_Time$Analysis_Time) 
TP_WIN$Activity_Date_Time = paste0(TP_Time$Activity_Start_Date," ",TP_Time$Activity_Start_Time)
TP_WIN$Activity_Time_Zone
TP_WIN$Org_Result_Value = paste0(TP_Time$'Result_Value(ug/L)')
TP_WIN$Org_Result_Unit = paste("ug/L")
TP_WIN$Org_MDL = paste("6")
TP_WIN$Org_PQL = paste("19")
TP_WIN$Org_Detection_Unit = paste("ug/L")
TP_WIN$Value_Qualifier = paste0(TP_Time$WIN_Value_Qualifier)
TP_WIN$Result_Comments = paste(" ")
TP_WIN$Result_Value_Type_Name = paste("Actual")
TP_WIN$Lab_ID = paste("21FLKWAT")
TP_WIN$Lab_Accreditation_Authority = paste("None")
TP_WIN$Lab_Sample_ID = paste0(TP_WIN$Activity_ID)


#Reorder columns
TP_Print <- TP_WIN[,c("Project_ID","Sampling_Agency_Name","Matrix",
                      "Monitoring_Location_ID","Activity_ID","ADAPT_Analyte_ID","Org_Analyte_Name",
                      "Activity_Type","Sample_Collection_Type","Sample_Collection_Equipment",
                      "Activity_Depth","Activity_Depth_Unit","Total_Depth",
                      "Total_Depth_Unit","Analysis_Method","Sample_Fraction",
                      "Preparation_Date_Time","Preparation_Time_Zone","Analysis_Date_Time",
                      "Analysis_Time_Zone","Activity_Date_Time","Activity_Time_Zone",
                      "Org_Result_Value","Org_Result_Unit","Org_MDL",
                      "Org_PQL","Org_Detection_Unit","Value_Qualifier",
                      "Result_Comments","Result_Value_Type_Name","Dilution",
                      "Lab_ID","Lab_Accreditation_Authority","Lab_Sample_ID")]



CBA_TP <- TP_Print %>% filter(Sampling_Agency_Name == "CHOCTAWHATCHEE BASIN ALLIANCE")
write.table(CBA_TP, file = "Output/CBA_TP_Q3_2025.txt", sep = "|", na = "", row.names = FALSE, quote = FALSE)

LW_TP <- TP_Print %>%  filter(Sampling_Agency_Name %in% c("Florida Lakewatch", "ST. ANDREW BAY RESOURCE MANAGEMENT ASSOCIATION INC."))
write.table(LW_TP, file = "Output/LW_TP_Q3_2025.txt", sep = "|", na = "", row.names = FALSE, quote = FALSE)


### TN ----

#bind rows from TP and TP_NonMatch
TN <- rbind(TN, TN_NonMatch)

#delete duplicate rows
TN <- TN[!duplicated(TN), ]

# #change format of dates
# v8<- c(TN$Activity_Start_Date)
# x8 <- lubridate::parse_date_time(v8,'"%m%d%y"')
# format(x8, format = '%m/%d/%Y')
# 
# v107<- c(TN$Preparation_Date)
# x107 <- lubridate::parse_date_time(v107,'"%m%d%y"')
# format(x107, format = '%m/%d/%Y')
# 
# v108<- c(TN$Analysis_Date)
# x108 <- lubridate::parse_date_time(v108,'"%m%d%y"')
# format(x108, format = '%m/%d/%Y')

TN_t = TN%>%
  filter(!str_detect(Station, regex("MLBK|MBLK|CCV|MS|DUP|LFB", ignore_case = TRUE)))%>%
  # mutate('Activity_Start_Date'=format(x8, format = '%m/%d/%Y')) %>%
  # mutate('Preparation_Date'=format(x107, format = '%m/%d/%Y')) %>%
  # mutate('Analysis_Date'=format(x108, format = '%m/%d/%Y')) %>%
  mutate('WIN_Value_Qualifier' = ifelse(WIN_Value_Qualifier %in% c("T", "TI"), "U", 
                                        ifelse(WIN_Value_Qualifier == "QTI", "QU", WIN_Value_Qualifier))) %>%
  mutate(`Result_Value(ug/L)` = ifelse(`Result_Value(ug/L)` < 20, 20, `Result_Value(ug/L)`)) %>%
  mutate(Sampling_Agency_Name = mapply(change_sampling_names, Lake, County))%>%
  mutate(Project_ID = change_project_id(Project_ID, Sampling_Agency_Name))

#join with Secchi for start time and subset non-matches

# Perform the full join
TN_Time <- inner_join(TN_t, Secchi_Join, 
                      by = c("County", "Lake", "Activity_Start_Date", "Station"))

# Get rows in TP_t that did not match any row in Secchi_Join
unmatched_TN <- anti_join(TN_t, Secchi_Join, 
                            by = c("County", "Lake", "Activity_Start_Date", "Station"))
unmatched_TN <- unmatched_TN %>%
  select(-c(Sampling_Agency_Name, Project_ID))
# Write to CSV 
write.csv(unmatched_TN, "/Users/mevanskeene/Documents/GitHub/WIN File Creation/Output/Non-Matches/Unmatched_TN_Q3.csv", na = "", row.names = FALSE)

#Add columns
TN_WIN <- TN_Time 

TN_WIN$Project_ID = paste0(TN_Time$'Project_ID')
TN_WIN$Sampling_Agency_Name = paste0(TN_Time$'Sampling_Agency_Name')
TN_WIN$Matrix <- "AQUEOUS-Surface Water"
TN_WIN$Monitoring_Location_ID = paste0(TN_Time$Monitoring_Location_ID)
TN_WIN$Activity_ID = paste0(TN_WIN$Monitoring_Location_ID,"-",TN_Time$Activity_Start_Date,"S")
TN_WIN$ADAPT_Analyte_ID = paste("FL-INORG-002")
TN_WIN$Org_Analyte_Name = paste("Nitrogen- Total")
TN_WIN$Activity_Type = paste("Sample")
TN_WIN$Sample_Collection_Type = paste("Direct Grab")
TN_WIN$Sample_Collection_Equipment = paste("Water Bottle")
TN_WIN$Activity_Depth = paste("0.3")
TN_WIN$Activity_Depth_Unit = paste("m")
TN_WIN$Total_Depth = paste("")
TN_WIN$Total_Depth_Unit = paste("")
TN_WIN$Analysis_Method = paste("LAKEWATCH-TN")
TN_WIN$Sample_Fraction = paste("Total")
TN_WIN$Preparation_Date_Time = paste0(TN_Time$Preparation_Date," ",TN_Time$Preparation_Time)
TN_WIN$Analysis_Date_Time = paste0(TN_Time$Analysis_Date," ",TN_Time$Analysis_Time) 
TN_WIN$Activity_Date_Time = paste0(TN_Time$Activity_Start_Date," ",TN_Time$Activity_Start_Time)
TN_WIN$Activity_Time_Zone
TN_WIN$Org_Result_Value = paste0(TN_Time$'Result_Value(ug/L)')
TN_WIN$Org_Result_Unit = paste("ug/L")
TN_WIN$Org_MDL = paste("20")
TN_WIN$Org_PQL = paste("60")
TN_WIN$Org_Detection_Unit = paste("ug/L")
TN_WIN$Value_Qualifier = paste0(TN_Time$WIN_Value_Qualifier)
TN_WIN$Result_Comments = paste(" ")
TN_WIN$Result_Value_Type_Name = paste("Actual")
TN_WIN$Lab_ID = paste("21FLKWAT")
TN_WIN$Lab_Accreditation_Authority = paste("None")
TN_WIN$Lab_Sample_ID = paste0(TN_WIN$Activity_ID)


#Reorder columns
TN_Print <- TN_WIN[,c("Project_ID","Sampling_Agency_Name","Matrix",
                      "Monitoring_Location_ID","Activity_ID","ADAPT_Analyte_ID","Org_Analyte_Name",
                      "Activity_Type","Sample_Collection_Type","Sample_Collection_Equipment",
                      "Activity_Depth","Activity_Depth_Unit","Total_Depth",
                      "Total_Depth_Unit","Analysis_Method","Sample_Fraction",
                      "Preparation_Date_Time","Preparation_Time_Zone","Analysis_Date_Time",
                      "Analysis_Time_Zone","Activity_Date_Time","Activity_Time_Zone",
                      "Org_Result_Value","Org_Result_Unit","Org_MDL",
                      "Org_PQL","Org_Detection_Unit","Value_Qualifier",
                      "Result_Comments","Result_Value_Type_Name","Dilution",
                      "Lab_ID","Lab_Accreditation_Authority","Lab_Sample_ID")]



CBA_TN <- TN_Print %>% filter(Sampling_Agency_Name == "CHOCTAWHATCHEE BASIN ALLIANCE")
write.table(CBA_TN, file = "Output/CBA_TN_Q3_2025.txt", sep = "|", na = "", row.names = FALSE, quote = FALSE)

LW_TN <- TN_Print %>%  filter(Sampling_Agency_Name %in% c("Florida Lakewatch", "ST. ANDREW BAY RESOURCE MANAGEMENT ASSOCIATION INC."))
write.table(LW_TN, file = "Output/LW_TN_Q3_2025.txt", sep = "|", na = "", row.names = FALSE, quote = FALSE)



### CHLcor ----

# #Change format of times
# v102<- c(CHL$Activity_Start_Time)
# x102 <- lubridate::parse_date_time(v102,'H:M:S')
# format(x102, format = '%I:%M:%S %p')
# #Change format of times
# v103 <- c(CHL$Analysis_Time)
# x103 <- lubridate::parse_date_time(v103,'H:M:S')
# format(x103, format = '%I:%M:%S %p')
# #Change format of times
# v104<- c(CHL$Preparation_Time)
# x104 <- lubridate::parse_date_time(v104,'H:M:S')
# format(x104, format = '%I:%M:%S %p')

CHLcor_t = CHLcor%>%
  # mutate('Activity_Start_Time'=format(x102, format = '%I:%M:%S %p')) %>%
  # mutate('Analysis_Time'=format(x103, format = '%I:%M:%S %p'))%>%
  # mutate('Preparation_Time'=format(x104, format = '%I:%M:%S %p'))%>%
  filter(!str_detect(Station, regex("MLBK|MBLK|CCV|MS|DUP|LFB", ignore_case = TRUE)))%>%
  mutate(WIN_Value_Qualifier = ifelse(WIN_Value_Qualifier %in% c("T", "TI"), "U", 
                                  ifelse(WIN_Value_Qualifier == "QTI", "QU", WIN_Value_Qualifier)))%>%
  mutate(`Result_Value(ug/L)` = ifelse(`Result_Value(ug/L)` < 1, 1, `Result_Value(ug/L)`)) %>%
  mutate(Sampling_Agency_Name = mapply(change_sampling_names, Lake, County))%>%
  mutate(Project_ID = change_project_id(Project_ID, Sampling_Agency_Name))

# Perform the full join
CHLcor_Time = full_join(CHLcor_t, Secchi_Join, 
                      by = c("County", "Lake", "Activity_Start_Date", "Station"))



#Add columns
CHLcor_WIN <- CHLcor_Time 

CHLcor_WIN$Project_ID = paste0(CHLcor_Time$'Project_ID')
CHLcor_WIN$Sampling_Agency_Name = paste0(CHLcor_Time$'Sampling_Agency_Name')
CHLcor_WIN$Matrix <- "AQUEOUS-Surface Water"
CHLcor_WIN$Monitoring_Location_ID = paste0(CHLcor_Time$Monitoring_Location_ID)
CHLcor_WIN$Activity_ID = paste0(CHLcor_WIN$Monitoring_Location_ID,"-",CHLcor_Time$Activity_Start_Date,"S")
CHLcor_WIN$ADAPT_Analyte_ID = paste("WIN-001")
CHLcor_WIN$Org_Analyte_Name = paste("Chlorophyll a- corrected")
CHLcor_WIN$Activity_Type = paste("Sample")
CHLcor_WIN$Sample_Collection_Type = paste("Direct Grab")
CHLcor_WIN$Sample_Collection_Equipment = paste("Water Bottle")
CHLcor_WIN$Activity_Depth = paste("0.3")
CHLcor_WIN$Activity_Depth_Unit = paste("m")
CHLcor_WIN$Total_Depth = paste("")
CHLcor_WIN$Total_Depth_Unit = paste("")
CHLcor_WIN$Analysis_Method = paste("LAKEWATCH-CHL")
CHLcor_WIN$Sample_Fraction = paste("Total")
CHLcor_WIN$Preparation_Date_Time = paste0(CHLcor_Time$Preparation_Date," ",CHLcor_Time$Preparation_Time)
CHLcor_WIN$Analysis_Date_Time = paste0(CHLcor_Time$Analysis_Date," ",CHLcor_Time$Analysis_Time) 
CHLcor_WIN$Activity_Date_Time = paste0(CHLcor_Time$Activity_Start_Date," ",CHLcor_Time$Activity_Start_Time.x)
CHLcor_WIN$Activity_Time_Zone = paste0(CHLcor_Time$Activity_Time_Zone.x)
CHLcor_WIN$Org_Result_Value = paste0(CHLcor_Time$`Result_Value(ug/L)`)
CHLcor_WIN$Org_Result_Unit = paste("ug/L")
CHLcor_WIN$Org_MDL = paste("1")
CHLcor_WIN$Org_PQL = paste("1")
CHLcor_WIN$Dilution = paste("1")
CHLcor_WIN$Org_Detection_Unit = paste("ug/L")
CHLcor_WIN$Value_Qualifier = paste0(CHLcor_Time$WIN_Value_Qualifier)
CHLcor_WIN$Result_Comments = paste(" ")
CHLcor_WIN$Result_Value_Type_Name = paste("Actual")
CHLcor_WIN$Lab_ID = paste("21FLKWAT")
CHLcor_WIN$Lab_Accreditation_Authority = paste("None")
CHLcor_WIN$Lab_Sample_ID = paste0(CHLcor_WIN$Activity_ID)


#Reorder columns
CHLcor_Print <- CHLcor_WIN[,c("Project_ID","Sampling_Agency_Name","Matrix",
                      "Monitoring_Location_ID","Activity_ID","ADAPT_Analyte_ID","Org_Analyte_Name",
                      "Activity_Type","Sample_Collection_Type","Sample_Collection_Equipment",
                      "Activity_Depth","Activity_Depth_Unit","Total_Depth",
                      "Total_Depth_Unit","Analysis_Method","Sample_Fraction",
                      "Preparation_Date_Time","Preparation_Time_Zone","Analysis_Date_Time",
                      "Analysis_Time_Zone","Activity_Date_Time","Activity_Time_Zone",
                      "Org_Result_Value","Org_Result_Unit","Org_MDL",
                      "Org_PQL","Org_Detection_Unit","Value_Qualifier",
                      "Result_Comments","Result_Value_Type_Name","Dilution",
                      "Lab_ID","Lab_Accreditation_Authority","Lab_Sample_ID")]



CBA_CHLcor <- CHLcor_Print %>% filter(Sampling_Agency_Name == "CHOCTAWHATCHEE BASIN ALLIANCE")
write.table(CBA_CHLcor, file = "Output/CBA_CHLcor_Q3_2025.txt", sep = "|", na = "", row.names = FALSE, quote = FALSE)

LW_CHLcor <- CHLcor_Print %>%  filter(Sampling_Agency_Name %in% c("Florida Lakewatch", "ST. ANDREW BAY RESOURCE MANAGEMENT ASSOCIATION INC."))
write.table(LW_CHLcor, file = "Output/LW_CHLcor_Q3_2025.txt", sep = "|", na = "", row.names = FALSE, quote = FALSE)

### CHLun ----

# #Change format of times
# v102<- c(CHL$Activity_Start_Time)
# x102 <- lubridate::parse_date_time(v102,'H:M:S')
# format(x102, format = '%I:%M:%S %p')
# #Change format of times
# v103 <- c(CHL$Analysis_Time)
# x103 <- lubridate::parse_date_time(v103,'H:M:S')
# format(x103, format = '%I:%M:%S %p')
# #Change format of times
# v104<- c(CHL$Preparation_Time)
# x104 <- lubridate::parse_date_time(v104,'H:M:S')
# format(x104, format = '%I:%M:%S %p')

CHLun_t = CHLun%>%
  # mutate('Activity_Start_Time'=format(x102, format = '%I:%M:%S %p')) %>%
  # mutate('Analysis_Time'=format(x103, format = '%I:%M:%S %p'))%>%
  # mutate('Preparation_Time'=format(x104, format = '%I:%M:%S %p'))%>%
  filter(!str_detect(Station, regex("MLBK|MBLK|CCV|MS|DUP|LFB", ignore_case = TRUE)))%>%
  mutate(WIN_Value_Qualifier = ifelse(WIN_Value_Qualifier %in% c("T", "TI"), "U", 
                                  ifelse(WIN_Value_Qualifier == "QTI", "QU", WIN_Value_Qualifier)))%>%
  mutate(`Result_Value(ug/L)` = ifelse(`Result_Value(ug/L)` < 1, 1, `Result_Value(ug/L)`)) %>%
  mutate(Sampling_Agency_Name = mapply(change_sampling_names, Lake, County))%>%
  mutate(Project_ID = change_project_id(Project_ID, Sampling_Agency_Name))

# Perform the full join
CHLun_Time = full_join(CHLun_t, Secchi_Join, 
                        by = c("County", "Lake", "Activity_Start_Date", "Station"))



#Add columns
CHLun_WIN <- CHLun_Time 

CHLun_WIN$Project_ID = paste0(CHLun_Time$'Project_ID')
CHLun_WIN$Sampling_Agency_Name = paste0(CHLun_Time$'Sampling_Agency_Name')
CHLun_WIN$Matrix <- "AQUEOUS-Surface Water"
CHLun_WIN$Monitoring_Location_ID = paste0(CHLun_Time$Monitoring_Location_ID)
CHLun_WIN$Activity_ID = paste0(CHLun_WIN$Monitoring_Location_ID,"-",CHLun_Time$Activity_Start_Date,"S")
CHLun_WIN$ADAPT_Analyte_ID = paste("FL-BIO-004")
CHLun_WIN$Org_Analyte_Name = paste("Chlorophyll a- uncorrected")
CHLun_WIN$Activity_Type = paste("Sample")
CHLun_WIN$Sample_Collection_Type = paste("Direct Grab")
CHLun_WIN$Sample_Collection_Equipment = paste("Water Bottle")
CHLun_WIN$Activity_Depth = paste("0.3")
CHLun_WIN$Activity_Depth_Unit = paste("m")
CHLun_WIN$Total_Depth = paste("")
CHLun_WIN$Total_Depth_Unit = paste("")
CHLun_WIN$Analysis_Method = paste("LAKEWATCH-CHL")
CHLun_WIN$Sample_Fraction = paste("Total")
CHLun_WIN$Preparation_Date_Time = paste0(CHLun_Time$Preparation_Date," ",CHLun_Time$Preparation_Time)
CHLun_WIN$Analysis_Date_Time = paste0(CHLun_Time$Analysis_Date," ",CHLun_Time$Analysis_Time) 
CHLun_WIN$Activity_Date_Time = paste0(CHLun_Time$Activity_Start_Date," ",CHLun_Time$Activity_Start_Time.x)
CHLun_WIN$Activity_Time_Zone = paste0(CHLun_Time$Activity_Time_Zone.x)
CHLun_WIN$Org_Result_Value = paste0(CHLun_Time$`Result_Value(ug/L)`)
CHLun_WIN$Org_Result_Unit = paste("ug/L")
CHLun_WIN$Org_MDL = paste("1")
CHLun_WIN$Org_PQL = paste("1")
CHLun_WIN$Dilution = paste("1")
CHLun_WIN$Org_Detection_Unit = paste("ug/L")
CHLun_WIN$Value_Qualifier = paste0(CHLun_Time$WIN_Value_Qualifier)
CHLun_WIN$Result_Comments = paste(" ")
CHLun_WIN$Result_Value_Type_Name = paste("Actual")
CHLun_WIN$Lab_ID = paste("21FLKWAT")
CHLun_WIN$Lab_Accreditation_Authority = paste("None")
CHLun_WIN$Lab_Sample_ID = paste0(CHLun_WIN$Activity_ID)


#Reorder columns
CHLun_Print <- CHLun_WIN[,c("Project_ID","Sampling_Agency_Name","Matrix",
                              "Monitoring_Location_ID","Activity_ID","ADAPT_Analyte_ID","Org_Analyte_Name",
                              "Activity_Type","Sample_Collection_Type","Sample_Collection_Equipment",
                              "Activity_Depth","Activity_Depth_Unit","Total_Depth",
                              "Total_Depth_Unit","Analysis_Method","Sample_Fraction",
                              "Preparation_Date_Time","Preparation_Time_Zone","Analysis_Date_Time",
                              "Analysis_Time_Zone","Activity_Date_Time","Activity_Time_Zone",
                              "Org_Result_Value","Org_Result_Unit","Org_MDL",
                              "Org_PQL","Org_Detection_Unit","Value_Qualifier",
                              "Result_Comments","Result_Value_Type_Name","Dilution",
                              "Lab_ID","Lab_Accreditation_Authority","Lab_Sample_ID")]



CBA_CHLun <- CHLun_Print %>% filter(Sampling_Agency_Name == "CHOCTAWHATCHEE BASIN ALLIANCE")
write.table(CBA_CHLun, file = "Output/CBA_CHLun_Q3_2025.txt", sep = "|", na = "", row.names = FALSE, quote = FALSE)

LW_CHLun <- CHLun_Print %>%  filter(Sampling_Agency_Name %in% c("Florida Lakewatch", "ST. ANDREW BAY RESOURCE MANAGEMENT ASSOCIATION INC."))
write.table(LW_CHLun, file = "Output/LW_CHLun_Q3_2025.txt", sep = "|", na = "", row.names = FALSE, quote = FALSE)


### Color ----

#bind rows from TP and TP_NonMatch
Color <- rbind(Color, Color_NonMatch)

#delete duplicate rows
Color <- Color[!duplicated(Color), ]

# #change format of dates
# v109<- c(Color$Activity_Start_Date)
# x109 <- lubridate::parse_date_time(v109,'"%m%d%y"')
# format(x109, format = '%m/%d/%Y')
# 
# v110<- c(Color$Preparation_Date)
# x110 <- lubridate::parse_date_time(v110,'"%m%d%y"')
# format(x110, format = '%m/%d/%Y')
# 
# v111<- c(Color$Analysis_Date)
# x111 <- lubridate::parse_date_time(v111,'"%m%d%y"')
# format(x111, format = '%m/%d/%Y')

Color_t = Color%>%
  filter(!str_detect(Station, regex("MLBK|MBLK|CCV|MS|DUP|LFB", ignore_case = TRUE)))%>%
  # mutate('Activity_Start_Date'=format(x109, format = '%m/%d/%Y')) %>%
  # mutate('Preparation_Date'=format(x110, format = '%m/%d/%Y')) %>%
  # mutate('Analysis_Date'=format(x111, format = '%m/%d/%Y')) %>%
  mutate('WIN_Value_Qualifier' = ifelse(WIN_Value_Qualifier %in% c("T", "TI"), "U", 
                                        ifelse(WIN_Value_Qualifier == "QTI", "QU", WIN_Value_Qualifier)))%>%
  mutate(`Result_Value(PCU)` = ifelse(`Result_Value(PCU)` < 1, 1, `Result_Value(PCU)`)) %>%
  mutate(Sampling_Agency_Name = mapply(change_sampling_names, Lake, County))%>%
  mutate(Project_ID = change_project_id(Project_ID, Sampling_Agency_Name))


#join with Secchi for start time and subset non-matches

# Perform the full join
Color_Time <- inner_join(Color_t, Secchi_Join, 
                      by = c("County", "Lake", "Activity_Start_Date", "Station"))

# Get rows in TP_t that did not match any row in Secchi_Join
unmatched_Color <- anti_join(Color_t, Secchi_Join, 
                          by = c("County", "Lake", "Activity_Start_Date", "Station"))
unmatched_Color <- unmatched_Color %>%
  select(-c(Sampling_Agency_Name, Project_ID))
# Write to CSV 
write.csv(unmatched_Color, "/Users/mevanskeene/Documents/GitHub/WIN File Creation/Output/Non-Matches/Unmatched_Color_Q3.csv", na = "", row.names = FALSE)

#Add columns


Color_WIN <- Color_Time 

Color_WIN$Project_ID = paste0(Color_Time$'Project_ID')
Color_WIN$Sampling_Agency_Name = paste0(Color_Time$'Sampling_Agency_Name')
Color_WIN$Matrix <- "AQUEOUS-Surface Water"
Color_WIN$Monitoring_Location_ID = paste0(Color_Time$Monitoring_Location_ID)
Color_WIN$Activity_ID = paste0(Color_WIN$Monitoring_Location_ID,"-",Color_Time$Activity_Start_Date,"S")
Color_WIN$ADAPT_Analyte_ID = paste("FL-PHYS-012")
Color_WIN$Org_Analyte_Name = paste("Color- True")
Color_WIN$Activity_Type = paste("Sample")
Color_WIN$Sample_Collection_Type = paste("Direct Grab")
Color_WIN$Sample_Collection_Equipment = paste("Water Bottle")
Color_WIN$Activity_Depth = paste("0.3")
Color_WIN$Activity_Depth_Unit = paste("m")
Color_WIN$Total_Depth = paste("")
Color_WIN$Total_Depth_Unit = paste("")
Color_WIN$Analysis_Method = paste("SM 2120 C")
Color_WIN$Sample_Fraction = paste("")
Color_WIN$Preparation_Date_Time = paste0(Color_Time$Preparation_Date," ",Color_Time$Preparation_Time)
Color_WIN$Analysis_Date_Time = paste0(Color_Time$Analysis_Date," ",Color_Time$Analysis_Time) 
Color_WIN$Activity_Date_Time = paste0(Color_Time$Activity_Start_Date," ",Color_Time$Activity_Start_Time)
Color_WIN$Activity_Time_Zone
Color_WIN$Org_Result_Value = paste0(Color_Time$`Result_Value(PCU)`)
Color_WIN$Org_Result_Unit = paste("PCU")
Color_WIN$Org_MDL = paste("1")
Color_WIN$Org_PQL = paste("1")
Color_WIN$Org_Detection_Unit = paste("PCU")
Color_WIN$Value_Qualifier = paste0(Color_Time$WIN_Value_Qualifier)
Color_WIN$Result_Comments = paste(" ")
Color_WIN$Result_Value_Type_Name = paste("Actual")
Color_WIN$Lab_ID = paste("21FLKWAT")
Color_WIN$Lab_Accreditation_Authority = paste("None")
Color_WIN$Lab_Sample_ID = paste0(Color_WIN$Activity_ID)


#Reorder columns
Color_Print <- Color_WIN[,c("Project_ID","Sampling_Agency_Name","Matrix",
                      "Monitoring_Location_ID","Activity_ID","ADAPT_Analyte_ID","Org_Analyte_Name",
                      "Activity_Type","Sample_Collection_Type","Sample_Collection_Equipment",
                      "Activity_Depth","Activity_Depth_Unit","Total_Depth",
                      "Total_Depth_Unit","Analysis_Method","Sample_Fraction",
                      "Preparation_Date_Time","Preparation_Time_Zone","Analysis_Date_Time",
                      "Analysis_Time_Zone","Activity_Date_Time","Activity_Time_Zone",
                      "Org_Result_Value","Org_Result_Unit","Org_MDL",
                      "Org_PQL","Org_Detection_Unit","Value_Qualifier",
                      "Result_Comments","Result_Value_Type_Name","Dilution",
                      "Lab_ID","Lab_Accreditation_Authority","Lab_Sample_ID")]



CBA_Color <- Color_Print %>% filter(Sampling_Agency_Name == "CHOCTAWHATCHEE BASIN ALLIANCE")
write.table(CBA_Color, file = "Output/CBA_Color_Q3_2025.txt", sep = "|", na = "", row.names = FALSE, quote = FALSE)

LW_Color <- Color_Print %>%  filter(Sampling_Agency_Name %in% c("Florida Lakewatch", "ST. ANDREW BAY RESOURCE MANAGEMENT ASSOCIATION INC."))
write.table(LW_Color, file = "Output/LW_Color_Q3_2025.txt", sep = "|", na = "", row.names = FALSE, quote = FALSE)


### Conductivity ----

#bind rows from Cond_NonMatch
Cond <- rbind(Cond, Cond_NonMatch)

#delete duplicate rows
Cond <- Cond[!duplicated(Cond), ]

# #change format of dates
# v112<- c(Cond$Activity_Start_Date)
# x112 <- lubridate::parse_date_time(v112,'"%m%d%y"')
# format(x112, format = '%m/%d/%Y')
# 
# v113<- c(Cond$Preparation_Date)
# x113 <- lubridate::parse_date_time(v113,'"%m%d%y"')
# format(x113, format = '%m/%d/%Y')
# 
# v114<- c(Cond$Analysis_Date)
# x114 <- lubridate::parse_date_time(v114,'"%m%d%y"')
# format(x114, format = '%m/%d/%Y')

Cond_t = Cond%>%
  filter(!str_detect(Station, regex("MLBK|MBLK|CCV|MS|DUP|LFB", ignore_case = TRUE)))%>%
  # mutate('Activity_Start_Date'=format(x112, format = '%m/%d/%Y')) %>%
  # mutate('Preparation_Date'=format(x113, format = '%m/%d/%Y')) %>%
  # mutate('Analysis_Date'=format(x114, format = '%m/%d/%Y')) %>%
  mutate('WIN_Value_Qualifier' = ifelse(WIN_Value_Qualifier %in% c("T", "TI"), "U", 
                                        ifelse(WIN_Value_Qualifier == "QTI", "QU", WIN_Value_Qualifier)))%>%
  mutate(Sampling_Agency_Name = mapply(change_sampling_names, Lake, County))%>%
  mutate(Project_ID = change_project_id(Project_ID, Sampling_Agency_Name))


#join with Secchi for start time and subset non-matches

# Perform the full join
Cond_Time <- inner_join(Cond_t, Secchi_Join, 
                         by = c("County", "Lake", "Activity_Start_Date", "Station"))

# Get rows in TP_t that did not match any row in Secchi_Join
unmatched_Cond <- anti_join(Cond_t, Secchi_Join, 
                             by = c("County", "Lake", "Activity_Start_Date", "Station"))
#REMOVE COLUMNS sampling agency and project id from unmatched before writing to csv
unmatched_Cond <- unmatched_Cond %>%
  select(-c(Sampling_Agency_Name, Project_ID))

# Write to CSV 
write.csv(unmatched_Cond, "/Users/mevanskeene/Documents/GitHub/WIN File Creation/Output/Non-Matches/Unmatched_Cond_Q3.csv", na = "", row.names = FALSE)

#Add columns


Cond_WIN <- Cond_Time 

Cond_WIN$Project_ID = paste0(Cond_Time$'Project_ID')
Cond_WIN$Sampling_Agency_Name = paste0(Cond_Time$'Sampling_Agency_Name')
Cond_WIN$Matrix <- "AQUEOUS-Surface Water"
Cond_WIN$Monitoring_Location_ID = paste0(Cond_Time$Monitoring_Location_ID)
Cond_WIN$Activity_ID = paste0(Cond_WIN$Monitoring_Location_ID,"-",Cond_Time$Activity_Start_Date,"S")
Cond_WIN$ADAPT_Analyte_ID = paste("1610")
Cond_WIN$Org_Analyte_Name = paste("Specific Conductance")
Cond_WIN$Activity_Type = paste("Sample")
Cond_WIN$Sample_Collection_Type = paste("Direct Grab")
Cond_WIN$Sample_Collection_Equipment = paste("Water Bottle")
Cond_WIN$Activity_Depth = paste("0.3")
Cond_WIN$Activity_Depth_Unit = paste("m")
Cond_WIN$Total_Depth = paste("")
Cond_WIN$Total_Depth_Unit = paste("")
Cond_WIN$Analysis_Method = paste("SM 2510 B")
Cond_WIN$Sample_Fraction = paste("")
Cond_WIN$Preparation_Date_Time = paste0("")
Cond_WIN$Analysis_Date_Time = paste0(Cond_Time$Analysis_Date," ",Cond_Time$Analysis_Time) 
Cond_WIN$Activity_Date_Time = paste0(Cond_Time$Activity_Start_Date," ",Cond_Time$Activity_Start_Time)
Cond_WIN$Activity_Time_Zone
Cond_WIN$Org_Result_Value = paste0(Cond_Time$Org_Result_Value)
Cond_WIN$Org_Result_Unit = paste("uS/cm")
Cond_WIN$Org_MDL = paste("")
Cond_WIN$Org_PQL = paste("")
Cond_WIN$Org_Detection_Unit = paste("")
Cond_WIN$Value_Qualifier = paste0(Cond_Time$WIN_Value_Qualifier)
Cond_WIN$Result_Comments = paste(" ")
Cond_WIN$Result_Value_Type_Name = paste("Actual")
Cond_WIN$Lab_ID = paste("21FLKWAT")
Cond_WIN$Lab_Accreditation_Authority = paste("None")
Cond_WIN$Lab_Sample_ID = paste0(Cond_WIN$Activity_ID)


#Reorder columns
Cond_Print <- Cond_WIN[,c("Project_ID","Sampling_Agency_Name","Matrix",
                            "Monitoring_Location_ID","Activity_ID","ADAPT_Analyte_ID","Org_Analyte_Name",
                            "Activity_Type","Sample_Collection_Type","Sample_Collection_Equipment",
                            "Activity_Depth","Activity_Depth_Unit","Total_Depth",
                            "Total_Depth_Unit","Analysis_Method","Sample_Fraction",
                            "Preparation_Date_Time","Preparation_Time_Zone","Analysis_Date_Time",
                            "Analysis_Time_Zone","Activity_Date_Time","Activity_Time_Zone",
                            "Org_Result_Value","Org_Result_Unit","Org_MDL",
                            "Org_PQL","Org_Detection_Unit","Value_Qualifier",
                            "Result_Comments","Result_Value_Type_Name","Dilution",
                            "Lab_ID","Lab_Accreditation_Authority","Lab_Sample_ID")]



CBA_Cond <- Cond_Print %>% filter(Sampling_Agency_Name == "CHOCTAWHATCHEE BASIN ALLIANCE")
write.table(CBA_Cond, file = "Output/CBA_Cond_Q3_2025.txt", sep = "|", na = "", row.names = FALSE, quote = FALSE)

LW_Cond <- Cond_Print %>%  filter(Sampling_Agency_Name %in% c("Florida Lakewatch", "ST. ANDREW BAY RESOURCE MANAGEMENT ASSOCIATION INC."))
write.table(LW_Cond, file = "Output/LW_Cond_Q3_2025.txt", sep = "|", na = "", row.names = FALSE, quote = FALSE)

