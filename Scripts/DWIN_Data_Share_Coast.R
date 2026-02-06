# Project Notes ----
#Future Directions - mash up all the stacked file creation codes with this one to run all together
#Time formats are solved in stacked file creation codes but dates are solved here. 
#Before running this code:
#use the script for Stacked File Creation 
#Change column names to match code - use template in project folder
#Change output file names to new upload month

#Sampling Agency Names : University of Florida (Soil and Water Sciences Department)
#Project ID: 21FLUFSW

### WD and Packages ----

setwd("~/Documents/GitHub/WIN File Creation")
library(tidyverse)
library(readxl)
library(dplyr)
library(readr)
library(lubridate)
library(purrr)


### Data Import ---- 
TP <- read_csv("Data/Project Coast/TP_DATA_COAST_NOV.csv", col_types = cols(
  Preparation_Time = col_character(),
  Analysis_Time = col_character(),
))

TN <- read_csv("Data/Project Coast/TN_DATA_COAST_NOV.csv", col_types = cols(
  Preparation_Time = col_character(),
  Analysis_Time = col_character(),
))

CHLcor <- read_csv("Data/Project Coast/Chlcor_DATA_COAST_NOV.csv", col_types = cols(
  Preparation_Time = col_character(),
  Analysis_Time = col_character(),
  Station = col_character(),
  Activity_Start_Date = col_character(),
))

CHLun <- read_csv("Data/Project Coast/Chlun_DATA_COAST_NOV.csv", col_types = cols(
  Preparation_Time = col_character(),
  Analysis_Time = col_character(),
  Station = col_character(),
  Activity_Start_Date = col_character(),
))

Secchi <- read_csv("Data/Project Coast/Secchi_DATA_COAST_NOV.csv", col_types = cols(
  Activity_Start_Time = col_character(),
  Activity_Start_Date = col_character()
))


### SECCHI ----

#Change format of times
v1<- c(Secchi$Activity_Start_Time)
x <- lubridate::parse_date_time(v1,'H:M:S')
format(x, format = '%I:%M:%S %p')


#change format of dates
v2<- c(Secchi$Activity_Start_Date)
x2 <- lubridate::parse_date_time(v2,'"%m%d%y"')
format(x2, format = '%m/%d/%Y')


Secchi_t <- Secchi%>%
  # mutate('Activity_Start_Time'=format(x, format = '%I:%M:%S %p')) %>%
  mutate('Activity_Start_Date'=format(x2, format = '%m/%d/%Y')) %>%
  # mutate('Total Depth' = mutate_columns(Secchi, "Value_Qualifier", "Total_Depth", "Org_Result_Value")) %>%
  # mutate(Org_Result_Value = not_reported_secchi(Org_Result_Value)) %>%
  mutate(Org_Analyte_Name = "Depth, Secchi Disk Depth")%>%
  mutate(WIN_Value_Qualifier = ifelse(is.na(WIN_Value_Qualifier), "", WIN_Value_Qualifier))


#filter out rows that are not stations "1"-"10" and sites that are not Project Coast 
Secchi_t <- Secchi_t %>%
  filter(Station %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "B"))%>%
  filter(Lake %in% c("CIT-CRY", "CIT-HOM", "CIT-WIT", "HER-CHA", "HER-WEE", "PAS-ANC", "PAS-ARI", "PAS-HUD", "PAS-PIT"))


#Add columns

Secchi_WIN <- Secchi_t

Secchi_WIN$Project_ID <- "21FLUFSW"
Secchi_WIN$Sampling_Agency_Name <- "University of Florida (Soil and Water Sciences Department)"
Secchi_WIN$Matrix <- "AQUEOUS-Surface Water"
Secchi_WIN$Monitoring_Location_ID = paste0(Secchi_t$Lake,"-",Secchi_t$Station)
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


#Subset Secchi Start Time for join with TN TP

Secchi_Join <- Secchi_t[,c("County", "Lake", "Activity_Start_Date", "Activity_Start_Time", "Station", "Activity_Time_Zone")]
#make station character to join with others
Secchi_Join$Station <- as.character(Secchi_Join$Station)

##Change date to avoid overwriting older files##
write.table(Secchi_Print, file = "Output/Secchi_NOV_2025.txt", sep = "|", na = "", row.names = FALSE, quote = FALSE)



### TP ----


#Change format of times
# v3 <- c(TP$Analysis_Time)
# x3 <- lubridate::parse_date_time(v3,'H:M:S')
# format(x3, format = '%I:%M:%S %p')

#Change format of times
# v4<- c(TP$Preparation_Time)
# x4 <- lubridate::parse_date_time(v4,'H:M:S')
# format(x4, format = '%I:%M:%S %p')


#change format of dates
v5<- c(TP$Activity_Start_Date)
x5 <- lubridate::parse_date_time(v5,'"%m%d%y"')
format(x5, format = '%m/%d/%Y')

v105<- c(TP$Preparation_Date)
x105 <- lubridate::parse_date_time(v105,'"%m%d%y"')
format(x105, format = '%m/%d/%Y')

v106<- c(TP$Analysis_Date)
x106 <- lubridate::parse_date_time(v106,'"%m%d%y"')
format(x106, format = '%m/%d/%Y')

TP_t = TP%>%
  mutate('Activity_Start_Date'=format(x5, format = '%m/%d/%Y')) %>%
  mutate('Preparation_Date'=format(x105, format = '%m/%d/%Y')) %>%
  mutate('Analysis_Date'=format(x106, format = '%m/%d/%Y')) %>%
  mutate('WIN_Value_Qualifier' = ifelse(WIN_Value_Qualifier %in% c("T", "TI"), "U", 
                                        ifelse(WIN_Value_Qualifier == "QTI", "QU", WIN_Value_Qualifier)))%>%
  mutate(WIN_Value_Qualifier = ifelse(is.na(WIN_Value_Qualifier), "", WIN_Value_Qualifier))
#change columns
#TP_t <- rename(TP_t, Site = Lake)


#filter out rows that are not stations "1"-"10" and sites that are not Project Coast 
TP_t <- TP_t %>%
  filter(Station %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "B"))%>%
  filter(Lake %in% c("CIT-CRY", "CIT-HOM", "CIT-WIT", "HER-CHA", "HER-WEE", "PAS-ANC", "PAS-ARI", "PAS-HUD", "PAS-PIT",
                     "Old Greg- Bottom",
                     "Old Greg- 10M",
                     "Old Greg- 50M",
                     "Scar Hole- Bottom",
                     "Scar Hole- 10M",
                     "Scar Hole- 50M",
                     "Crack- Bottom",
                     "Crack- 10M",
                     "Crack- 50M")) 


#join with Secchi for start time

TP_Time = full_join(TP_t, Secchi_Join, 
                    by = c("County", "Lake", "Activity_Start_Date", "Station"))
#Add columns (rearrange these to match final file for ease of error fixing later)

TP_WIN <- TP_Time

TP_WIN$Project_ID <- "21FLUFSW"
TP_WIN$Sampling_Agency_Name <- "University of Florida (Soil and Water Sciences Department)"
TP_WIN$Matrix <- "AQUEOUS-Surface Water"
TP_WIN$Monitoring_Location_ID = paste0(TP_t$Lake,"-",TP_t$Station)
TP_WIN$Activity_ID = paste0(TP_WIN$Monitoring_Location_ID,"-",TP_t$Activity_Start_Date,"S")
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
TP_WIN$Preparation_Date_Time = paste0(TP_t$Preparation_Date," ",TP_t$Preparation_Time)
TP_WIN$Analysis_Date_Time = paste0(TP_t$Analysis_Date," ",TP_t$Analysis_Time) 
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



##Change date to avoid overwriting older files##
write.table(TP_Print, file = "Output/TP_NOV_2025.txt", sep = "|", na = "", row.names = FALSE, quote = FALSE)



### TN ----


#change format of dates
v8<- c(TN$Activity_Start_Date)
x8 <- lubridate::parse_date_time(v8,'"%m%d%y"')
format(x8, format = '%m/%d/%Y')

v107<- c(TN$Preparation_Date)
x107 <- lubridate::parse_date_time(v107,'"%m%d%y"')
format(x107, format = '%m/%d/%Y')

v108<- c(TN$Analysis_Date)
x108 <- lubridate::parse_date_time(v108,'"%m%d%y"')
format(x108, format = '%m/%d/%Y')

TN_t = TN%>%
  mutate('Activity_Start_Date'=format(x8, format = '%m/%d/%Y')) %>%
  mutate('Preparation_Date'=format(x107, format = '%m/%d/%Y')) %>%
  mutate('Analysis_Date'=format(x108, format = '%m/%d/%Y')) %>%
  mutate('WIN_Value_Qualifier' = ifelse(WIN_Value_Qualifier %in% c("T", "TI"), "U", 
                                        ifelse(WIN_Value_Qualifier == "QTI", "QU", WIN_Value_Qualifier)))%>%
  #mutate('Result_Value(ug/L)' = ifelse(Result_Value(ug/L) <20, 20, Result_Value(ug/L)))
  mutate(WIN_Value_Qualifier = ifelse(is.na(WIN_Value_Qualifier), "", WIN_Value_Qualifier))
#change columns
#TN_t <- rename(TN_t, Site = Lake)

#filter out rows that are not stations "1"-"10" and sites that are not Project Coast 
TN_t <- TN_t %>%
  filter(Station %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "B"))%>%
  filter(Lake %in% c("CIT-CRY", "CIT-HOM", "CIT-WIT", "HER-CHA", "HER-WEE", "PAS-ANC", "PAS-ARI", "PAS-HUD", "PAS-PIT",
                     "Old Greg- Bottom",
                     "Old Greg- 10M",
                     "Old Greg- 50M",
                     "Scar Hole- Bottom",
                     "Scar Hole- 10M",
                     "Scar Hole- 50M",
                     "Crack- Bottom",
                     "Crack- 10M",
                     "Crack- 50M")) 


#join with Secchi for start time
TN_Time = full_join(TN_t, Secchi_Join, 
                    by = c("County", "Lake", "Activity_Start_Date", "Station"))

#Add columns

TN_WIN <- TN_Time 

TN_WIN$Project_ID <- "21FLUFSW"
TN_WIN$Sampling_Agency_Name <- "University of Florida (Soil and Water Sciences Department)"
TN_WIN$Matrix <- "AQUEOUS-Surface Water"
TN_WIN$Monitoring_Location_ID = paste0(TN_t$Lake,"-",TN_t$Station)
TN_WIN$Activity_ID = paste0(TN_WIN$Monitoring_Location_ID,"-",TN_t$Activity_Start_Date,"S")
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
TN_WIN$Preparation_Date_Time = paste0(TN_t$Preparation_Date," ",TN_t$Preparation_Time)
TN_WIN$Analysis_Date_Time = paste0(TN_t$Analysis_Date," ",TN_t$Analysis_Time) 
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



##Change date to avoid overwriting older files##
write.table(TN_Print, file = "Output/TN_NOV_2025.txt", sep = "|", na = "", row.names = FALSE, quote = FALSE)

### CHLcor ----

#Change format of times
v102<- c(CHLcor$Activity_Start_Time)
x102 <- lubridate::parse_date_time(v102,'H:M:S')
format(x102, format = '%I:%M:%S %p')
#Change format of times
v103 <- c(CHLcor$Analysis_Time)
x103 <- lubridate::parse_date_time(v103,'H:M:S')
format(x103, format = '%I:%M:%S %p')
#Change format of times
v104<- c(CHLcor$Preparation_Time)
x104 <- lubridate::parse_date_time(v104,'H:M:S')
format(x104, format = '%I:%M:%S %p')

#change format of dates
v109<- c(CHLcor$Activity_Start_Date)
x109 <- lubridate::parse_date_time(v109,'"%m%d%y"')
format(x109, format = '%m/%d/%Y')
#change format of dates
#v8<- c(TN$Activity_Start_Date)
#x8 <- lubridate::parse_date_time(v8,'"%m%d%y"')
#format(x8, format = '%m/%d/%Y')
#change format of dates
#v8<- c(TN$Activity_Start_Date)
#x8 <- lubridate::parse_date_time(v8,'"%m%d%y"')
#format(x8, format = '%m/%d/%Y')

#change columns
CHLcor <- rename(CHLcor, Org_Result_Value = 'Result_Value(ug/L)')

CHLcor_t = CHLcor%>%
  # mutate('Activity_Start_Time'=format(x102, format = '%I:%M:%S %p')) %>%
  # mutate('Analysis_Time'=format(x103, format = '%I:%M:%S %p'))%>%
  # mutate('Preparation_Time'=format(x104, format = '%I:%M:%S %p'))%>%
  mutate('Activity_Start_Date'=format(x109, format = '%m/%d/%Y')) %>%
  mutate(WIN_Value_Qualifier = ifelse(WIN_Value_Qualifier %in% c("T", "TI"), "U", 
                                      ifelse(WIN_Value_Qualifier == "QTI", "QU", WIN_Value_Qualifier)))%>%
  mutate(Org_Result_Value = ifelse(Org_Result_Value <1, 1, Org_Result_Value))%>%
  mutate(WIN_Value_Qualifier = ifelse(is.na(WIN_Value_Qualifier), "", WIN_Value_Qualifier))


#filter out rows that are not stations "1"-"10" and sites that are not Project Coast 
CHLcor_t <-CHLcor_t %>%
  filter(Station %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "B"))%>%
  filter(Lake %in% c("CIT-CRY", "CIT-HOM", "CIT-WIT", "HER-CHA", "HER-WEE", "PAS-ANC", "PAS-ARI", "PAS-HUD", "PAS-PIT"))


#join with Secchi for start time
CHLcor_time = full_join(CHLcor_t, Secchi_Join, 
                        by = c("County", "Lake", "Activity_Start_Date", "Station"))

#Add columns

CHLcor_WIN <-CHLcor_time 

CHLcor_WIN$Project_ID <- "21FLUFSW"
CHLcor_WIN$Sampling_Agency_Name <- "University of Florida (Soil and Water Sciences Department)"
CHLcor_WIN$Matrix <- "AQUEOUS-Surface Water"
CHLcor_WIN$Monitoring_Location_ID = paste0(CHLcor_t$Lake,"-",CHLcor_t$Station)
CHLcor_WIN$Activity_ID = paste0(CHLcor_WIN$Monitoring_Location_ID,"-",CHLcor_t$Activity_Start_Date,"S")
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
CHLcor_WIN$Preparation_Date_Time = paste0(CHLcor_t$Preparation_Date," ",CHLcor_t$Preparation_Time)
CHLcor_WIN$Analysis_Date_Time = paste0(CHLcor_t$Analysis_Date," ",CHLcor_t$Analysis_Time) 
CHLcor_WIN$Activity_Date_Time = paste0(CHLcor_time$Activity_Start_Date," ",CHLcor_time$Activity_Start_Time.y)
CHLcor_WIN$Activity_Time_Zone.y
#CHLcor_WIN$Org_Result_Value = paste0(CHLcor_time$Org_Result_Value)
CHLcor_WIN$Org_Result_Unit = paste("ug/L")
CHLcor_WIN$Org_MDL = paste("1")
CHLcor_WIN$Org_PQL = paste("1")
CHLcor_WIN$Org_Detection_Unit = paste("ug/L")
CHLcor_WIN$Value_Qualifier = paste0(CHLcor_t$WIN_Value_Qualifier)
CHLcor_WIN$Result_Comments = paste(" ")
CHLcor_WIN$Dilution = paste("1")
CHLcor_WIN$Result_Value_Type_Name = paste("Actual")
CHLcor_WIN$Lab_ID = paste("21FLKWAT")
CHLcor_WIN$Lab_Accreditation_Authority = paste("None")
CHLcor_WIN$Lab_Sample_ID = paste0(CHLcor_WIN$Activity_ID)


#Reorder columns
CHLcor_Print <-CHLcor_WIN[,c("Project_ID","Sampling_Agency_Name","Matrix",
                             "Monitoring_Location_ID","Activity_ID","ADAPT_Analyte_ID","Org_Analyte_Name",
                             "Activity_Type","Sample_Collection_Type","Sample_Collection_Equipment",
                             "Activity_Depth","Activity_Depth_Unit","Total_Depth",
                             "Total_Depth_Unit","Analysis_Method","Sample_Fraction",
                             "Preparation_Date_Time","Preparation_Time_Zone","Analysis_Date_Time",
                             "Analysis_Time_Zone","Activity_Date_Time","Activity_Time_Zone.y",
                             "Org_Result_Value","Org_Result_Unit","Org_MDL",
                             "Org_PQL","Org_Detection_Unit","Value_Qualifier",
                             "Result_Comments","Result_Value_Type_Name","Dilution",
                             "Lab_ID","Lab_Accreditation_Authority","Lab_Sample_ID")]  

##Change date to avoid overwriting older files##
write.table(CHLcor_Print, file = "Output/CHLcor_NOV_2025.txt", sep = "|", na = "", row.names = FALSE, quote = FALSE)



### CHLun ----

#Change format of times
v102<- c(CHLun$Activity_Start_Time)
x102 <- lubridate::parse_date_time(v102,'H:M:S')
format(x102, format = '%I:%M:%S %p')
#Change format of times
v103 <- c(CHLun$Analysis_Time)
x103 <- lubridate::parse_date_time(v103,'H:M:S')
format(x103, format = '%I:%M:%S %p')
#Change format of times
v104<- c(CHLun$Preparation_Time)
x104 <- lubridate::parse_date_time(v104,'H:M:S')
format(x104, format = '%I:%M:%S %p')

#change format of dates
v109<- c(CHLun$Activity_Start_Date)
x109 <- lubridate::parse_date_time(v109,'"%m%d%y"')
format(x109, format = '%m/%d/%Y')
#change format of dates
#v8<- c(TN$Activity_Start_Date)
#x8 <- lubridate::parse_date_time(v8,'"%m%d%y"')
#format(x8, format = '%m/%d/%Y')
#change format of dates
#v8<- c(TN$Activity_Start_Date)
#x8 <- lubridate::parse_date_time(v8,'"%m%d%y"')
#format(x8, format = '%m/%d/%Y')

#change columns
CHLun <- rename(CHLun, Org_Result_Value = 'Result_Value(ug/L)')

CHLun_t = CHLun%>%
  # mutate('Activity_Start_Time'=format(x102, format = '%I:%M:%S %p')) %>%
  # mutate('Analysis_Time'=format(x103, format = '%I:%M:%S %p'))%>%
  # mutate('Preparation_Time'=format(x104, format = '%I:%M:%S %p'))%>%
  mutate('Activity_Start_Date'=format(x109, format = '%m/%d/%Y')) %>%
  mutate(WIN_Value_Qualifier = ifelse(WIN_Value_Qualifier %in% c("T", "TI"), "U", 
                                      ifelse(WIN_Value_Qualifier == "QTI", "QU", WIN_Value_Qualifier)))%>%
  mutate(Org_Result_Value = ifelse(Org_Result_Value <1, 1, Org_Result_Value))%>%
  mutate(WIN_Value_Qualifier = ifelse(is.na(WIN_Value_Qualifier), "", WIN_Value_Qualifier))

#change columns
#CHLun_t <- rename(CHLun_t, Site = Lake)

#filter out rows that are not stations "1"-"10" and sites that are not Project Coast 
CHLun_t <-CHLun_t %>%
  filter(Station %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "B"))%>%
  filter(Lake %in% c("CIT-CRY", "CIT-HOM", "CIT-WIT", "HER-CHA", "HER-WEE", "PAS-ANC", "PAS-ARI", "PAS-HUD", "PAS-PIT"))


#join with Secchi for start time
CHLun_time = full_join(CHLun_t, Secchi_Join, 
                       by = c("County", "Lake", "Activity_Start_Date", "Station"))

#Add columns

CHLun_WIN <-CHLun_time 

CHLun_WIN$Project_ID <- "21FLUFSW"
CHLun_WIN$Sampling_Agency_Name <- "University of Florida (Soil and Water Sciences Department)"
CHLun_WIN$Matrix <- "AQUEOUS-Surface Water"
CHLun_WIN$Monitoring_Location_ID = paste0(CHLun_t$Lake,"-",CHLun_t$Station)
CHLun_WIN$Activity_ID = paste0(CHLun_WIN$Monitoring_Location_ID,"-",CHLun_t$Activity_Start_Date,"S")
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
CHLun_WIN$Preparation_Date_Time = paste0(CHLun_t$Preparation_Date," ",CHLun_t$Preparation_Time)
CHLun_WIN$Analysis_Date_Time = paste0(CHLun_t$Analysis_Date," ",CHLun_t$Analysis_Time) 
CHLun_WIN$Activity_Date_Time = paste0(CHLun_time$Activity_Start_Date," ",CHLun_time$Activity_Start_Time.y)
CHLun_WIN$Activity_Time_Zone.y
CHLun_WIN$Org_Result_Unit = paste("ug/L")
CHLun_WIN$Org_MDL = paste("1")
CHLun_WIN$Org_PQL = paste("1")
CHLun_WIN$Org_Detection_Unit = paste("ug/L")
CHLun_WIN$Value_Qualifier = paste0(CHLun_time$WIN_Value_Qualifier)
CHLun_WIN$Result_Comments = paste(" ")
CHLun_WIN$Result_Value_Type_Name = paste("Actual")
CHLun_WIN$Dilution = paste("1")
CHLun_WIN$Lab_ID = paste("21FLKWAT")
CHLun_WIN$Lab_Accreditation_Authority = paste("None")
CHLun_WIN$Lab_Sample_ID = paste0(CHLun_WIN$Activity_ID)


#Reorder columns
CHLun_Print <-CHLun_WIN[,c("Project_ID","Sampling_Agency_Name","Matrix",
                           "Monitoring_Location_ID","Activity_ID","ADAPT_Analyte_ID","Org_Analyte_Name",
                           "Activity_Type","Sample_Collection_Type","Sample_Collection_Equipment",
                           "Activity_Depth","Activity_Depth_Unit","Total_Depth",
                           "Total_Depth_Unit","Analysis_Method","Sample_Fraction",
                           "Preparation_Date_Time","Preparation_Time_Zone","Analysis_Date_Time",
                           "Analysis_Time_Zone","Activity_Date_Time","Activity_Time_Zone.y",
                           "Org_Result_Value","Org_Result_Unit","Org_MDL",
                           "Org_PQL","Org_Detection_Unit","Value_Qualifier",
                           "Result_Comments","Result_Value_Type_Name","Dilution",
                           "Lab_ID","Lab_Accreditation_Authority","Lab_Sample_ID")]  

##Change date to avoid overwriting older files##
write.table(CHLun_Print, file = "Output/CHLun_NOV_2025.txt", sep = "|", na = "", row.names = FALSE, quote = FALSE)



Secchi_All <- Secchi_t %>%
  transmute(
    County,
    Lake,
    Station,
    Activity_Start_Date,
    Parameter = "Secchi (ft)",
    Result_Value = Org_Result_Value,
    Value_Qualifier = WIN_Value_Qualifier
  )

TP_All <- TP_t %>%
  transmute(
    County,
    Lake,
    Station,
    Activity_Start_Date,
    Parameter = "Total Phosphorus (ug/L)",
    Result_Value = `Result_Value(ug/L)`,
    Value_Qualifier = WIN_Value_Qualifier
  )

TN_All <- TN_t %>%
  transmute(
    County,
    Lake,
    Station,
    Activity_Start_Date,
    Parameter = "Total Nitrogen (ug/L)",
    Result_Value = `Result_Value(ug/L)`,
    Value_Qualifier = WIN_Value_Qualifier
  )

CHLcor_All <- CHLcor_t %>%
  transmute(
    County,
    Lake,
    Station,
    Activity_Start_Date,
    Parameter = "Chlorophyll-a (Corrected, ug/L)",
    Result_Value = Org_Result_Value,
    Value_Qualifier = WIN_Value_Qualifier
  )

CHLun_All <- CHLun_t %>%
  transmute(
    County,
    Lake,
    Station,
    Activity_Start_Date,
    Parameter = "Chlorophyll-a (Uncorrected, ug/L)",
    Result_Value = Org_Result_Value,
    Value_Qualifier = WIN_Value_Qualifier
  )


### COMBINE ALL PARAMETERS ----

All_Coast <- bind_rows(Secchi_All, TP_All, TN_All, CHLcor_All, CHLun_All) %>%
  arrange(County, Lake, Station, Activity_Start_Date, Parameter)

# Create a wide version for viewing in Excel
All_Coast_wide <- All_Coast %>%
  pivot_wider(names_from = Parameter, values_from = c(Result_Value, Value_Qualifier))

# Write 
write_csv(All_Coast_wide, "Output/All_Coast_NOV2025.csv", na = "")