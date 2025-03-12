# Project Notes ----

#Still need to figure out:
    #If value qualifier = S, then total Depth needs to = org result value.Function or mutate this?
    #adding the mon ID to secchi join and 
    #add the mutate for the naming functions.
    #Need to join all files with mon ID separately since they are likely to not have all the same lakes/sta combos. Wrong- see next point

    #Do we need to worry about non matches or can we upload and let the data coalesce in the WIN system? 
    #Nope, Activity times come from Secchi so anything that doesn't match to a secchi date won't go in.
    #Subset these non matches and add them to a non match folder in the project?
##Also add by joining this secchi with old non matches for each run through this code


#Before running this code, 
    #Copy the new version of files to Data folder using Data Import names
    #Adjust column names to work with code
    #Delete rows without activity times from secchi and rows without filtering times from CHL files (both cor and un)
    #Change output file names to new upload date

#Trying with Secchi first - Secchi will have to come first since Activity Start date/time come from this file for all parameters.

#Sampling Agency Names : ST. ANDREW BAY RESOURCE MANAGEMENT ASSOCIATION INC. and CHOCTAWHATCHEE BASIN ALLIANCE and Florida Lakewatch
#Project ID: BAYRALLY	or 21FLKWAT

### WD and Packages ----

setwd("~/Documents/GitHub/WIN File Creation")
library(tidyverse)
library(readxl)
library(dplyr)
library(readr)
library(lubridate)


### Data Import ---- 


Mon_ID <- read_csv("Data/LW/Lakewatch Monitoring Location ID.csv")
TP <- read_csv("Data/LW/TP Stacked.csv")
TN <- read_csv("Data/LW/TN Stacked.csv")
CHL_cor <- read_csv("Data/LW/Chl cor Stacked.csv")
CHL_un <- read_csv("Data/LW/Chl un Stacked.csv")
Secchi <- read_csv("Data/LW/Secchi Stacked.csv")
Color <- read_csv("Data/LW/Color Stacked.csv")
Cond <- read_csv("Data/LW/Cond Stacked.csv")
##Read in non-match files from last time, start storing these in this repository?


### Functions ----

## FOR ALL PARAMETERS ##


# For changing the Sampling Agency Names _unsure if working
#Function or mutate? Can I make a vector containing all the lakes for these categories to plug in here?
change_sampling_names <- function(Sampling_Agency_Name){
  if(County=="Bay"){
    Sampling_Agency_Name <- "ST. ANDREW BAY RESOURCE MANAGEMENT ASSOCIATION INC."
  }else if(Lake=="ADD ALL HOLMES CBA River"){
    Sampling_Agency_Name <- "CHOCTAWHATCHEE BASIN ALLIANCE"
  }else if(Lake=="ADD ALL Okaloosa CBA"){
    Sampling_Agency_Name <- "CHOCTAWHATCHEE BASIN ALLIANCE"
  }else if(Lake=="ADD ALL Walton CBA"){
    Sampling_Agency_Name <- "CHOCTAWHATCHEE BASIN ALLIANCE"
  }else
    Sampling_Agency_Name <- "Florida Lakewatch"
  return(Sampling_Agency_Name)
}

# For changing the Project ID_unsure if working


change_project_id <- function(Project_ID){
  if(Sampling_Agency_Name=="CHOCTAWHATCHEE BASIN ALLIANCE"){
    Project_ID <- "BAYRALLY"
  }else{
    Project_ID <- "21FLKWAT"
  }
  return(Project_ID)
}



## FOR SECCHI ONLY ##

# # If value qualifier = S, then total Depth needs to = org result value.
# s_value_qualifiers <- function(Value_Qualifier, Org_Result_Value, Total_Depth) {
#   Org_Result_Value[Value_Qualifier == "S"] <- Total_Depth[Value_Qualifier == "S"]
#   return(Org_Result_Value)
# }


#For blank org result value, org result value should = "Not Reported".
not_reported_secchi <- function(Org_Result_Value){
  ifelse(is.na(Org_Result_Value), "Not Reported", Org_Result_Value)
}


### JOIN DATA WITH BASE FILE FOR MONITORING LOCATION ID ----
#this will need to be done for all parameters

Mon_ID = Mon_ID %>%
  mutate(Station = as.character(Station)) %>%
  mutate(Lake_County_Sta = paste(Lake, County, Station)) 


Mon_ID$Project_ID <- "21FLKWAT"
Mon_ID$Sampling_Agency_Name <- "Florida Lakewatch"


Secchi = Secchi%>%
  mutate(Station = as.character(Station)) %>%
  mutate(Lake_County_Sta = paste(Lake, County, Station)) 
 

Secchi_ID  <- Secchi %>% inner_join(Mon_ID, by = c("Lake_County_Sta")) 
#select(-County.y, -Lake.y, -Station.y, -Study)

### RECYCLE NON_MATCHES ----

#Join each non match file with Secchi for start time and concat it with its parameter file for this round -
#make sure to remove these lines from the non match and resave non match file as new 

### SECCHI ----

#Change format of times
v1<- c(Secchi_ID$Activity_Start_Time)
x <- lubridate::parse_date_time(v1,'H:M:S')
format(x, format = '%I:%M:%S %p')


#change format of dates
v2<- c(Secchi_ID$Activity_Start_Date)
x2 <- lubridate::parse_date_time(v2,'"%m%d%y"')
format(x2, format = '%m/%d/%Y')


Secchi_t <- Secchi_ID%>%
  mutate('Activity_Start_Time'=format(x, format = '%I:%M:%S %p')) %>%
  mutate('Activity_Start_Date'=format(x2, format = '%m/%d/%Y')) %>%
  #mutate('Total Depth' = s_value_qualifiers(Value_Qualifier, Org_Result_Value, Total_Depth)) %>%
  mutate('Org_Result_Value' = not_reported_secchi(Org_Result_Value))



#Add columns

Secchi_WIN <- Secchi_t

Secchi_WIN$ADAPT_Analyte_ID <- "WIN-010"
Secchi_WIN$Sampling_Agency_Name <- "Florida Lakewatch"
Secchi_WIN$Project_ID <- "21FLKWAT"
Secchi_WIN$Matrix <- "AQUEOUS-Surface Water"
Secchi_WIN$Monitoring_Location_ID = paste0(Secchi_t$Monitoring_Location_ID)
Secchi_WIN$Activity_ID = paste0(Secchi_WIN$Monitoring_Location_ID,"-",Secchi_t$Activity_Start_Date,"F")
Secchi_WIN$Activity_Date_Time = paste0(Secchi_t$Activity_Start_Date," ",Secchi_t$Activity_Start_Time)
Secchi_WIN$Preparation_Date_Time = paste0(" ")
Secchi_WIN$Analysis_Date_Time = paste0(" ")  
Secchi_WIN$Lab_ID = paste("21FLKWAT")
Secchi_WIN$Lab_Accreditation_Authority = paste("None")
Secchi_WIN$Lab_Sample_ID = paste0(Secchi_WIN$Activity_ID)


#Reorder columns
Secchi_Print <- Secchi_WIN[,c("Project_ID","Sampling_Agency_Name","Matrix",
                              "Monitoring_Location_ID","Activity_ID","ADAPT_Analyte_ID","Org Analyte Name",
                              "Activity Type","Sample Collection Type","Sample Collection Equipment",
                              "Activity Depth","Activity Depth Unit","Total_Depth",
                              "Total Depth Unit","Analysis Method","Sample Fraction",
                              "Preparation_Date_Time","Preparation Time Zone","Analysis_Date_Time",
                              "Analysis Time Zone","Activity_Date_Time","Activity Time Zone",
                              "Org_Result_Value","Org Result Unit","Org MDL",
                              "Org PQL","Org Detection Unit","Value_Qualifier",
                              "Result Comments","Result Value Type Name","Dilution",
                              "Lab_ID","Lab_Accreditation_Authority","Lab_Sample_ID")]



### Change date to avoid overwriting older files
### Filter by Sampling Agency Name to separate files
CBA_Secchi <- Secchi_Fun %>% filter(Sampling_Agency_Name == "CHOCTAWHATCHEE BASIN ALLIANCE")
write.table(CBA_Secchi, file = "Output/CBA_Secchi_Upload_6-27-24.txt", sep = "|", na = "", row.names = FALSE, quote = FALSE)

LW_Secchi <- Secchi_Fun %>% filter(Sampling_Agency_Name == "Florida Lakewatch")
write.table(LW_Secchi, file = "Output/LW_Secchi_Upload_6-27-24.txt", sep = "|", na = "", row.names = FALSE, quote = FALSE)


###Subset Secchi Start Time for join with TN TP

Secchi_Join <- Secchi_WIN[,c("County", "Lake", "Activity_Start_Date", "Activity_Start_Time", "Station")]

### TP ----

#Change format of times
v3 <- c(TP$Analysis_Time)
x3 <- lubridate::parse_date_time(v3,'H:M:S')
format(x3, format = '%I:%M:%S %p')

#Change format of times
v4<- c(TP$Preparation_Time)
x4 <- lubridate::parse_date_time(v4,'H:M:S')
format(x4, format = '%I:%M:%S %p')


#change format of dates
v5<- c(TP$Activity_Start_Date)
x5 <- lubridate::parse_date_time(v5,'"%m%d%y"')
format(x5, format = '%m/%d/%Y')

TP_t = TP%>%
  mutate('Activity_Start_Date'=format(x5, format = '%m/%d/%Y')) %>%
  mutate('Analysis_Time'=format(x2, format = '%I:%M:%S %p'))%>%
  mutate('Preparation_Time'=format(x3, format = '%I:%M:%S %p'))%>%
  # For all "T" value qualifiers, make the result value = the mdl
  mutate(Value_Qualifier = ifelse(Value_Qualifier %in% c("T", "TI"), "U", 
                                  ifelse(Value_Qualifier == "QTI", "QU", Value_Qualifier)))


#join with Secchi for start time

TP_Time = full_join(TP_t, Secchi_Join, 
                    by = c("County", "Site", "Activity_Start_Date", "Station"))
#Add columns

TP_WIN <- TP_Time 

TP_WIN$Project_ID <- "21FLUFSW"
TP_WIN$Sampling_Agency_Name <- "University of Florida (Soil and Water Sciences Department)"
TP_WIN$Matrix <- "AQUEOUS-Surface Water"
TP_WIN$Monitoring_Location_ID = paste0(TP$Site,"-",TP$Station)
TP_WIN$Activity_ID = paste0(TP_WIN$Monitoring_Location_ID,"-",TP_t$Activity_Start_Date,"S")
TP_WIN$Activity_Date_Time = paste0(TP_Time$Activity_Start_Date," ",TP_Time$Activity_Start_Time.y)
TP_WIN$Preparation_Date_Time = paste0(TP_t$Preparation_Date," ",TP_t$Preparation_Time)
TP_WIN$Analysis_Date_Time = paste0(TP_t$Analysis_Date," ",TP_t$Analysis_Time)  
TP_WIN$Lab_ID = paste("21FLKWAT")
TP_WIN$Lab_Accreditation_Authority = paste("None")
TP_WIN$Lab_Sample_ID = paste0(TP_WIN$Activity_ID)

#FIX value qualifiers
#mutate('Value Qualifier' = t_value_qualifiers) 


#Reorder columns
TP_Print <- TP_WIN[,c("Project_ID","Sampling_Agency_Name","Matrix",
                      "Monitoring_Location_ID","Activity_ID","ADAPT Analyte ID","Org Analyte Name",
                      "Activity Type","Sample Collection Type","Sample Collection Equipment",
                      "Activity Depth","Activity Depth Unit","Total Depth",
                      "Total Depth Unit","Analysis Method","Sample Fraction",
                      "Preparation_Date_Time","Preparation Time Zone","Analysis_Date_Time",
                      "Analysis Time Zone","Activity_Date_Time","Activity Time Zone",
                      "Org Result Value","Org Result Unit","Org MDL",
                      "Org PQL","Org Detection Unit","Value Qualifier",
                      "Result Comments","Result Value Type Name","Dilution",
                      "Lab_ID","Lab_Accreditation_Authority","Lab_Sample_ID")]



##Change date to avoid overwriting older files##
write.table(TP_Print, file = "Output/TP_Upload_6-27-24.txt", sep = "|", na = "", row.names = FALSE, quote = FALSE)



### TN ----

#Change format of times
v6 <- c(TN$Analysis_Time)
x6 <- lubridate::parse_date_time(v6,'H:M:S')
format(x6, format = '%I:%M:%S %p')

#Change format of times
v7<- c(TN$Preparation_Time)
x7 <- lubridate::parse_date_time(v7,'H:M:S')
format(x7, format = '%I:%M:%S %p')


#change format of dates
v8<- c(TN$Activity_Start_Date)
x8 <- lubridate::parse_date_time(v8,'"%m%d%y"')
format(x8, format = '%m/%d/%Y')

TN_t = TN%>%
  mutate('Activity_Start_Date'=format(x8, format = '%m/%d/%Y')) %>%
  mutate('Analysis_Time'=format(x6, format = '%I:%M:%S %p'))%>%
  mutate('Preparation_Time'=format(x7, format = '%I:%M:%S %p'))%>%
  mutate(Value_Qualifier = ifelse(Value_Qualifier %in% c("T", "TI"), "U", 
                                  ifelse(Value_Qualifier == "QTI", "QU", Value_Qualifier)))
#join with Secchi for start time

TN_Time = full_join(TN_t, Secchi_Join, 
                    by = c("County", "Site", "Activity_Start_Date", "Station"))

#Add columns

TN_WIN <- TN_Time 

TN_WIN$Project_ID <- "21FLUFSW"
TN_WIN$Sampling_Agency_Name <- "University of Florida (Soil and Water Sciences Department)"
TN_WIN$Matrix <- "AQUEOUS-Surface Water"
TN_WIN$Monitoring_Location_ID = paste0(TN$Site,"-",TN$Station)
TN_WIN$Activity_ID = paste0(TN_WIN$Monitoring_Location_ID,"-",TN_t$Activity_Start_Date,"S")
TN_WIN$Activity_Date_Time = paste0(TN_Time$Activity_Start_Date," ",TN_Time$Activity_Start_Time.y)
TN_WIN$Preparation_Date_Time = paste0(TN_t$Preparation_Date," ",TN_t$Preparation_Time)
TN_WIN$Analysis_Date_Time = paste0(TN_t$Analysis_Date," ",TN_t$Analysis_Time)  
TN_WIN$Lab_ID = paste("21FLKWAT")
TN_WIN$Lab_Accreditation_Authority = paste("None")
TN_WIN$Lab_Sample_ID = paste0(TN_WIN$Activity_ID)


#Reorder columns
TN_Print <- TN_WIN[,c("Project_ID","Sampling_Agency_Name","Matrix",
                      "Monitoring_Location_ID","Activity_ID","ADAPT Analyte ID","Org Analyte Name",
                      "Activity Type","Sample Collection Type","Sample Collection Equipment",
                      "Activity Depth","Activity Depth Unit","Total Depth",
                      "Total Depth Unit","Analysis Method","Sample Fraction",
                      "Preparation_Date_Time","Preparation Time Zone","Analysis_Date_Time",
                      "Analysis Time Zone","Activity_Date_Time","Activity Time Zone",
                      "Org Result Value","Org Result Unit","Org MDL",
                      "Org PQL","Org Detection Unit","Value Qualifier",
                      "Result Comments","Result Value Type Name","Dilution",
                      "Lab_ID","Lab_Accreditation_Authority","Lab_Sample_ID")]



##Change date to avoid overwriting older files##
write.table(TN_Print, file = "Output/TN_Upload_6-27-24.txt", sep = "|", na = "", row.names = FALSE, quote = FALSE)

### CHL cor ----


#Change format of times
v9<- c(CHL_cor$Activity_Start_Time)
x9 <- lubridate::parse_date_time(v9,'H:M:S')
format(x9, format = '%I:%M:%S %p')
#Change format of times
v10 <- c(CHL_cor$Analysis_Time)
x10 <- lubridate::parse_date_time(v10,'H:M:S')
format(x10, format = '%I:%M:%S %p')
#Change format of times
v101<- c(CHL_cor$Preparation_Time)
x101 <- lubridate::parse_date_time(v101,'H:M:S')
format(x101, format = '%I:%M:%S %p')

#change format of dates
#v2<- c(Secchi$Activity_Start_Date)
#x2 <- lubridate::parse_date_time(v2,'"%m%d%y"')
#format(x2, format = '%m/%d/%Y')
#change format of dates
#v8<- c(TN$Activity_Start_Date)
#x8 <- lubridate::parse_date_time(v8,'"%m%d%y"')
#format(x8, format = '%m/%d/%Y')
#change format of dates
#v8<- c(TN$Activity_Start_Date)
#x8 <- lubridate::parse_date_time(v8,'"%m%d%y"')
#format(x8, format = '%m/%d/%Y')


CHL_cor_t = CHL_cor%>%
  mutate('Activity_Start_Time'=format(x9, format = '%I:%M:%S %p')) %>%
  mutate('Analysis_Time'=format(x10, format = '%I:%M:%S %p'))%>%
  mutate('Preparation_Time'=format(x101, format = '%I:%M:%S %p'))%>%
  mutate(Value_Qualifier = ifelse(Value_Qualifier %in% c("T", "TI"), "U", 
                                  ifelse(Value_Qualifier == "QTI", "QU", Value_Qualifier)))


#Add columns

CHL_cor_WIN <- CHL_cor_t 

CHL_cor_WIN$Project_ID <- "21FLUFSW"
CHL_cor_WIN$Sampling_Agency_Name <- "University of Florida (Soil and Water Sciences Department)"
CHL_cor_WIN$Matrix <- "AQUEOUS-Surface Water"
CHL_cor_WIN$Monitoring_Location_ID = paste0(CHL_cor$Site,"-",CHL_cor$Station)
CHL_cor_WIN$Activity_ID = paste0(CHL_cor_WIN$Monitoring_Location_ID,"-",CHL_cor_t$Activity_Start_Date,"S")
CHL_cor_WIN$Activity_Date_Time = paste0(CHL_cor_t$Activity_Start_Date," ",CHL_cor_t$Activity_Start_Time)
CHL_cor_WIN$Preparation_Date_Time = paste0(CHL_cor_t$Preparation_Date," ",CHL_cor_t$Preparation_Time)
CHL_cor_WIN$Analysis_Date_Time = paste0(CHL_cor_t$Analysis_Date," ",CHL_cor_t$Analysis_Time)  
CHL_cor_WIN$Lab_ID = paste("21FLKWAT")
CHL_cor_WIN$Lab_Accreditation_Authority = paste("None")
CHL_cor_WIN$Lab_Sample_ID = paste0(CHL_cor_WIN$Activity_ID)


#Reorder columns
CHL_cor_Print <- CHL_cor_WIN[,c("Project_ID","Sampling_Agency_Name","Matrix",
                                "Monitoring_Location_ID","Activity_ID","ADAPT Analyte ID","Org Analyte Name",
                                "Activity Type","Sample Collection Type","Sample Collection Equipment",
                                "Activity Depth","Activity Depth Unit","Total Depth",
                                "Total Depth Unit","Analysis Method","Sample Fraction",
                                "Preparation_Date_Time","Preparation Time Zone","Analysis_Date_Time",
                                "Analysis Time Zone","Activity_Date_Time","Activity Time Zone",
                                "Org Result Value","Org Result Unit","Org MDL",
                                "Org PQL","Org Detection Unit","Value Qualifier",
                                "Result Comments","Result Value Type Name","Dilution",
                                "Lab_ID","Lab_Accreditation_Authority","Lab_Sample_ID")]



##Change date to avoid overwriting older files##
write.table(CHL_cor_Print, file = "Output/CHL_cor_Upload_6-27-24.txt", sep = "|", na = "", row.names = FALSE, quote = FALSE)

### CHL un ----

#Change format of times
v102<- c(CHL_un$Activity_Start_Time)
x102 <- lubridate::parse_date_time(v102,'H:M:S')
format(x102, format = '%I:%M:%S %p')
#Change format of times
v103 <- c(CHL_un$Analysis_Time)
x103 <- lubridate::parse_date_time(v103,'H:M:S')
format(x103, format = '%I:%M:%S %p')
#Change format of times
v104<- c(CHL_un$Preparation_Time)
x104 <- lubridate::parse_date_time(v104,'H:M:S')
format(x104, format = '%I:%M:%S %p')

#change format of dates
#v2<- c(Secchi$Activity_Start_Date)
#x2 <- lubridate::parse_date_time(v2,'"%m%d%y"')
#format(x2, format = '%m/%d/%Y')
#change format of dates
#v8<- c(TN$Activity_Start_Date)
#x8 <- lubridate::parse_date_time(v8,'"%m%d%y"')
#format(x8, format = '%m/%d/%Y')
#change format of dates
#v8<- c(TN$Activity_Start_Date)
#x8 <- lubridate::parse_date_time(v8,'"%m%d%y"')
#format(x8, format = '%m/%d/%Y')


CHL_un_t = CHL_un%>%
  mutate('Activity_Start_Time'=format(x102, format = '%I:%M:%S %p')) %>%
  mutate('Analysis_Time'=format(x103, format = '%I:%M:%S %p'))%>%
  mutate('Preparation_Time'=format(x104, format = '%I:%M:%S %p'))%>%
  mutate(Value_Qualifier = ifelse(Value_Qualifier %in% c("T", "TI"), "U", 
                                  ifelse(Value_Qualifier == "QTI", "QU", Value_Qualifier)))


#Add columns

CHL_un_WIN <- CHL_un_t 

CHL_un_WIN$Project_ID <- "21FLUFSW"
CHL_un_WIN$Sampling_Agency_Name <- "University of Florida (Soil and Water Sciences Department)"
CHL_un_WIN$Matrix <- "AQUEOUS-Surface Water"
CHL_un_WIN$Monitoring_Location_ID = paste0(CHL_un$Site,"-",CHL_un$Station)
CHL_un_WIN$Activity_ID = paste0(CHL_un_WIN$Monitoring_Location_ID,"-",CHL_un_t$Activity_Start_Date,"S")
CHL_un_WIN$Activity_Date_Time = paste0(CHL_un_t$Activity_Start_Date," ",CHL_un_t$Activity_Start_Time)
CHL_un_WIN$Preparation_Date_Time = paste0(CHL_un_t$Preparation_Date," ",CHL_un_t$Preparation_Time)
CHL_un_WIN$Analysis_Date_Time = paste0(CHL_un_t$Analysis_Date," ",CHL_un_t$Analysis_Time)  
CHL_un_WIN$Lab_ID = paste("21FLKWAT")
CHL_un_WIN$Lab_Accreditation_Authority = paste("None")
CHL_un_WIN$Lab_Sample_ID = paste0(CHL_un_WIN$Activity_ID)



#Reorder columns
CHL_un_Print <- CHL_un_WIN[,c("Project_ID","Sampling_Agency_Name","Matrix",
                              "Monitoring_Location_ID","Activity_ID","ADAPT Analyte ID","Org Analyte Name",
                              "Activity Type","Sample Collection Type","Sample Collection Equipment",
                              "Activity Depth","Activity Depth Unit","Total Depth",
                              "Total Depth Unit","Analysis Method","Sample Fraction",
                              "Preparation_Date_Time","Preparation Time Zone","Analysis_Date_Time",
                              "Analysis Time Zone","Activity_Date_Time","Activity Time Zone",
                              "Org Result Value","Org Result Unit","Org MDL",
                              "Org PQL","Org Detection Unit","Value Qualifier",
                              "Result Comments","Result Value Type Name","Dilution",
                              "Lab_ID","Lab_Accreditation_Authority","Lab_Sample_ID")]



##Change date to avoid overwriting older files##
write.table(CHL_un_Print, file = "Output/CHL_un_Upload_6-27-24.txt", sep = "|", na = "", row.names = FALSE, quote = FALSE)

### Add color and Conductivity here ###



