# Project Notes ----
#Before running this code, delete rows with field blanks (sort by station column) copy the new version of Stacked files to Data/Project Coast folder.
#Make sure all Site names are UPPERS
#Sampling Agency Names : University of Florida (Soil and Water Sciences Department)
#Project ID: 21FLUFSW

### WD and Packages ----

setwd("~/Documents/GitHub/WIN File Creation")
library(tidyverse)
library(readxl)
library(dplyr)
library(readr)
library(lubridate)



### Data Import ---- 

TP <- read_csv("Data/Project Coast/TP Stacked.csv")
TN <- read_csv("Data/Project Coast/TN Stacked.csv")
CHL_cor <- read_csv("Data/Project Coast/Chl cor Stacked.csv")
CHL_un <- read_csv("Data/Project Coast/Chl un Stacked.csv")
Secchi <- read_csv("Data/Project Coast/Secchi Stacked.csv")



### Functions ----

## FOR ALL PARAMETERS ##

# For changing T value qualifiers to U

change_value_qualifier <- function(data) {
  # Replace "T" or "TI" with "U"
  data$Value_Qualifier[data$Value_Qualifier %in% c("T", "TI")] <- "U"
  
  # Replace "QTI" with "QU"
  data$Value_Qualifier[data$Value_Qualifier == "QTI"] <- "QU"
  
  return(data)
}

## FOR SECCHI ONLY ##

# If value qualifier = S, then total Depth needs to = org result value.
s_value_qualifiers <- function(Value_Qualifier, Org_Result_Value, Total_Depth) {
  Org_Result_Value[Value_Qualifier == "S"] <- Total_Depth[Value_Qualifier == "S"]
  return(Org_Result_Value)
  }
 

#For blank org result value, org result value should = "Not Reported".
not_reported_secchi <- function(Org_Result_Value){
  ifelse(is.na(Org_Result_Value), "Not Reported", Org_Result_Value)
  }
 

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
  mutate('Activity_Start_Time'=format(x, format = '%I:%M:%S %p')) %>%
  mutate('Activity_Start_Date'=format(x2, format = '%m/%d/%Y')) %>%
  mutate(Value_Qualifier = s_value_qualifiers(Value_Qualifier, Org_Result_Value, Total_Depth)) %>%
  mutate(Org_Result_Value = not_reported_secchi(Org_Result_Value))
  

#Add columns

Secchi_WIN <- Secchi_t

  Secchi_WIN$Project_ID <- "21FLUFSW"
  Secchi_WIN$Sampling_Agency_Name <- "University of Florida (Soil and Water Sciences Department)"
  Secchi_WIN$Matrix <- "AQUEOUS-Surface Water"
  Secchi_WIN$Monitoring_Location_ID = paste0(Secchi$Site,"-",Secchi$Station)
  Secchi_WIN$Activity_ID = paste0(Secchi_WIN$Monitoring_Location_ID,"-",Secchi_t$Activity_Start_Date,"F")
  Secchi_WIN$Activity_Date_Time = paste0(Secchi_t$Activity_Start_Date," ",Secchi_t$Activity_Start_Time)
  Secchi_WIN$Preparation_Date_Time = paste0(" ")
  Secchi_WIN$Analysis_Date_Time = paste0(" ")  
  Secchi_WIN$Lab_ID = paste("21FLKWAT")
  Secchi_WIN$Lab_Accreditation_Authority = paste("None")
  Secchi_WIN$Lab_Sample_ID = paste0(Secchi_WIN$Activity_ID)
  

 #Reorder columns
 Secchi_Print <- Secchi_WIN[,c("Project_ID","Sampling_Agency_Name","Matrix",
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
  
#Subset Secchi Start Time for join with TN TP
 
 Secchi_Join <- Secchi_t[,c("County", "Site", "Activity_Start_Date", "Activity_Start_Time", "Station")]

##Change date to avoid overwriting older files##
write.table(Secchi_Print, file = "Output/Secchi_Upload_5-9-24.txt", sep = "|", na = "", row.names = FALSE, quote = FALSE)



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
write.table(TP_Print, file = "Output/TP_Upload_5-9-24.txt", sep = "|", na = "", row.names = FALSE, quote = FALSE)



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
write.table(TN_Print, file = "Output/TN_Upload_5-9-24.txt", sep = "|", na = "", row.names = FALSE, quote = FALSE)

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
  write.table(CHL_cor_Print, file = "Output/CHL_cor_Upload_5-9-24.txt", sep = "|", na = "", row.names = FALSE, quote = FALSE)

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
  write.table(CHL_un_Print, file = "Output/CHL_un_Upload_5-9-24.txt", sep = "|", na = "", row.names = FALSE, quote = FALSE)

  
  
  
  
  
#Mutate Library for all columns

#mutate('ADAPT Analyte ID' = Secchi_t$ADAPT_Analyte_ID) %>%
#mutate('Org Analyte Name' = paste("Depth, Secchi Disk Depth")) %>%
#mutate('Activity Type' = paste("Field")) %>%  
#mutate('Sample Collection Type' = paste("Field Testing-Discrete")) %>%    
#mutate('Sample Collection Equipment' = paste("Misc Field Device")) %>%
#mutate('Activity Depth' = paste("")) %>%
#mutate('Activity Depth Unit' = paste("")) %>%  
#mutate('Total Depth' = Secchi$`Total Depth`) %>%
#mutate('Total Depth Unit' = Secchi$Total_Depth_Unit) %>%
#mutate('Analysis Method' = paste("FDEP FT1700")) %>%
#mutate('Sample Fraction' = as.character(paste(""))) %>%

# mutate('Activity_Start_Date' = make_date(month = MM, day = DD, year = yyyy)) %>%
# mutate('Activity_Start_Date' = format(Activity_Start_Date, format = "%D")) %>%
#mutate('Activity Start Time' = format(Activity_Start_Time, format = '%I:%M:%S %p')) %>%
#mutate('Preparation Time Zone' = paste("")) %>%

#mutate('Activity_Start_Date' = make_date(month = MM, day = DD, year = YYYY)) %>%
#mutate('Activity_Start_Date' = format(Activity_Start_Date, format = "%D")) %>%
#mutate('Activity_Start_Time' = format(Activity_Start_Time, format = "%R,%p")) %>%
#mutate('Analysis Time Zone' = paste("")) %>%

#mutate('Activity_Start_Date' = make_date(month = MM, day = DD, year = YYYY)) %>%
#mutate('Activity_Start_Date' = format(Activity_Start_Date, format = "%D")) %>%
#mutate('Activity_Start_Time' = format(Activity_Start_Time, format = "%R,%p")) %>%
#mutate('Activity Time Zone' = Secchi$Activity_Time_Zone) %>%

#mutate('Org Result Value' = not_reported_secchi(Secchi_WIN$Org_Result_Value)) %>%
# mutate('Org MDL' = paste("")) %>%
# mutate('Org PQL' = paste("")) %>%
# mutate('Org Detection Unit' = paste("")) %>%
#mutate('Value Qualifier' = s_value_qualifiers) %>%
#mutate('Result Comments' = Secchi_WIN$Result_Comments) %>%
# mutate('Result Value Type Name' = paste("Actual")) %>%
# mutate('Dilution' = paste("")) %>%
