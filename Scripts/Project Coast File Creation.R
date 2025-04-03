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
  TP <- read_csv("Data/Project Coast/TP_STACKED.csv", col_types = cols(
    Preparation_Time = col_character(),
    Analysis_Time = col_character(),
  ))
  
  TN <- read_csv("Data/Project Coast/TN_STACKED.csv", col_types = cols(
    Preparation_Time = col_character(),
    Analysis_Time = col_character(),
  ))
  
  CHL <- read_csv("Data/Project Coast/CHL_STACKED.csv", col_types = cols(
    Preparation_Time = col_character(),
    Analysis_Time = col_character(),
    Station = col_character() 
  ))
  
  Secchi <- read_csv("Data/Project Coast/Secchi_STACKED.csv", col_types = cols(
    Preparation_Time = col_character(),
    Analysis_Time = col_character(),
    Activity_Start_Time = col_character(),
  ))
  
  
  ### SECCHI ----
  
  #Change format of times
  # v1<- c(Secchi$Activity_Start_Time)
  # x <- lubridate::parse_date_time(v1,'H:M:S')
  # format(x, format = '%I:%M:%S %p')
  
  
  #change format of dates
  v2<- c(Secchi$Activity_Start_Date)
  x2 <- lubridate::parse_date_time(v2,'"%m%d%y"')
  format(x2, format = '%m/%d/%Y')
  
  
  Secchi_t <- Secchi%>%
   # mutate('Activity_Start_Time'=format(x, format = '%I:%M:%S %p')) %>%
    mutate('Activity_Start_Date'=format(x2, format = '%m/%d/%Y')) %>%
   # mutate('Total Depth' = mutate_columns(Secchi, "Value_Qualifier", "Total_Depth", "Org_Result_Value")) %>%
   # mutate(Org_Result_Value = not_reported_secchi(Org_Result_Value)) %>%
    mutate(Org_Analyte_Name = "Depth, Secchi Disk Depth")
 
 
  #change columns
  #Secchi_t <- rename(Secchi_t, Site = Lake)
  
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
   
   Secchi_Join <- Secchi_t[,c("County", "Site", "Activity_Start_Date", "Activity_Start_Time", "Station", "Activity_Time_Zone")]
   #make station character to join with others
   Secchi_Join$Station <- as.character(Secchi_Join$Station)
  
  ##Change date to avoid overwriting older files##
  write.table(Secchi_Print, file = "Output/Secchi_Feb2025.txt", sep = "|", na = "", row.names = FALSE, quote = FALSE)
  
  
  
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
                                    ifelse(WIN_Value_Qualifier == "QTI", "QU", WIN_Value_Qualifier)))
  
  #change columns
  TP_t <- rename(TP_t, Site = Lake)
 

  #filter out rows that are not stations "1"-"10" and sites that are not Project Coast 
   TP_t <- TP_t %>%
    filter(Station %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))%>%
    filter(Site %in% c("CIT-CRY", "CIT-HOM", "CIT-WIT", "HER-CHA", "HER-WEE", "PAS-ANC", "PAS-ARI", "PAS-HUD", "PAS-PIT"))
   
  
  #join with Secchi for start time
  
  TP_Time = full_join(TP_t, Secchi_Join, 
                     by = c("County", "Site", "Activity_Start_Date", "Station"))
  #Add columns (rearrange these to match final file for ease of error fixing later)
  
  TP_WIN <- TP_Time 
  
   TP_WIN$Project_ID <- "21FLUFSW"
   TP_WIN$Sampling_Agency_Name <- "University of Florida (Soil and Water Sciences Department)"
   TP_WIN$Matrix <- "AQUEOUS-Surface Water"
   TP_WIN$Monitoring_Location_ID = paste0(TP_t$Site,"-",TP_t$Station)
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
  write.table(TP_Print, file = "Output/TP_Feb2025.txt", sep = "|", na = "", row.names = FALSE, quote = FALSE)
  
  
  
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
                                      ifelse(WIN_Value_Qualifier == "QTI", "QU", WIN_Value_Qualifier)))
    #mutate('Result_Value(ug/L)' = ifelse(Result_Value(ug/L) <20, 20, Result_Value(ug/L)))
  
  #change columns
  TN_t <- rename(TN_t, Site = Lake)
  
  #filter out rows that are not stations "1"-"10" and sites that are not Project Coast 
  TN_t <- TN_t %>%
    filter(Station %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))%>%
    filter(Site %in% c("CIT-CRY", "CIT-HOM", "CIT-WIT", "HER-CHA", "HER-WEE", "PAS-ANC", "PAS-ARI", "PAS-HUD", "PAS-PIT"))
  
  
  #join with Secchi for start time
  TN_Time = full_join(TN_t, Secchi_Join, 
                      by = c("County", "Site", "Activity_Start_Date", "Station"))
  
  #Add columns
  
  TN_WIN <- TN_Time 
  
  TN_WIN$Project_ID <- "21FLUFSW"
  TN_WIN$Sampling_Agency_Name <- "University of Florida (Soil and Water Sciences Department)"
  TN_WIN$Matrix <- "AQUEOUS-Surface Water"
  TN_WIN$Monitoring_Location_ID = paste0(TN_t$Site,"-",TN_t$Station)
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
  write.table(TN_Print, file = "Output/TN_Feb2025.txt", sep = "|", na = "", row.names = FALSE, quote = FALSE)
  
  ### CHL ----
    
    #Change format of times
    v102<- c(CHL$Activity_Start_Time)
    x102 <- lubridate::parse_date_time(v102,'H:M:S')
    format(x102, format = '%I:%M:%S %p')
    #Change format of times
    v103 <- c(CHL$Analysis_Time)
    x103 <- lubridate::parse_date_time(v103,'H:M:S')
    format(x103, format = '%I:%M:%S %p')
    #Change format of times
    v104<- c(CHL$Preparation_Time)
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
    
    
    CHL_t = CHL%>%
      mutate('Activity_Start_Time'=format(x102, format = '%I:%M:%S %p')) %>%
      mutate('Analysis_Time'=format(x103, format = '%I:%M:%S %p'))%>%
      mutate('Preparation_Time'=format(x104, format = '%I:%M:%S %p'))%>%
      mutate(Value_Qualifier = ifelse(Value_Qualifier %in% c("T", "TI"), "U", 
                                      ifelse(Value_Qualifier == "QTI", "QU", Value_Qualifier)))%>%
      mutate('Org_Result_Value' = ifelse(Org_Result_Value <1, 1, Org_Result_Value))
    
    #change columns
   #CHL_t <- rename(CHL_t, Site = Lake)
    
    #filter out rows that are not stations "1"-"10" and sites that are not Project Coast 
   CHL_t <-CHL_t %>%
      filter(Station %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))%>%
      filter(Site %in% c("CIT-CRY", "CIT-HOM", "CIT-WIT", "HER-CHA", "HER-WEE", "PAS-ANC", "PAS-ARI", "PAS-HUD", "PAS-PIT"))
    
    
    #join with Secchi for start time
   CHL_time = full_join(CHL_t, Secchi_Join, 
                        by = c("County", "Site", "Activity_Start_Date", "Station"))
    
    #Add columns
    
    CHL_WIN <-CHL_time 
    
   CHL_WIN$Project_ID <- "21FLUFSW"
   CHL_WIN$Sampling_Agency_Name <- "University of Florida (Soil and Water Sciences Department)"
   CHL_WIN$Matrix <- "AQUEOUS-Surface Water"
   CHL_WIN$Monitoring_Location_ID = paste0(CHL_t$Site,"-",CHL_t$Station)
   CHL_WIN$Activity_ID = paste0(CHL_WIN$Monitoring_Location_ID,"-",CHL_t$Activity_Start_Date,"S")
   #CHL_WIN$ADAPT_Analyte_ID = paste("FL-INORG-002")
   #CHL_WIN$Org_Analyte_Name = paste("Nitrogen- Total")
   CHL_WIN$Activity_Type = paste("Sample")
   CHL_WIN$Sample_Collection_Type = paste("Direct Grab")
   CHL_WIN$Sample_Collection_Equipment = paste("Water Bottle")
   CHL_WIN$Activity_Depth = paste("0.3")
   CHL_WIN$Activity_Depth_Unit = paste("m")
   CHL_WIN$Total_Depth = paste("")
   CHL_WIN$Total_Depth_Unit = paste("")
   #CHL_WIN$Analysis_Method = paste("LAKEWATCH-TN")
   CHL_WIN$Sample_Fraction = paste("Total")
   CHL_WIN$Preparation_Date_Time = paste0(CHL_t$Preparation_Date," ",CHL_t$Preparation_Time)
   CHL_WIN$Analysis_Date_Time = paste0(CHL_t$Analysis_Date," ",CHL_t$Analysis_Time) 
   CHL_WIN$Activity_Date_Time = paste0(CHL_time$Activity_Start_Date," ",CHL_time$Activity_Start_Time.y)
   CHL_WIN$Activity_Time_Zone.y
   #CHL_WIN$Org_Result_Value = paste0(CHL_time$'Result_Value(ug/L)')
   CHL_WIN$Org_Result_Unit = paste("ug/L")
   CHL_WIN$Org_MDL = paste("1")
   CHL_WIN$Org_PQL = paste("1")
   CHL_WIN$Org_Detection_Unit = paste("ug/L")
   CHL_WIN$Value_Qualifier = paste0(CHL_time$Value_Qualifier)
   CHL_WIN$Result_Comments = paste(" ")
   CHL_WIN$Result_Value_Type_Name = paste("Actual")
   CHL_WIN$Lab_ID = paste("21FLKWAT")
   CHL_WIN$Lab_Accreditation_Authority = paste("None")
   CHL_WIN$Lab_Sample_ID = paste0(CHL_WIN$Activity_ID)
    
    
    #Reorder columns
   CHL_Print <-CHL_WIN[,c("Project_ID","Sampling_Agency_Name","Matrix",
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
    write.table(CHL_Print, file = "Output/CHL_Feb2025.txt", sep = "|", na = "", row.names = FALSE, quote = FALSE)
  
    
    

