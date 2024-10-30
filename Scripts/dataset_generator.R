if (!require("tidyverse")) install.packages("tidyverse", dependencies = TRUE)
library(openxlsx)
library(tidyverse)
library(dplyr)
library(redcapAPI)
library(RMySQL)
library(summarytools)
library(readxl)
library(haven)
library(xlsx)
library(survival)
library(conflicted)


source("Scripts/functions.R")

print("getting REDCap connection")
rcon <- getREDCapConnection(2)
path <- "./Data/"
output_file <- paste0('dataset',format(Sys.time(), '%d_%B_%Y'),'.xlsx')

events <- exportEvents(rcon)

events <- as.list(events$unique_event_name)

instruments <- exportMappings(rcon)

today <- as.POSIXct(Sys.time())


for(event in events){
  forms <- subset(instruments, instruments$unique_event_name==event)
  forms <- as.vector(forms$form)
  
  print(paste0("Event: ", event))
  
  if(event != "index_enrolment_arm_1"){
    forms <- append(forms,"index_screening_and_consent",0)
    
    temp <- getREDCapRecords(event, forms, NULL, TRUE)
  }
  
  if(event != "index_hhc_investig_arm_1" & event!= "household_level_da_arm_1"){
    temp <- getREDCapRecords(event, forms, NULL, TRUE)
    
    if(event!="index_enrolment_arm_1"){
      temp <- temp[-c(5:100)]
    }
    
    temp$record_id <- as.numeric(temp$record_id)
    assign(paste('raw_data', event, sep = '_'), temp)
  }
  
  if(event=="index_hhc_investig_arm_1"){
    temp <- getREDCapRecords(event, forms, NULL, TRUE)
    
    if(event!="index_enrolment_arm_1"){
      temp <- temp[-(5:100)]
    }
    
    temp$record_id <- as.numeric(temp$record_id)
    assign(paste('raw_data', event, sep = '_'), temp)
  }
}
  

write.table(raw_data_index_enrolment_arm_1, 'Data/Baseline.csv', sep = ",", row.names = FALSE)

write.table(raw_data_index_hhc_investig_arm_1, 'Data/HHCI.csv', sep = ",", row.names = FALSE)



#Drop record with record_id 121
raw_data_index_enrolment_arm_1<- subset(raw_data_index_enrolment_arm_1, raw_data_index_enrolment_arm_1$record_id!='121')












