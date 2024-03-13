#install.packages('redcapAPI')
library(redcapAPI)
library(config)
library(openxlsx)
library(RMySQL)

#rcon <- NULL

source("Scripts/missingSummary.R")

getReportData <- function(rcon, report_id){
  rcon <- getREDCapConnection(1)
  data <- exportReports(
    rcon,
    report_id,
    factors = TRUE,
    labels = FALSE,
    dates = TRUE,
    
    checkboxLabels = FALSE,
    colClasses = NA
  )
  
  return(data)
  
}

getREDCapConnection <- function(study){
  dw <- config::get(file = "Config/config.yml")
  if(study=='1'){
    
    print(dw$redcap_api_url)
    rcon <<- redcapConnection(
      url = dw$redcap_api_url,
      token = dw$token_1)
  }else if(study=='2'){
    print(dw$redcap_api_url)
    rcon <<- redcapConnection(
      url = dw$redcap_api_url,
      token = dw$token_2)
  }else{
    print(dw$redcap_api_url)
    rcon <<- redcapConnection(
      url = dw$redcap_api_url,
      token = dw$token)
  }
  
  
  return(rcon)
}

getMissingSummary <- function(){
  dw <- config::get(file = "Config/config.yml")
  data <- collaborator::report_miss(redcap_project_uri = dw$redcap_api_url, 
                                    redcap_project_token = dw$token, use_ssl = FALSE)
}

getREDCapRecords <- function(events, forms, selected_fields, labels){
  records <- exportRecordsTyped(
    rcon,
    factors = labels,
    fields = selected_fields,
    forms = forms,
    records = NULL,
    events = events,
    labels = labels,
    dates = TRUE,
    survey = TRUE,
    dag = TRUE,
    checkboxLabels = TRUE,
    colClasses = NA
  )
  return(records)
}

generateSummaryReport <- function(df){
  df_summary <- dfSummary(baseline_data, 
                          plain.ascii  = FALSE, 
                          style        = "grid", 
                          graph.magnif = 0.75, 
                          valid.col    = FALSE,
                          tmp.img.dir  = "Data/")
  
  return(df_summary)
}

getMetadata <- function(forms, fields){
  
  metadata <- exportMetaData(
    rcon,
    fields = NULL,
    forms = forms,
    error_handling = getOption("redcap_error_handling"),
    drop_utf8 = FALSE
  )
  
  
  
  
  
  return(metadata)
}

writeXlsx <- function(df, worksheetName, outputFile, isCreateFile, isRowName){
  options(java.parameters = "-Xmx4096m")
  
  #df <- trial_recruitment_summary
  #outputFile <- "Data.xlsx"
  #isCreateFile <- TRUE
  #isRowName <- FALSE
  wb <<- NULL
  print(getwd())
  if(isCreateFile){
    wb <<- openxlsx::createWorkbook()
  }else{
    wb <<- openxlsx::loadWorkbook(outputFile)
  }
  
  #if(worksheetName %in% names(wb)){
  #  removeWorksheet(wb, worksheetName)
  #  addWorksheet(wb, worksheetName)
  #}else{
  #  addWorksheet(wb,worksheetName)
  #}/
  openxlsx::addWorksheet(wb, worksheetName)
  openxlsx::writeDataTable(wb, worksheetName, df, tableStyle = "TableStyleLight9", rowNames = isRowName)
  openxlsx::saveWorkbook(wb, outputFile, overwrite = TRUE)
}

save_data <- function(df, name) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, name)
  openxlsx::writeDataTable(wb, name, df, tableStyle = "TableStyleLight9")
  openxlsx::saveWorkbook(wb, paste0(name, ".xlsx"), overwrite = TRUE)
}

getREDCapMappings <- function(){
  mappings <- exportMappings(rcon)
  return(mappings)
}

merge_by_week_df <- function(df_1, df_2){
  return(base::merge(df_1, df_2, by = "Week", all.x = TRUE))
}

merge_df <- function(df_1, df_2, by_col){
  return(base::merge(df_1,df_2, by = by_col, all.x = TRUE, sort=FALSE))
}

set_weeks <- function(df){
  df$site_start_date <- as.Date("1900-01-01")
  
  df$project_start_date <- as.Date("2021-03-28")
  
 # df$site_start_date[df$site_name=="Grey Gateway"] <- as.Date("2021-03-28")
 # df$site_start_date[df$site_name=="Empilweni Gompo CHC"] <- as.Date("2021-03-28")
 # df$site_start_date[df$site_name=="Nontyatyambo CHC"] <- as.Date("2021-05-10")
 # df$site_start_date[df$site_name=="Duncan Village CHC"] <- as.Date("2021-05-23")
 # df$site_start_date[df$site_name=="Ndevana"] <- as.Date("2023-07-03")
  
  df$week <- ceiling(difftime(df$today_s_date, df$site_start_date, units = "weeks"))
  df$week <- as.integer(df$week)
  
  df$global_week <- ceiling(difftime(df$today_s_date, df$project_start_date, units = "weeks"))
  df$global_week <- as.integer(df$global_week)
  
  return(df)
}

set_months <- function(df){
  df$site_start_date <- as.Date("1900-01-01")
  
  df$project_start_date <- as.Date("2021-03-28")
  
 # df$site_start_date[df$site_name=="Grey Gateway"] <- as.Date("2021-03-28")
 #  df$site_start_date[df$site_name=="Empilweni Gompo CHC"] <- as.Date("2021-03-28")
 #  df$site_start_date[df$site_name=="Nontyatyambo CHC"] <- as.Date("2021-05-10")
 #  df$site_start_date[df$site_name=="Duncan Village CHC"] <- as.Date("2021-05-23")
 # df$site_start_date[df$site_name=="Ndevana"] <- as.Date("2022-07-03")
  
  df$month <- ceiling(difftime(df$today_s_date, df$site_start_date, units = "days")/(365.25/12))
  df$month <- as.integer(df$month)
  
  df$global_month <- ceiling(difftime(df$today_s_date, df$project_start_date, units = "days")/(365.25/12))
  df$global_month <- as.integer(df$global_month)
  
  return(df)
}

get_screening_figures <- function(df, study, period){
  
  screened_freq <- calculate_frequencies(df, study, period)
  
  screened_cumfreq <- calculate_cumulative_frequncies(df, study, period)
  
  tmp_period <- NULL
  
  if(period=="weekly"){
    tmp_period <- "Week"
    if(study=="RCT"){
      #Renaming column headers for RCT data frame
      names(screened_freq) <- c('Week', 'Grey Gateway Screened', 'Nontyatyambo CHC Screened', 'Duncan Village CHC Screened', 'Ndevana Screened')
      names(screened_cumfreq) <- c('Week', 'Grey Gateway Screened - Cumulative', 'Nontyatyambo CHC Screened - Cumulative', 'Duncan Village CHC Screened - Cumulative', 'Ndevana Screened - Cumulative')
    }else if(study=="micro"){
      #Renaming column headers for microbiome data frame
      names(screened_freq) <- c('Week', 'Empilweni Gompo CHC Screened')
      names(screened_cumfreq) <- c('Week', 'Empilweni Gompo CHC Screened - Cumulative')
    }
  }else if(period=="monthly"){
    tmp_period <- "Month"
    if(study=="RCT"){
      names(screened_freq) <- c('Month', 'Grey Gateway Screened', 'Nontyatyambo CHC Screened', 'Duncan Village CHC Screened', 'Ndevana Screened')
      names(screened_cumfreq) <- c('Month', 'Grey Gateway Screened - Cumulative', 'Nontyatyambo CHC Screened - Cumulative', 'Duncan Village CHC Screened - Cumulative', 'Ndevana Screened - Cumulative')
    }else if(study=="micro"){
      names(screened_freq) <- c('Month', 'Empilweni Gompo CHC Screened')
      names(screened_cumfreq) <- c('Month', 'Empilweni Gompo CHC Screened - Cumulative')
    }
  }
  trial_recruitment_summary <- merge_df(screened_freq, screened_cumfreq, tmp_period)
  return(trial_recruitment_summary)
}

get_eligibility_figures2 <- function(df, study, period){
  #Weekly
  
  trial_not_eligible_freq <- calculate_frequencies(not_eligible, "RCT", "weekly")
  names(trial_not_eligible_freq) <- c('Week', 'Grey Gateway Not Eligible', 'Nontyatyambo CHC Not Eligible', 'Duncan Village CHC Not Eligible', 'Ndevana Not Eligible')
  
  micro_not_eligible_freq <- calculate_frequencies(not_eligible, "micro", "weekly")
  names(micro_not_eligible_freq) <- c('Week', 'Empilweni Gompo CHC Not Eligible')
  
  trial_not_eligible_cumfreq <- calculate_cumulative_frequncies(not_eligible, "RCT", "weekly")
  names(trial_not_eligible_cumfreq) <- c('Week', 'Grey Gateway Not Eligible - Cumulative', 'Nontyatyambo CHC Not Eligible - Cumulative', 'Duncan Village CHC Not Eligible - Cumulative', 'Ndevana Not Eligible - Cumulative')
  
  micro_not_eligible_cumfreq <- calculate_cumulative_frequncies(not_eligible, "micro", "weekly")
  names(micro_not_eligible_cumfreq) <- c('Week', 'Empilweni Gompo CHC Not Eligible - Cumulative')
  
  #Monthly 
  
  trial_not_eligible_m_freq <- calculate_frequencies(not_eligible, "RCT", "monthly")
  names(trial_not_eligible_m_freq) <- c('Month', 'Grey Gateway Not Eligible', 'Nontyatyambo CHC Not Eligible', 'Duncan Village CHC Not Eligible', 'Ndevana Not Eligible')
  
  micro_not_eligible_m_freq <- calculate_frequencies(not_eligible, "micro", "monthly")
  names(micro_not_eligible_m_freq) <- c('Month', 'Empilweni Gompo CHC Not Eligible')
  
  trial_not_eligible_m_cumfreq <- calculate_cumulative_frequncies(not_eligible, "RCT", "monthly")
  names(trial_not_eligible_m_cumfreq) <- c('Month', 'Grey Gateway Not Eligible - Cumulative', 'Nontyatyambo CHC Not Eligible - Cumulative', 'Duncan Village CHC Not Eligible - Cumulative', 'Ndevana Not Eligible - Cumulative')
  
  micro_not_eligible_m_cumfreq <- calculate_cumulative_frequncies(not_eligible, "micro", "monthly")
  names(micro_not_eligible_m_cumfreq) <- c('Month', 'Empilweni Gompo CHC Not Eligible - Cumulative')
  
}

get_eligibility_figures <- function(df, study, period){
  
  eligible_freq <- calculate_frequencies(df, study, period)
  
  eligible_cumfreq <- calculate_cumulative_frequncies(df, study, period)
  
  tmp_period <- NULL
  
  if(period=="weekly"){
    tmp_period <- "Week"
    if(study=="RCT"){
      names(eligible_freq) <- c('Week', 'Grey Gateway Not Eligible', 'Nontyatyambo CHC Not Eligible', 'Duncan Village CHC Not Eligible', 'Ndevana Not Eligible')
      names(eligible_cumfreq) <- c('Week', 'Grey Gateway Not Eligible - Cumulative', 'Nontyatyambo CHC Not Eligible - Cumulative', 'Duncan Village CHC Not Eligible - Cumulative', 'Ndevana Not Eligible - Cumulative')
    }else if(study=="micro"){
      names(eligible_freq) <- c('Week', 'Empilweni Gompo CHC Not Eligible')
      names(eligible_cumfreq) <- c('Week', 'Empilweni Gompo CHC Not Eligible - Cumulative')
    }
  }else if(period=="monthly"){
    tmp_period <- "Month"
    if(study=="RCT"){
      names(eligible_freq) <- c('Month', 'Grey Gateway Not Eligible', 'Nontyatyambo CHC Not Eligible', 'Duncan Village CHC Not Eligible', 'Ndevana Not Eligible')
      names(eligible_cumfreq) <- c('Month', 'Grey Gateway Not Eligible - Cumulative', 'Nontyatyambo CHC Not Eligible - Cumulative', 'Duncan Village CHC Not Eligible - Cumulative', 'Ndevana Not Eligible - Cumulative')
    }else if(study=="micro"){
      names(eligible_freq) <- c('Month', 'Empilweni Gompo CHC Not Eligible')
      names(eligible_cumfreq) <- c('Month', 'Empilweni Gompo CHC Not Eligible - Cumulative')
    }
  }
  
  eligibility_summary <- merge_df(eligible_freq, eligible_cumfreq, tmp_period)
  return(eligibility_summary)
}

get_enrolment_figures2 <- function(){
  #Weekly
  tmp_period="weekly"
  trial_enrolled_freq <- calculate_frequencies(enrolled, "RCT", tmp_period)
  names(trial_enrolled_freq) <- c('Week', 'Grey Gateway Enrolled', 'Nontyatyambo CHC Enrolled', 'Duncan Village CHC Enrolled', 'Ndevana Enrolled')
  
  micro_enrolled_freq <- calculate_frequencies(enrolled, "micro", tmp_period)
  names(micro_enrolled_freq) <- c('Week', 'Empilweni Gompo CHC Enrolled')
  
  trial_enrolled_cumfreq <- calculate_cumulative_frequncies(enrolled, "RCT", tmp_period)
  names(trial_enrolled_cumfreq) <- c('Week', 'Grey Gateway Enrolled - Cumulative', 'Nontyatyambo CHC Enrolled - Cumulative', 'Duncan Village CHC Enrolled - Cumulative', 'Ndevana Enrolled - Cumulative')
  
  micro_enrolled_cumfreq <- calculate_cumulative_frequncies(enrolled, "micro", tmp_period)
  names(micro_enrolled_cumfreq) <- c('Week', 'Empilweni Gompo CHC Enrolled - Cumulative')
  
  #Monthly
  print("tem")
  trial_enrolled_m_freq <- calculate_frequencies(enrolled, "RCT", "monthly")
  names(trial_enrolled_m_freq) <- c('Month', 'Grey Gateway Enrolled', 'Nontyatyambo CHC Enrolled', 'Duncan Village CHC Enrolled', 'Ndevana Enrolled')
  print("mem")
  micro_enrolled_m_freq <- calculate_frequencies(enrolled, "micro", "monthly")
  names(micro_enrolled_m_freq) <- c('Month', 'Empilweni Gompo CHC Enrolled')
  
  trial_enrolled_m_cumfreq <- calculate_cumulative_frequncies(enrolled, "RCT", "monthly")
  names(trial_enrolled_m_cumfreq) <- c('Month', 'Grey Gateway Enrolled - Cumulative', 'Nontyatyambo CHC Enrolled - Cumulative', 'Duncan Village CHC Enrolled - Cumulative', 'Ndevana Enrolled - Cumulative')
  
  micro_enrolled_m_cumfreq <- calculate_cumulative_frequncies(enrolled, "micro", "monthly")
  names(micro_enrolled_m_cumfreq) <- c('Month', 'Empilweni Gompo CHC Enrolled - Cumulative')
}

get_enrollment_figures <- function(df, study, period){
  
  enrollment_freq <- calculate_frequencies(df, study, period)
  
  enrollment_cumfreq <- calculate_cumulative_frequncies(df, study, period)
  
  tmp_period <- NULL
  
  if(period=="weekly"){
    tmp_period <- "Week"
    if(study=="RCT"){
      names(enrollment_freq) <- c('Week', 'Grey Gateway Enrolled', 'Nontyatyambo CHC Enrolled', 'Duncan Village CHC Enrolled', 'Ndevana Enrolled')
      names(enrollment_cumfreq) <- c('Week', 'Grey Gateway Enrolled - Cumulative', 'Nontyatyambo CHC Enrolled - Cumulative', 'Duncan Village CHC Enrolled - Cumulative', 'Ndevana Enrolled - Cumulative')
    }else if(study=="micro"){
      names(enrollment_freq) <- c('Week', 'Empilweni Gompo CHC Enrolled')
      names(enrollment_cumfreq) <- c('Week', 'Empilweni Gompo CHC Enrolled - Cumulative')
    }
  }else if(period=="monthly"){
    tmp_period <- "Month"
    if(study=="RCT"){
      names(enrollment_freq) <- c('Month', 'Grey Gateway Enrolled', 'Nontyatyambo CHC Enrolled', 'Duncan Village CHC Enrolled', 'Ndevana Enrolled')
      names(enrollment_cumfreq) <- c('Month', 'Grey Gateway Enrolled - Cumulative', 'Nontyatyambo CHC Enrolled - Cumulative', 'Duncan Village CHC Enrolled - Cumulative', 'Ndevana Enrolled - Cumulative')
    }else if(study=="micro"){
      names(enrollment_freq) <- c('Month', 'Empilweni Gompo CHC Enrolled')
      names(enrollment_cumfreq) <- c('Month', 'Empilweni Gompo CHC Enrolled - Cumulative')
    }
  }
  enrollment_summary <- merge_df(enrollment_freq, enrollment_cumfreq, tmp_period)
  return(enrollment_summary)
}

find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
