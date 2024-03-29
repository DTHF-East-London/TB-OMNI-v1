#### Intended for use with the `redcapAPI` package
#' @name missingSummary
#' @aliases missingSummary.redcapApiConnection
#' @aliases missingSummary.redcapDbConneciton
#' @aliases missingSummary_offline
#' @export missingSummary
#' @export missingSummary.redcapApiConnection
#' @export missingSummary.redcapDbConnection
#' @export missingSummary_offline
#' 
#' @title Report of Missing Values
#' @description Returns a data frame of subject events with missing values. 
#' 
#' @param rcon A recapConnection object.
#' @param proj A redcapProjectInfo object.
#' @param batch.size Batch size parameter for \code{exportRecords}
#' @param records a filename pointing to the raw records download from REDCap
#' @param meta_data a filename pointing to the data dictionary download from REDCap
#' @param excludeMissingForms If all of the fields in a form are missing, would 
#'   you like to assume that they are purposefully missing?  For instance, if
#'   a patient did not experience an adverse event, the adverse event form would
#'   contain no data and you would not want it in this report.
#' @param ... Additional arguments to pass to other methods.  Currently ignored.
#'   
#' @details The intention of this function is to generate a list of subject
#'   events that are missing and could potentially be values that should have
#'   been entered.
#'   
#'   The branching logic from the data dictionary is parsed and translated into
#'   and R expression.  When a field with branching logic passes the logical
#'   statement, it is evaluated with \code{is.na}, otherwise, it is set to 
#'   \code{FALSE} (non-missing, because there was never an opportunity to 
#'   provide a value).
#'   
#'   Optionally, forms that are entirely missing can be determined to be 
#'   non-missing.  This is applicable when, for instance, a patient did not 
#'   have an adverse event.  In this case, a form dedicated to adverse events 
#'   would contain meaningless missing values and could be excluded from the 
#'   report.
#'   
#' @author Benjamin Nutter
#' 

missingSummary <- function(rcon, excludeMissingForms=TRUE, ...){
  UseMethod("missingSummary")
}

#' @rdname missingSummary

missingSummary.redcapDbConnection <- function(rcon, 
                                              excludeMissingForms=TRUE, ...){
  message("Please accept my apologies.  The missingSummary method for redcapDbConnection objects\n",
          "has not yet been written.  Please consider using the API.")
}

#' @rdname missingSummary

missingSummary.redcapApiConnection <- function(rcon, 
                                               excludeMissingForms = TRUE, ...,
                                               proj=NULL, batch.size=-1){
  
  records <- exportRecords(rcon, factors=FALSE, labels=TRUE,
                           dates=FALSE, survey=FALSE, dag=TRUE,
                           batch.size=batch.size)
  #   records.orig <- records
  
  meta_data <- exportMetaData(rcon)
  meta_data <- meta_data[meta_data$field_type != "descriptive", ]
  
  meta_data <- metadata[metadata$field_name!='sche_32week_header', ]
  
  meta_data <- metadata[metadata$field_name!='sche_32week_intro', ]
  
  meta_data <- metadata[metadata$field_name!='pn_pmtct_given', ]
  
  meta_data <- metadata[metadata$field_name!='pn_birth_pcr', ]
  
  meta_data <- metadata[metadata$field_name!='sched_32week_header_v2', ]
  
  meta_data <- metadata[metadata$field_name!='sched_32week_intro_v2', ]
  
  form_names <- unique(meta_data$form_name)
  form_complete_names <- paste0(form_names, "_complete")
  
  logic <- parseBranchingLogic(meta_data$branching_logic)
  names(logic) <- meta_data$field_name
  
  start_value <- 2 + sum(c("redcap_event_name", "redcap_data_access_group") %in% names(records)) 
  for (i in tail(seq_along(records), -(start_value - 1))){
    
    l <- logic[[names(records)[i]]]
    
    tmp_form <- meta_data$form_name[meta_data$field_name == 
                                      sub("___[[:print:]]", "", names(records)[i])]
    
    tmp_form <- paste0(tmp_form, "_complete")
    
    {
      if (tmp_form == "_complete") records[[i]] <- FALSE
      if (!tmp_form %in% names(records))
        records[[i]] <- is.na(records[[i]])
      else if (!is.expression(l))
        records[[i]] <- ifelse(is.na(records[[tmp_form]]), 
                               FALSE, is.na(records[[i]]))
      else
        records[[i]] <- ifelse(is.na(records[[tmp_form]]),
                               FALSE,
                               ifelse(with(records, eval(l)), is.na(records[[i]]),
                                      FALSE))  
    }
  }
  
  if (excludeMissingForms){
    for (i in seq_len(nrow(records))){
      completeFormMissing <- lapply(form_names, 
                                    function(f){
                                      flds <- meta_data$field_name[meta_data$form_name %in% f]
                                      flds <- flds[!flds %in% meta_data$field_name[1]]
                                      flds <- flds[!flds %in% meta_data$field_name[meta_data$field_type == "checkbox"]]
                                      if (all(unlist(records[i, flds, drop=FALSE]) | sapply(logic[flds], is.expression))){
                                        return(flds)
                                      }
                                      else return(NULL)
                                    })
      completeFormMissing <- unlist(completeFormMissing)
      if (!is.null(completeFormMissing)) records[i, completeFormMissing] <- FALSE    
    }
  }
  
  n_missing <- apply(records[-(1:(start_value-1))], 1, sum)
  
  missing <- apply(records[-(1:(start_value-1))], 1, 
                   function(r) paste(names(r)[r], collapse=", "))
  
  MissingReport <- 
    cbind(records[, 1:(start_value - 1)],
          n_missing, 
          missing)
  
  return(MissingReport)
}

#' @rdname missingSummary
#' 

missingSummary_offline <- function(records, meta_data,
                                   excludeMissingForms = TRUE){
  
  records <- read.csv(records,
                      stringsAsFactors=FALSE, 
                      na.string="")
  #   records.orig <- records
  
  meta_data <- read.csv(meta_data,
                        col.names=c('field_name', 'form_name', 'section_header',
                                    'field_type', 'field_label', 'select_choices_or_calculations',
                                    'field_note', 'text_validation_type_or_show_slider_number',
                                    'text_validation_min', 'text_validation_max', 'identifier',
                                    'branching_logic', 'required_field', 'custom_alignment',
                                    'question_number', 'matrix_group_name', 'matrix_ranking',
                                    'field_annotation'),
                        stringsAsFactors=FALSE)
  meta_data <- meta_data[meta_data$field_type != "descriptive", ]
  
  form_names <- unique(meta_data$form_name)
  form_complete_names <- paste0(form_names, "_complete")
  
  logic <- parseBranchingLogic(meta_data$branching_logic)
  names(logic) <- meta_data$field_name
  
  start_value <- 2 + sum(c("redcap_event_name", "redcap_data_access_group") %in% names(records)) 
  for (i in tail(seq_along(records), -(start_value - 1))){
    l <- logic[[names(records)[i]]]
    
    tmp_form <- meta_data$form_name[meta_data$field_name == 
                                      sub("___[[:print:]]", "", names(records)[i])]
    tmp_form <- paste0(tmp_form, "_complete")
    
    {if (tmp_form == "_complete") records[[i]] <- FALSE
      else if (!is.expression(l))
        records[[i]] <- ifelse(is.na(records[[tmp_form]]), 
                               FALSE, is.na(records[[i]]))
      else
        records[[i]] <- ifelse(is.na(records[[tmp_form]]),
                               FALSE,
                               ifelse(with(records, eval(l)), is.na(records[[i]]),
                                      FALSE))  
    }
  }
  
  if (excludeMissingForms){
    for (i in seq_len(nrow(records))){
      completeFormMissing <- lapply(form_names, 
                                    function(f){
                                      flds <- meta_data$field_name[meta_data$form_name %in% f]
                                      flds <- flds[!flds %in% meta_data$field_name[1]]
                                      flds <- flds[!flds %in% meta_data$field_name[meta_data$field_type == "checkbox"]]
                                      if (all(unlist(records[i, flds, drop=FALSE]) | sapply(logic[flds], is.expression))){
                                        return(flds)
                                      }
                                      else return(NULL)
                                    })
      completeFormMissing <- unlist(completeFormMissing)
      if (!is.null(completeFormMissing)) records[i, completeFormMissing] <- FALSE    
    }
  }
  
  n_missing <- apply(records[-(1:(start_value-1))], 1, sum)
  
  missing <- apply(records[-(1:(start_value-1))], 1, 
                   function(r) paste(names(r)[r], collapse=", "))
  
  MissingReport <- 
    cbind(records[, 1:(start_value - 1)],
          n_missing, 
          missing)
  
  return(MissingReport)
}