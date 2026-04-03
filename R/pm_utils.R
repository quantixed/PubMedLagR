#' Extract journal name from search query string
#'
#' @param jname journal name string as part of search query, eg `'"embo j[ta]"'`
#'
#' @returns cleaned string to use as filename
#' @noRd
#'
#'
extract_journal_name <- function(jname) {
  jname <- gsub('\\"', "", jname)
  jname <- gsub("\\[ta\\]", "", jname)
  jname <- gsub("\\[Journal\\]", "", jname)
  jname <- gsub(" ", "-", jname)

  return(jname)
}


#' Ensure working directory setup
#'
#' Checks if the necessary directories ("Data", "Output", "Output/Data",
#' "Output/Plots", "Script") exist in the current working directory. If any of
#' these directories are missing, it creates them to ensure that the project
#' structure is properly set up for data storage and output generation.
#'
#' @returns No return value
#' @noRd
#'
ensure_wd_setup <- function() {
  if (!dir.exists("Data")) dir.create("Data")
  if (!dir.exists("Output")) dir.create("Output")
  if (!dir.exists("Output/Data")) dir.create("Output/Data")
  if (!dir.exists("Output/Plots")) dir.create("Output/Plots")
  if (!dir.exists("Script")) dir.create("Script")
}


#' Journal name to query string
#'
#' @param jrnl valid journal name to modify
#'
#' @returns modified journal name string to be used in PubMed query
#' @noRd

journal_name_to_query <- function(jrnl) {
  query <- paste0("\"", jrnl, "\"[Journal]")

  return(query)
}

#' Add restriction of paper types to query string
#'
#' The aim is to restrict the records retrieved to be papers only. Not
#' reviews or front matter.
#'
#' @param query string for PubMed query
#'
#' @returns modified query string
#' @noRd
paper_type_query <- function(query) {
  query <- paste0(query, " AND (",
         "\"Journal Article\"[pt] NOT \"Review\"[pt])")
  return(query)
}
