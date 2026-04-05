#' Get data from PubMed XML
#'
#' This function reads a PubMed XML file and extracts relevant information into
#' a data frame. It allows for optional inclusion of abstracts, publication
#' dates, and other metadata such as MeSH headings, chemical names, grant
#' information, and clinical trial identifiers.
#'
#' @param theFile The path to the PubMed XML file to be processed.
#' @param include_all If TRUE, includes all optional fields (abstracts, dates,
#'   and other metadata).
#' @param include_abstract If TRUE, includes the abstract text in the output
#'   data frame.
#' @param include_dates If TRUE, includes publication dates (received, accepted,
#'   and pubmed) in the output data frame.
#' @param include_other If TRUE, includes additional metadata such as MeSH
#'   headings, chemical names, grant information, and clinical trial identifiers
#'   in the output data frame.
#'
#' @returns A data frame containing the extracted information from the PubMed
#'   XML file, with columns for PMID, DOI, authors, year, article title,
#'   journal, volume, issue, pages, publication type, and optionally abstracts,
#'   dates, and other metadata.
#' @export
#'
get_data_from_pubmed_xml <- function(theFile,
                                     include_all = FALSE,
                                     include_abstract = FALSE,
                                     include_dates = FALSE,
                                     include_other = FALSE) {
  if (include_all) {
    include_abstract <- TRUE
    include_dates <- TRUE
    include_other <- TRUE
  }

  # this version uses {XML} and is much faster
  # than the previous version using {xml2}

  doc <- xmlParse(theFile)
  records <- getNodeSet(doc, "//PubmedArticle")

  if (length(records) == 0) {
    free(doc)
    return(NULL)
  }

  # Helper: get trimmed text of first matching node, or ""
  node_text <- function(node, xpath) {
    val <- xpathSApply(node, xpath, xmlValue)
    if (!is.character(val) || length(val) == 0) return("")
    trimws(val[[1]])
  }

  collapse_nodes <- function(nodes, xpath, unique_values = FALSE) {
    vapply(nodes, function(node) {
      text <- xpathSApply(node, xpath, xmlValue)
      if (!is.character(text) || length(text) == 0) return(NA_character_)
      text <- trimws(text)
      text <- text[nzchar(text)]
      if (unique_values) text <- unique(text)
      if (length(text) == 0) return(NA_character_)
      paste(text, collapse = "|")
    }, character(1))
  }

  extract_authors <- function(nodes) {
    vapply(nodes, function(node) {
      author_nodes <- getNodeSet(node, ".//Author")

      authors <- vapply(author_nodes, function(author_node) {
        collective_name <- node_text(author_node, "./CollectiveName")
        if (nzchar(collective_name)) return(collective_name)

        last_name <- node_text(author_node, "./LastName")
        initials  <- node_text(author_node, "./Initials")
        fore_name <- node_text(author_node, "./ForeName")

        parts <- c(last_name, initials)
        parts <- parts[nzchar(parts)]

        if (length(parts) == 0 && nzchar(fore_name)) return(fore_name)
        if (length(parts) == 0) return(NA_character_)
        paste(parts, collapse = " ")
      }, character(1))

      authors <- authors[!is.na(authors) & nzchar(authors)]
      if (length(authors) == 0) return(NA_character_)
      paste(authors, collapse = "|")
    }, character(1))
  }

  extract_year <- function(nodes) {
    vapply(nodes, function(node) {
      year_value <- node_text(node, ".//PubDate/Year")

      if (!nzchar(year_value)) {
        medline_value <- node_text(node, ".//PubDate/MedlineDate")
        year_value <- sub("^([0-9]{4}).*$", "\\1", medline_value)
        if (!grepl("^[0-9]{4}$", year_value)) year_value <- NA_character_
      }

      year_value
    }, character(1))
  }

  parse_pubmed_date <- function(nodes, status) {
    month_lookup <- setNames(sprintf("%02d", seq_along(month.abb)), tolower(month.abb))

    values <- vapply(nodes, function(node) {
      date_nodes <- getNodeSet(node, paste0(".//PubMedPubDate[@PubStatus='", status, "']"))
      if (length(date_nodes) == 0) return(NA_character_)
      date_node <- date_nodes[[1]]

      year_value  <- node_text(date_node, "./Year")
      month_value <- node_text(date_node, "./Month")
      day_value   <- node_text(date_node, "./Day")

      if (!nzchar(year_value) || !nzchar(month_value) || !nzchar(day_value)) {
        return(NA_character_)
      }

      if (grepl("^[0-9]+$", month_value)) {
        month_value <- sprintf("%02d", as.integer(month_value))
      } else {
        month_key   <- tolower(substr(month_value, 1, 3))
        month_value <- month_lookup[[month_key]]
      }

      if (is.null(month_value) || is.na(month_value)) return(NA_character_)
      if (!grepl("^[0-9]+$", day_value)) return(NA_character_)

      sprintf("%s-%s-%02d", year_value, month_value, as.integer(day_value))
    }, character(1))

    as.Date(unname(values))
  }

  theDF <- data.frame(
    pmid         = collapse_nodes(records, ".//MedlineCitation/PMID"),
    doi          = collapse_nodes(records, ".//ELocationID[@EIdType='doi']"),
    authors      = extract_authors(records),
    year         = extract_year(records),
    articletitle = collapse_nodes(records, ".//ArticleTitle"),
    journal      = collapse_nodes(records, ".//ISOAbbreviation"),
    volume       = collapse_nodes(records, ".//JournalIssue/Volume"),
    issue        = collapse_nodes(records, ".//JournalIssue/Issue"),
    pages        = collapse_nodes(records, ".//MedlinePgn"),
    ptype        = collapse_nodes(records, ".//PublicationType"),
    stringsAsFactors = FALSE
  )

  if (isTRUE(include_abstract)) {
    theDF$abstract <- collapse_nodes(records, ".//Abstract/AbstractText")
  }

  if (isTRUE(include_dates)) {
    theDF$recdate <- parse_pubmed_date(records, "received")
    theDF$accdate <- parse_pubmed_date(records, "accepted")
    theDF$pubdate <- parse_pubmed_date(records, "pubmed")
  }

  if (isTRUE(include_other)) {
    theDF$meshHeadings <- collapse_nodes(records, ".//DescriptorName")
    theDF$chemNames    <- collapse_nodes(records, ".//NameOfSubstance")
    theDF$grantAgency  <- collapse_nodes(records, ".//Grant/Agency", unique_values = TRUE)
    theDF$grantNumber  <- collapse_nodes(records, ".//Grant/GrantID")
    theDF$grantCountry <- collapse_nodes(records, ".//Grant/Country", unique_values = TRUE)
    theDF$nctID        <- collapse_nodes(
      records,
      ".//DataBank[DataBankName='ClinicalTrials.gov']/AccessionNumberList/AccessionNumber"
    )
  }

  free(doc)
  return(theDF)
}
