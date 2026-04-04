#' Retrieve PubMed Records for Specified Journals and Years
#'
#' @description
#' This function retrieves PubMed records for specified journals and years, with
#' options to filter for journal articles only, specify batch size for
#' retrieval, and manage data storage. It saves the retrieved records as XML
#' files in a specified directory, with options to overwrite existing files and
#' control the wait time between API requests to avoid hitting PubMed's rate
#' limits. The function handles cases where the number of records exceeds
#' PubMed's limits by structuring queries by month to ensure successful
#' retrieval.
#'
#' The resulting XML files are saved with names that reflect the
#' journal, year, and batch range for easy identification and organization. For
#' example: "Data/all_2010_EMBO-J_0-499.xml" for the first 500 records of EMBO
#' Journal articles published in 2010. or "Data/all_2010-01_EMBO-J.xml" for all
#' EMBO Journal articles published in January 2010 if the total count for 2010
#' exceeded 9999. If more than 500 papers were published in that window, then
#' the file would be named "Data/all_2010-01_EMBO-J_0-499.xml" for the first 500
#' records of that month.
#' So the logic is
#' `all_{year}_{month (if needed)}_{journal}_{start-end (if needed)}.xml`
#'
#' @param journals character vector of journal search terms, e.g. c("EMBO J",
#'   "Nature"), case insensitive
#' @param years numeric or character vector of years, e.g. c(2010, 2011),
#'   c(2009:2015) or c("2010", "2011")
#' @param papers_only logical, if TRUE, only retrieves records that are
#'   classified as "Journal Article" and not "Review" in PubMed. Default is
#'   TRUE.
#' @param batch_size numeric, the number of records to retrieve in each batch
#'   when fetching records from PubMed. Default is 500.
#' @param data_dir character, the directory where the retrieved XML files will
#'   be saved. Default is "Data".
#' @param overwrite logical, if TRUE, overwrites existing XML files in the data
#'   directory. If FALSE, skips downloading records for journal-year
#'   combinations that already have corresponding XML files in the data
#'   directory. Default is FALSE.
#' @param wait_time numeric, the number of seconds to wait between API requests
#'   to avoid hitting PubMed's rate limits. Default is 2 seconds.
#'
#' @returns No return value. This function saves XML files containing PubMed
#'   records for the specified journals and years in the specified data
#'   directory.
#' @export
#'
retrieve_journal_year_records <- function(journals = NULL,
                                          years = NULL,
                                          papers_only = TRUE,
                                          batch_size = 500,
                                          data_dir = "Data",
                                          overwrite = FALSE,
                                          wait_time = 2) {
  # ensure working directory setup
  ensure_wd_setup()
  # if journals is NULL, exit with error
  if (is.null(journals)) {
    stop("Please provide a vector of journal search terms.")
  }
  # if years is NULL, exit with error
  if (is.null(years)) {
    stop("Please provide a vector of years.")
  }

  # loop through journals and loop through the years downloading xml data
  for (i in years) {
    for(j in journals) {

      # format journal name e.g. "embo j" to "\"embo j\"[Journal]"
      j <- journal_name_to_query(j)

      # simple case is our year-journal query yields less than 500 records
      # and we get one xml file
      xml_name <- paste0(data_dir, "/all_", i, "_", extract_journal_name(j), ".xml")

      # if overwrite is FALSE and xml_name already exists,
      # skip to the next iteration of the loop
      if(!overwrite & file.exists(xml_name)) {
        next
      }

      srchTrm <- paste0(j, ' AND ', i, '[pdat]')
      if (papers_only) {
        srchTrm <- paper_type_query(srchTrm)
      }
      pp <- entrez_search(db = "pubmed",
                          term = srchTrm, use_history = TRUE)
      # no entries, then skip
      if(pp$count == 0) {
        next
      }

      # if we have less than batch_size, let's get them!
      if(pp$count < batch_size) {
        Sys.sleep(wait_time)
        pp_rec <- entrez_fetch(db = "pubmed",
                               web_history = pp$web_history,
                               rettype = "xml", parsed = TRUE)
        saveXML(pp_rec, file = xml_name)
      }

      # now deal with the case where there are batch_size to 9999 papers
      if(pp$count >= batch_size & pp$count < 10000) {
        # if there are more than batch_size papers, fetch them in batches
        for (start in seq(0, pp$count - 1, by = batch_size)) {
          end <- min(start + batch_size - 1, pp$count)
          xml_name <- paste0(data_dir, "/all_",
                             i, "_", extract_journal_name(j), "_",
                             start, "-", end, ".xml")
          if(!overwrite & file.exists(xml_name)) {
            next
          }
          Sys.sleep(wait_time)
          pp_rec <- entrez_fetch(db = "pubmed",
                                 web_history = pp$web_history,
                                 rettype = "xml", parsed = TRUE,
                                 retstart = start, retmax = batch_size)
          saveXML(pp_rec, file = xml_name)
        }
      }

      # if our query yields more than 9999 papers, we need to structure the query
      # by month to reduce the number of papers per query
      if(pp$count > 9999) {
        for (m in 1:12) {
          # Last day of month as 2-digit day (e.g., "28", "30", "31") for PubMed date range.
          day <- format(seq(as.Date(sprintf("%d-%02d-01", i, m)),
                            by = "1 month", length.out = 2)[2] - 1, "%d")
          xml_name <- paste0(data_dir, "/all_",
                             i, "-", sprintf("%02d", m), "_",
                             extract_journal_name(j), ".xml")
          # if xml_base_name matches the start of any item in xml_files, then skip the
          if(!overwrite & file.exists(xml_name)) {
            next
          }
          srchTrm <- paste0(j, ' AND ',
                            i, "/", sprintf("%02d", m), "/01:",
                            i, "/", sprintf("%02d", m), "/", day,
                            "[Date - Publication]")
          if (papers_only) {
            srchTrm <- paper_type_query(srchTrm)
          }
          pp <- entrez_search(db = "pubmed",
                              term = srchTrm, use_history = TRUE)
          if(pp$count == 0) {
            next
          } else if (pp$count < batch_size) {
            Sys.sleep(wait_time)
            pp_rec <- entrez_fetch(db = "pubmed",
                                   web_history = pp$web_history,
                                   rettype = "xml", parsed = TRUE)
            saveXML(pp_rec, file = xml_name)
          } else {
            # if there are more than batchsize papers, we need to fetch them in batches
            for (start in seq(0, pp$count - 1, by = batch_size)) {
              end <- min(start + batch_size - 1, pp$count)
              xml_name <- paste0(data_dir, "/all_",
                                 i, "-", sprintf("%02d", m), "_",
                                 extract_journal_name(j), "_",
                                 start, "-", end, ".xml")
              if(!overwrite & file.exists(xml_name)) {
                next
              }
              # introduce a delay of 3 seconds between each batch to avoid hitting the rate limit
              Sys.sleep(wait_time)
              pp_rec <- entrez_fetch(db = "pubmed",
                                     web_history = pp$web_history,
                                     rettype = "xml", parsed = TRUE,
                                     retstart = start, retmax = batch_size)
              saveXML(pp_rec, file = xml_name)
            }
          }
        }
      }
    }
  }
}
