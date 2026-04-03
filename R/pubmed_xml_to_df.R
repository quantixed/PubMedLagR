#' PubMed XML to Data Frame
#'
#' Wrapper function to extract data from all XML files in `data_dir` and combine
#' them into a single data frame. Optionally, it can clean the data by removing
#' duplicates, filtering out unwanted publication types, and ensuring that only
#' journal articles are included.
#'
#' @param data_dir string, the directory containing the PubMed XML files to be
#'   processed. Default is "Data".
#' @param clean logical, whether to clean the data by removing duplicates and
#'   filtering out unwanted publication types. Default is TRUE.
#'
#' @returns data frame of PubMed records
#' @export

pubmed_xml_to_df <- function(data_dir = "Data",
                             clean = TRUE) {

  xml_files <- list.files(data_dir, pattern = "\\.xml$", full.names = TRUE)

  # Use forked parallelism on macOS/Linux; fall back to serial when unavailable.
  n_workers <- max(1L, detectCores(logical = FALSE) - 1L)

  if (.Platform$OS.type == "unix" && n_workers > 1L) {
    pprs_list <- mclapply(xml_files, get_data_from_pubmed_xml,
                          include_dates = TRUE,
                          mc.cores = n_workers)
  } else {
    pprs_list <- lapply(xml_files, get_data_from_pubmed_xml,
                        include_dates = TRUE)
  }

  # check for NULLs in pprs_list and remove them before binding rows
  keep <- !vapply(pprs_list, is.null, logical(1))
  # if all are NULL, return an empty data frame with the expected columns
  pprs <- bind_rows(pprs_list[keep])

  if (clean) {
    # remove duplicates
    pprs <- pprs[!duplicated(pprs$pmid), ]
    # remove unwanted publication types by using a vector of strings
    unwanted <- c("Review", "Comment", "Retracted Publication",
                  "Retraction of Publication", "Editorial", "Autobiography",
                  "Biography", "Historical", "Published Erratum",
                  "Expression of Concern", "Editorial")
    # subset pprs to remove unwanted publication types using grepl
    pprs <- pprs[!grepl(paste(unwanted, collapse = "|"), pprs$ptype), ]
    # ensure that ptype contains "Journal Article"
    pprs <- pprs[grepl("Journal Article", pprs$ptype), ]
    # remove papers with "NA NA" as the sole author
    pprs <- pprs[!grepl("NA NA", pprs$authors), ]
  }

  return(pprs)
}
