#' PubMed XMLs to CSVs
#'
#' Wrapper function to extract data from all XML files in `data_dir` and save
#' the data frame to disk as csv. Optionally, it can clean the data by removing
#' duplicates, filtering out unwanted publication types, and ensuring that only
#' journal articles are included.
#'
#' @param data_dir string, the directory containing the PubMed XML files to be
#'   processed. Default is "Data".
#' @param output_dir string, the directory for saving output CSVs. Default is
#'   "Output/Data".
#' @param overwrite logical, whether to overwrite existing CSV files. If FALSE
#'   and a CSV file with the same name already exists, the function will skip
#'   writing that file. Default is FALSE.
#'
#' @returns data frame of PubMed records
#' @export

pubmed_xmls_to_csvs <- function(data_dir = "Data",
                             output_dir = "Output/Data",
                             overwrite = FALSE) {
  # ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # function for saving the data frame to disk as csv
  xml_2_csv <- function(xml_filepath) {
    output_path <- file.path(output_dir,
                             paste0(file_path_sans_ext(
                               basename(xml_filepath)), ".csv"))
    # if overwrite is FALSE and the output file already exists, skip writing the csv
    if (!overwrite & file.exists(output_path)) {
      return()
    }
    df <- get_data_from_pubmed_xml(xml_filepath, include_dates = TRUE)

    write.csv(df, output_path, row.names = FALSE)
  }

  xml_files <- list.files(data_dir, pattern = "\\.xml$", full.names = TRUE)

  # Use forked parallelism on macOS/Linux; fall back to serial when unavailable.
  n_workers <- max(1L, detectCores(logical = FALSE) - 1L)

  if (.Platform$OS.type == "unix" && n_workers > 1L) {
    mclapply(xml_files, xml_2_csv,
                          mc.cores = n_workers)
  } else {
    lapply(xml_files, xml_2_csv)
  }

}
