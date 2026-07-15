#' Parse HVP Sample IDs into Discrete Fields
#'
#' Takes a data.table with a column containing HVP sample IDs and parses them
#' into discrete fields representing patient, sample, and storage information.
#' Handles both regular samples and control samples (identified by "-QC-" in the ID).
#'
#' @param dt A data.table containing a column with sample IDs.
#' @param id_col Character string specifying the column name containing sample IDs.
#'   Default is "sample_ID".
#'
#' @return A data.table with the original columns plus parsed fields:
#' \describe{
#'   \item{source_type}{"sample" for regular samples, "control" for QC samples}
#'   \item{enrollment_year}{Year of patient enrollment (2-digit)}
#'   \item{patient_number}{Unique patient identifier (6-digit, or "XXXXXX" for controls)}
#'   \item{project_code}{Project code (e.g., "54" or "U54" for Human Virome)}
#'   \item{collection_site}{Collection site code (e.g., "A2", or "QC" for controls)}
#'   \item{sample_type}{Sample type code (e.g., "STO" for Stool, "NP" for nasal swab)}
#'   \item{sample_sequence}{Sample sequence number}
#'   \item{additive_code}{Additive code}
#'   \item{aliquot_number}{Aliquot number}
#'   \item{freezer}{Freezer letter}
#'   \item{shelf}{Shelf letter}
#'   \item{rack}{Rack number}
#'   \item{box_number}{Box number}
#'   \item{box_position}{Position within the box}
#' }
#'
#' @details
#' Regular sample format (9 segments):
#' `25-000001-54A2-STO03-07-00-AB01-02-15`
#'
#' Control sample format (10 segments, contains "-QC-"):
#' `25-XXXXXX-U54-QC-NP01-20-01-XXXX-XX-XX`
#'
#' @examples
#' library(data.table)
#' dt <- data.table(sample_ID = c("25-000001-54A2-STO03-07-00-AB01-02-15",
#'                                "25-XXXXXX-U54-QC-NP01-20-01-XXXX-XX-XX"))
#' result <- parse_sample_id(dt)
#' print(result)
#'
#' @export
#' @importFrom data.table := copy tstrsplit
parse_sample_id <- function(dt, id_col = "sample_ID") {
  # Input validation
  if (!inherits(dt, "data.table")) {
    stop("Input 'dt' must be a data.table")
  }
  if (!id_col %in% names(dt)) {
    stop(sprintf("Column '%s' not found in data.table", id_col))
  }
  
  # Work on a copy to avoid modifying the original
  result <- data.table::copy(dt)
  
  # Detect source type based on presence of "-QC-"
  ids <- result[[id_col]]
  is_control <- grepl("-QC-", ids, fixed = TRUE)
  
  # Initialize output columns
  result[, source_type := ifelse(is_control, "control", "sample")]
  result[, enrollment_year := NA_character_]
  result[, patient_number := NA_character_]
  result[, project_code := NA_character_]
  result[, collection_site := NA_character_]
  result[, sample_type := NA_character_]
  result[, sample_sequence := NA_character_]
  result[, additive_code := NA_character_]
  result[, aliquot_number := NA_character_]
  result[, freezer := NA_character_]
  result[, shelf := NA_character_]
  result[, rack := NA_character_]
  result[, box_number := NA_character_]
  result[, box_position := NA_character_]
  
  # Parse regular samples (9 segments)
  # Format: 25-000001-54A2-STO03-07-00-AB01-02-15
  if (any(!is_control)) {
    sample_idx <- which(!is_control)
    sample_ids <- ids[sample_idx]
    parts <- data.table::tstrsplit(sample_ids, "-", fixed = TRUE)
    
    result[sample_idx, enrollment_year := parts[[1]]]
    result[sample_idx, patient_number := parts[[2]]]
    result[sample_idx, project_code := substr(parts[[3]], 1, 2)]
    result[sample_idx, collection_site := substr(parts[[3]], 3, nchar(parts[[3]]))]
    result[sample_idx, sample_type := substr(parts[[4]], 1, 3)]
    result[sample_idx, sample_sequence := substr(parts[[4]], 4, nchar(parts[[4]]))]
    result[sample_idx, additive_code := parts[[5]]]
    result[sample_idx, aliquot_number := parts[[6]]]
    result[sample_idx, freezer := substr(parts[[7]], 1, 1)]
    result[sample_idx, shelf := substr(parts[[7]], 2, 2)]
    result[sample_idx, rack := substr(parts[[7]], 3, nchar(parts[[7]]))]
    result[sample_idx, box_number := parts[[8]]]
    result[sample_idx, box_position := parts[[9]]]
  }
  
  # Parse control samples (10 segments)
  # Format: 25-XXXXXX-U54-QC-NP01-20-01-XXXX-XX-XX
  if (any(is_control)) {
    ctrl_idx <- which(is_control)
    ctrl_ids <- ids[ctrl_idx]
    parts <- data.table::tstrsplit(ctrl_ids, "-", fixed = TRUE)
    
    result[ctrl_idx, enrollment_year := parts[[1]]]
    result[ctrl_idx, patient_number := parts[[2]]]
    result[ctrl_idx, project_code := parts[[3]]]
    result[ctrl_idx, collection_site := parts[[4]]]  # "QC"
    result[ctrl_idx, sample_type := substr(parts[[5]], 1, 2)]
    result[ctrl_idx, sample_sequence := substr(parts[[5]], 3, nchar(parts[[5]]))]
    result[ctrl_idx, additive_code := parts[[6]]]
    result[ctrl_idx, aliquot_number := parts[[7]]]
    result[ctrl_idx, freezer := substr(parts[[8]], 1, 1)]
    result[ctrl_idx, shelf := substr(parts[[8]], 2, 2)]
    result[ctrl_idx, rack := substr(parts[[8]], 3, nchar(parts[[8]]))]
    result[ctrl_idx, box_number := parts[[9]]]
    result[ctrl_idx, box_position := parts[[10]]]
  }
  
  return(result)
}