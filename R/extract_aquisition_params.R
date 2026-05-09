
#' Extract Acquisition Parameters from a Bruker PDF Report
#'
#' This function reads a PDF file from Bruker Compass DataAnalysis reports, extracts
#' acquisition parameters, including the spectrum filename and analysis method, and
#' returns them as a `data.table`. Parameter values are separated into numeric values
#' and corresponding units.
#'
#' @param pdf_path Character. Path to the PDF file.
#' @return A `data.table` with columns: `Parameter`, `Value`, `Unit`, `Spectrum_Filename`, `Analysis_Method`.
#' @import data.table
# @importFrom pdftools pdf_text
#' @family internal functions
#' @keywords internal

extract_aquisition_params <- function(pdf_path) {

  Wert <- Parameter <- value <- filename <- NULL

  if (requireNamespace("pdftools", quietly = TRUE)) {

  # PDF-Text extrahieren (erste Seite)
  text <- pdftools::pdf_text(pdf_path)[1]

  # Zerlege den Text in Zeilen und entferne leere Zeilen
  lines <- unlist(strsplit(text, "\n"))
  lines <- trimws(lines[lines != ""])

  # Suche die Position der "Aquisition Parameter Table"
  param_start <- which(grepl("Aquisition Parameter Table", lines, ignore.case = TRUE))
  if (length(param_start) == 0) stop("Keine Aquisition Parameter Tabelle gefunden.")

  filename_line <- which(grepl("Spectrum Filename", lines, ignore.case = TRUE))
  method_line <- which(grepl("Analysis Method", lines, ignore.case = TRUE))

  # Extrahiere die relevanten Zeilen (bis zum nächsten Abschnitt)
  param_lines <- lines[c(filename_line, method_line, (param_start + 1):length(lines))]

  # Finde die erste Zeile nach den Parametern (z. B. durch eine Leerzeile oder neuen Abschnitt)
  next_section <- which(param_lines == "")[1]
  if (!is.na(next_section)) {
    param_lines <- param_lines[1:(next_section - 1)]
  }

  # Extrahiere Parameter und Werte mit regulärem Ausdruck
  param_dt <- rbindlist(lapply(param_lines, function(line) {
    match <- regexpr("(.+?)\\s{2,}(.+)", line)  # Trenne bei mindestens zwei Leerzeichen
    if (match[1] > 0) {
      param <- regmatches(line, match)[1]
      split_values <- unlist(strsplit(param, "\\s{2,}"))
      data.table(Parameter = split_values[1], Wert = split_values[2])
    } else {
      NULL
    }
  }), fill = TRUE)

  param_dt[, c("value", "unit"):=data.table::tstrsplit(Wert, " ")]
  param_dt$Wert <- NULL
  fn <- param_dt[Parameter == "Spectrum Filename", value]
  param_dt[, filename:=fn]
  param_dt <- param_dt[!Parameter == "Spectrum Filename"]
  return(param_dt)
  } else {
    stop("Package 'pdftools' is required but not installed.")
  }

}
