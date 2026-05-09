#' Download and Load a UME Formula Library from Zenodo
#'
#' @description
#' Downloads one of the UME formula libraries from Zenodo **only when explicitly
#' called by the user**.
#'
#' Unlike earlier versions, this CRAN-compliant implementation:
#' - **never writes to the user's filespace unless `dest` is explicitly provided**
#' - **does NOT create ~/.ume/** or any other default directory
#' - **does NOT perform automatic caching**
#' - In non-interactive environments (CRAN checks), the function **returns NULL**
#'
#' @param library Character. One of `"lib_02.rds"` or `"lib_05.rds"`.
#' @param doi Character. Zenodo DOI.
#' @param dest Optional file path where the library should be saved.
#'   If `NULL`, the library is **loaded into memory only**.
#' @param overwrite Logical. Redownload even if `dest` exists?
#'
#' @return A `data.table` or `NULL` (in non-interactive mode).
#' @export
download_library <- local({

  sha256_known <- list(
    "lib_02.rds" =
      "85839023b3ecfbecd4ecd9343fc5e8bcf326932cad5abd96aaf287c78043abaf",
    "lib_05.rds" =
      "8e42df4a5d4c600a2be129ff4cb45981fc8052734460bf29a5b2da8d4790f922"
  )

  sha256_file <- function(path) {
    con <- file(path, "rb")
    on.exit(close(con))
    raw <- readBin(con, what = "raw", n = file.info(path)$size)
    paste0(as.character(openssl::sha256(raw)), collapse = "")
  }

  function(library = "lib_05.rds",
           doi = "10.5281/zenodo.17606457",
           dest = NULL,
           overwrite = FALSE) {

    # CRAN: no network in non-interactive mode
    if (!interactive()) {
      message("download_library(): Non-interactive session -> no download performed.")
      return(invisible(NULL))
    }

    if (!library %in% names(sha256_known)) {
      stop("Unknown library: ", library,
           "\nAvailable: ", paste(names(sha256_known), collapse = ", "))
    }

    # Construct URL
    record_id <- sub(".*zenodo\\.", "", doi)
    url <- sprintf("https://zenodo.org/records/%s/files/%s", record_id, library)

    # ---------------------------------------------------------
    # CASE 1: dest = NULL -> download to temp file (NO WRITING!)
    # ---------------------------------------------------------
    if (is.null(dest)) {
      tmp <- tempfile(fileext = library)

      ans <- readline("Download library into memory (temporary file)? [y/N]: ")
      if (tolower(ans) != "y") {
        message("Cancelled.")
        return(invisible(NULL))
      }

      utils::download.file(url, destfile = tmp, mode = "wb", quiet = FALSE)

      dt <- readRDS(tmp)
      return(data.table::as.data.table(dt))
    }

    # ---------------------------------------------------------
    # CASE 2: dest explicitly provided -> writing allowed
    # ---------------------------------------------------------

    # Only write if user asks for it
    if (!overwrite && file.exists(dest)) {
      message("File already exists at dest. Loading without download.")
      dt <- readRDS(dest)
      return(data.table::as.data.table(dt))
    }

    ans <- readline(paste(
      "Download library and save to:\n", dest, "\nProceed? [y/N]: "
    ))

    if (tolower(ans) != "y") {
      message("Cancelled.")
      return(invisible(NULL))
    }

    utils::download.file(url, destfile = dest, mode = "wb", quiet = FALSE)

    # Verify checksum
    if (!is.na(sha256_known[[library]])) {
      local_hash <- sha256_file(dest)
      if (!identical(local_hash, sha256_known[[library]])) {
        warning("Checksum mismatch for downloaded library.")
      }
    }

    dt <- readRDS(dest)
    return(data.table::as.data.table(dt))
  }
})
