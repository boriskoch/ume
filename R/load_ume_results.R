#' Load UME Exported Results
#'
#' @title load_ume_results
#'
#' @description
#' Loads a ZIP file or directory produced by [export_ume_results()] and
#' reconstructs all exported data objects plus metadata.
#'
#' @param path Path to a ZIP file or directory containing exported UME results.
#' @param unzip_dir Directory used to unzip into (default: a temporary directory).
#' @keywords internal
#' @return A list with elements:
#'   * `peaklist`
#'   * `mfd`
#'   * `mfd_filt`
#'   * `mfd_filt_tf`
#'   * `mfd_filt_tf_pivot`
#'   * `ds_tf`
#'   * `metadata`
#'
#' @import data.table

load_ume_results <- function(
    path,
    unzip_dir = tempfile("ume_load_")
) {

  # ---------------------------------------------------------------------------
  # Determine input type (zip vs directory)
  # ---------------------------------------------------------------------------
  if (dir.exists(path)) {

    dir <- path

  } else if (file.exists(path) && grepl("\\.zip$", path, ignore.case = TRUE)) {

    dir.create(unzip_dir, recursive = TRUE)
    utils::unzip(path, exdir = unzip_dir)
    dir <- unzip_dir

  } else {
    stop("Path must be an existing directory or a .zip file.")
  }

  # ---------------------------------------------------------------------------
  # Helper to locate and load files
  # ---------------------------------------------------------------------------
  find_file <- function(pattern) {
    f <- list.files(dir, pattern = pattern, full.names = TRUE)
    if (length(f) == 0) return(NULL)
    f[1]
  }

  load_dt <- function(pattern) {
    file <- find_file(pattern)
    if (is.null(file)) return(NULL)
    data.table::fread(file)
  }

  # ---------------------------------------------------------------------------
  # Load all tables written by export_ume_results()
  # ---------------------------------------------------------------------------
  peaklist          <- load_dt("peaklist\\.csv$")
  mfd               <- load_dt("mfd\\.csv$")
  mfd_filt          <- load_dt("mfd_filt\\.csv$")
  mfd_filt_tf       <- load_dt("mfd_filt_tf\\.csv$")
  mfd_filt_tf_pivot <- load_dt("mfd_filt_tf_pivot\\.csv$")
  ds_tf             <- load_dt("ds_tf\\.csv$")

  # ---------------------------------------------------------------------------
  # Load metadata (R dump produced via dput)
  # ---------------------------------------------------------------------------
  metadata_file <- find_file("metadata\\.R$")

  metadata <- if (!is.null(metadata_file)) {
    dget(metadata_file)
  } else {
    warning("No metadata.R file found in export directory.")
    NULL
  }

  # ---------------------------------------------------------------------------
  # Return structured results
  # ---------------------------------------------------------------------------
  out <- list(
    peaklist          = peaklist,
    mfd               = mfd,
    mfd_filt          = mfd_filt,
    mfd_filt_tf       = mfd_filt_tf,
    mfd_filt_tf_pivot = mfd_filt_tf_pivot,
    ds_tf             = ds_tf,
    metadata          = metadata
  )

  class(out) <- c("ume_export", class(out))
  return(out)
}
