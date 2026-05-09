#' Export UME Analysis Results
#'
#' @title Export UME Analysis Results
#'
#' @description
#' Exports UME analysis results to a structured output folder.
#' The function writes the following objects to CSV (if provided):
#'
#' * `pl` – peaklist
#' * `mfd` – full molecular formula dataset
#' * `mfd_filt` – filtered MFD
#' * `mfd_filt_tf` – transformed filtered MFD
#' * `mfd_filt_tf_pivot` – pivoted intensity matrix
#' * `ds_tf` – transformed diagnostics / statistics
#'
#' Optionally, the function can export plot objects, create a ZIP archive
#' of all exported files, and write a metadata file (`metadata.R`)
#' containing a reproducibility snapshot that can be used later in
#' `load_ume_results()`.
#'
#' @inheritParams main_docu
#'
#' @param mfd_filt `data.table` or coercible object.
#'   Filtered version of the molecular formula dataset (optional).
#' @param mfd_filt_tf `data.table` or coercible object.
#'   Transformed filtered MFD used in downstream calculations (optional).
#' @param mfd_filt_tf_pivot `data.table` or coercible object.
#'   Pivoted / wide-format intensity matrix derived from `mfd_filt_tf` (optional).
#' @param ds_tf `data.table` or coercible object.
#'   Transformed diagnostic statistics (optional).
#'
#' @param outdir Character.
#'   Output directory in which all export files are stored.
#'   The directory is created if it does not exist.
#'   **Must be provided explicitly**; no default is used to comply with CRAN
#'   policies on writing to the user's filespace.
#'   For temporary exports, use e.g. `outdir = file.path(tempdir(), "ume_export")`.
#'
#' @param prefix Character.
#'   Prefix for all exported file names (e.g., `"SRFA_001"`).
#'   Default: `"ume"`.
#'
#' @param figures
#'   Controls figure export:
#'
#'   * `FALSE` – no figures are exported
#'   * `TRUE` – export all plot-like objects found in `env`
#'   * character vector – export only the listed object names
#'
#'   Recognized plot types are:
#'   **ggplot**, **plotly**, and **recordedplot** (base R).
#'
#' @param fig_width,fig_height Numeric.
#'   Dimensions of exported figures in inches.
#'   Default: `8` × `6`.
#'
#' @param fig_device Character.
#'   File format for figure export.
#'   One of `"png"` (default) or `"pdf"`.
#'
#' @param zip Logical.
#'   If `TRUE` (default), the exported directory is compressed into a `.zip` file
#'   in the same parent directory as `outdir`.
#'
#' @param metadata Named list.
#'   Additional metadata to write into `metadata.R`
#'   (e.g., analysis settings, instrument parameters, user comments).
#'   Default: empty list.
#'
#' @param env Environment.
#'   Environment from which figure objects should be collected.
#'   Default: `parent.frame()`.
#' @keywords internal
#' @return
#' Invisibly returns:
#'
#' * the **path to the ZIP file** (if `zip = TRUE`), or
#' * the **path to the output directory** (if `zip = FALSE`).
#'
#' @import data.table
#' @importFrom utils packageDescription zip

export_ume_results <- function(
    pl,
    mfd,
    mfd_filt = NULL,
    mfd_filt_tf = NULL,
    mfd_filt_tf_pivot = NULL,
    ds_tf = NULL,
    outdir = NULL,
    prefix = "ume",
    figures = FALSE,
    fig_width = 8,
    fig_height = 6,
    fig_device = c("png", "pdf"),
    zip = TRUE,
    metadata = list(),
    env = parent.frame()
) {

  # ---------------------------------------------------------------------------
  # CRAN-compliant: require explicit outdir
  # ---------------------------------------------------------------------------
  if (is.null(outdir) || !nzchar(outdir)) {
    stop(
      "Argument 'outdir' must be specified explicitly.\n",
      "For temporary exports, use e.g. outdir = file.path(tempdir(), 'ume_export')."
    )
  }

  fig_device <- match.arg(fig_device)

  # Create output directory if needed
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

  # ---------------------------------------------------------------------------
  # Helper: coerce to data.table
  # ---------------------------------------------------------------------------
  to_dt <- function(x) {
    if (is.null(x)) return(NULL)
    if (data.table::is.data.table(x)) return(x)
    data.table::as.data.table(x)
  }

  peaklist          <- to_dt(pl)
  mfd               <- to_dt(mfd)
  mfd_filt          <- to_dt(mfd_filt)
  mfd_filt_tf       <- to_dt(mfd_filt_tf)
  mfd_filt_tf_pivot <- to_dt(mfd_filt_tf_pivot)
  ds_tf             <- to_dt(ds_tf)

  # ---------------------------------------------------------------------------
  # Export tables
  # ---------------------------------------------------------------------------
  export_csv <- function(dt, name) {
    if (!is.null(dt)) {
      outfile <- file.path(outdir, sprintf("%s_%s.csv", prefix, name))
      data.table::fwrite(dt, outfile)
    }
  }

  export_csv(peaklist,          "peaklist")
  export_csv(mfd,               "mfd")
  export_csv(mfd_filt,          "mfd_filt")
  export_csv(mfd_filt_tf,       "mfd_filt_tf")
  export_csv(mfd_filt_tf_pivot, "mfd_filt_tf_pivot")
  export_csv(ds_tf,             "ds_tf")

  # ---------------------------------------------------------------------------
  # Export metadata (R dput file)
  # ---------------------------------------------------------------------------
  meta <- list(
    timestamp   = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    r_version   = R.Version()$version.string,
    ume_version = as.character(
      utils::packageDescription("ume", fields = "Version")
    ),
    prefix      = prefix,
    arguments   = list(
      figures    = figures,
      fig_width  = fig_width,
      fig_height = fig_height,
      fig_device = fig_device
    ),
    user_metadata = metadata
  )

  dput(meta, file = file.path(outdir, paste0(prefix, "_metadata.R")))

  # ---------------------------------------------------------------------------
  # Export figures
  # ---------------------------------------------------------------------------
  if (!identical(figures, FALSE)) {

    if (identical(figures, TRUE)) {
      obj_names <- ls(env)
    } else if (is.character(figures)) {
      obj_names <- figures
    } else {
      stop("Argument 'figures' must be TRUE, FALSE, or a character vector.")
    }

    for (nm in obj_names) {
      obj <- try(get(nm, envir = env), silent = TRUE)
      if (inherits(obj, "try-error")) next

      is_plot <- inherits(obj, "ggplot") ||
        inherits(obj, "recordedplot") ||
        inherits(obj, "plotly")

      if (!is_plot) next

      outfile <- file.path(outdir, sprintf("%s_%s.%s", prefix, nm, fig_device))

      # ggplot / recordedplot: draw into device and close
      if (!inherits(obj, "plotly")) {
        if (fig_device == "png") {
          grDevices::png(outfile, width = fig_width, height = fig_height,
                         units = "in", res = 150)
        } else {
          grDevices::pdf(outfile, width = fig_width, height = fig_height)
        }

        try({
          if (inherits(obj, "ggplot")) {
            print(obj)
          } else {
            grDevices::replayPlot(obj)
          }
        }, silent = TRUE)

        grDevices::dev.off()

      } else {
        # plotly: let plotly handle file export directly
        try({
          plotly::export(obj, file = outfile)
        }, silent = TRUE)
      }
    }
  }

  # ---------------------------------------------------------------------------
  # Create ZIP archive (optional)
  # ---------------------------------------------------------------------------
  if (isTRUE(zip)) {
    zipfile <- paste0(outdir, ".zip")
    utils::zip(
      zipfile = zipfile,
      files   = list.files(outdir, full.names = TRUE),
      flags   = "-jr9"
    )
    return(invisible(normalizePath(zipfile)))
  }

  invisible(normalizePath(outdir))
}
