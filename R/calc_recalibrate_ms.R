#' @title Recalibrate mass spectra
#' @name calc_recalibrate_ms
#' @family calculations
#'
#' @description
#' This function performs an automated mass recalibration for peak lists using
#' predefined or user-specified calibrant lists.
#'
#' Calibration can be based on existing calibrant tables included in `ume::known_mf`
#' (via the `calibr_list` argument) or on a user-provided set of molecular formulas
#' (`custom_calibr_list`).
#'
#' The function assigns calibrant peaks to each spectrum and evaluates their
#' mass accuracy. Three independent outlier tests are applied to the assigned
#' calibrants, and only those that pass all tests are used to calculate the
#' recalibration model.
#'
#' Recalibration is performed using a linear model (`m ~ m_cal`), and spectra with
#' insufficient calibrant matches can be either excluded or corrected using
#' extrapolated calibration parameters.
#'
#' @inheritParams main_docu
#' @inheritDotParams assign_formulas
#' @inheritDotParams calc_neutral_mass
#' @inheritDotParams calc_ma_abs
#'
#' @param calibr_list Character string. Name of a predefined calibrant list stored in
#' `ume::known_mf` (column `category`). Ignored if `custom_calibr_list` is provided.
#' @param custom_calibr_list Character vector. Custom list of molecular formulas to
#' be used as calibrants instead of a predefined list.
#' @param col_spectrum_id Character. Name of the column that identifies individual
#' spectra or samples (default: `"file_id"`). The peaklist must also contain a
#' column named `"mass"`.
#' @param min_no_calibrants Integer. Minimum number of calibrant peaks required per
#' spectrum to perform recalibration (default: 3). If fewer calibrants are found,
#' recalibration is skipped or handled according to `insufficient_calibrants`.
#' @param outlier_removal Logical. If `TRUE` (default), mass-accuracy-based outlier
#' detection is applied to the calibrants within each spectrum before recalibration.
#' @param insufficient_calibrants Character. Defines how spectra with too few
#' calibrants are handled:
#' \describe{
#'   \item{`"extrapolate"`}{Apply the median calibration slope and intercept from
#'   spectra with at least two calibrants (default).}
#'   \item{`"remove_spectrum"`}{Remove spectra for which no calibrant peaks were
#'   identified.}
#' }
#'
#' @return A list containing:
#' \describe{
#'   \item{`pl`}{Recalibrated peaklist.}
#'   \item{`check`}{Summary of the number of calibrants per spectrum.}
#'   \item{`cal_peaks`}{Assigned calibrant peaks and recalibration results.}
#'   \item{`cal_stats`}{Calibration statistics (slopes, intercepts, accuracy metrics).}
#'   \item{`fig_*`}{Interactive \pkg{plotly} figures comparing mass accuracy
#'   before and after recalibration.}
#' }
#'
#' @details
#' Recalibration is based on a linear fit (`lm(m ~ m_cal)`), with slopes and intercepts
#' computed individually for each spectrum. Optionally, spectra without sufficient
#' calibrants can be corrected using median calibration parameters derived from
#' other spectra.
#'
#' @import data.table
#' @importFrom plotly ggplotly
#' @keywords internal
#' @author Boris P. Koch
#' @export

calc_recalibrate_ms <- function(pl,
                                col_spectrum_id = "file_id",
                                calibr_list = c(
                                  "cal_fa_neg",
                                  "cal_marine_dom_neg",
                                  "calibration",
                                  "marine_dom",
                                  "cal_marine_dom_pos",
                                  "cal_marine_pw_neg",
                                  "cal_SRFA_neg",
                                  "cal_SRFA_OL_neg",
                                  "E_coli_metabolome",
                                  "Post-column standard"
                                ),
                                custom_calibr_list = NULL,
                                min_no_calibrants = 1,
                                outlier_removal = TRUE,
                                insufficient_calibrants = c("extrapolate", "remove_spectrum"),
                                verbose = FALSE,
                                pol = c("neg", "pos", "neutral"), ma_dev,
                                ...) {
  # to do: ####
  # Make sure that file_id's in peaklist contain data:
  # this throws an error: quantile(NA, 0.975)
  # this is na: quantile(NULL, 0.975)
  # Create plotly boxplot for all file_id's (before and after calibration) Quick info for calibrant list
  # Assign different calibration lists to file_id (i.e. blanks with cal_fa_neg, samples with other)
  # assign file_ids to either sample or blank
  # Do the same process with a selected Bruker ref file (user input dialogue)

  #pl <- check_peaklist(pl, ...)

  calibr_list <- match.arg(calibr_list)
  insufficient_calibrants <- match.arg(insufficient_calibrants)
  pol <- match.arg(pol)

  out_score <- m_recalib <- intercept <- slope <- ppm_recal <- analysis_filename <- NULL
  intercept_interpol <- slope_interpol <- ..col_spectrum_id <- ..cols_stats <- NULL

  m_diff <- m_uncalib <- NULL

  # Number of unique file_ids at the beginning
  file_id_size <- length(unique(pl[, get(col_spectrum_id)]))
  file_id_size

  # Number of peaks at the beginning:
  n_peaks_orig <- pl[, .N]

# Load list of calibrants ####
  if(!is.null(custom_calibr_list)){
    ref <- ume::convert_molecular_formula_to_data_table(mf = custom_calibr_list, table_format = "wide")
  } else {
    ref <- ume::convert_molecular_formula_to_data_table(mf = ume::known_mf[category == calibr_list, mf])
  }

# Set key / sorting:
  data.table::setkey(ref, mass)

  if (verbose == TRUE) {
    message("Number of calibrants in calibration file: ", nrow(ref), "\n")
  }

# Assign calibrant formulas to peaklist ####
  cols <- unique(c(col_spectrum_id, "file_id", "file", "peak_id", "mz", "i_magnitude"))
  if("ret_time_min" %in% names(pl)){
    cols <- c(cols, "ret_time_min") # add retention time
  }

# Assign calibration masses / formulas to peaklist
  cal_peaks <- assign_formulas(pl = pl[, ..cols], formula_library = ref, ma_dev = ma_dev, pol = pol, verbose = T)

  # pl[, .N, .(file_id, ret_time_min)]
  # cal_peaks[, .N, .(file_id, ret_time_min)][order(N)]
  # plot(cal_peaks[, .N, .(ret_time_min)])

  cal_peaks[, m_diff:=m-m_cal]

# Number of identified calibrant peaks in a spectrum
  check <-
    cal_peaks[, .(n_calibrants = .N), col_spectrum_id][pl[, .(n_peaks = .N), col_spectrum_id], on = col_spectrum_id]

# Check if there are files, for which less than 'min_no_calibrants' masses was found
  no_cal <- check[n_calibrants < min_no_calibrants | is.na(n_calibrants)]
  no_cal

  if (nrow(check) == nrow(no_cal)) {
    stop(
      paste0(
        "None of the calibrant masses in '",
        ifelse(is.null(custom_calibr_list), yes = calibr_list, no = "custom_calibr_list"),
        "' were identified in any of the measurements (file_id's)."
      )
    )
  }
  if(nrow(check) != nrow(no_cal) & nrow(no_cal) > 0) {
    warning(
      paste0("For one or more spectra (", col_spectrum_id, ") less than ",
        min_no_calibrants, " calibrant masses were identified (based on '",
        ifelse(is.null(custom_calibr_list), yes = calibr_list, no = "custom_calibr_list"),
        "):"
      )
    )
    .msg(no_cal)

    if(insufficient_calibrants == "remove_spectra"){
    warning("Removing ",
            nrow(no_cal),
            " spectra from a total of ",
            nrow(check),
            " spectra.")
    pl <-
      pl[!get(col_spectrum_id) %in% no_cal[, get(col_spectrum_id)]]
    }
  }

  # remove unnecessary isotope columns
  #cal_peaks[, c("13C", "15N", "34S") := NULL]

  if (verbose == TRUE) {
    message(
      "Number of calibrants identified in peaklist (at mass accuracy of +/- ",
      ma_dev,
      " ppm): ",
      nrow(cal_peaks),
      "\n"
    )
  }

  # Determine outliers for calibration peaks ####
  if (outlier_removal == TRUE) {
    cal_peaks <- ustats_outlier(dt = cal_peaks, check_col = "ppm")

    # Keep only calibrant peaks that passed all three outlier tests
    #, "out_rosner" was disabled because it did not find outliers

    cp <- cal_peaks[out_score == 0]
    cp[, c("out_box", "out_quantile", "out_hampel") := NULL]

    if (verbose == TRUE) {
      message("Number of remaining calibrants in peaklist after outlier removal: ",
          nrow(cp),
          "\n")
    }

  } else {
    cp <- cal_peaks
  }

  # Check calibrant peaks after removal of outlier
  # cp <- cp[!file_id %in% cp[, .N, .(file_id)][N < min_no_calibrants, file_id]]
  if (nrow(cp) == 0) {
    stop(paste(
      "All file_ids had insufficient numbers of calibration masses in '",
      calibr_list,
      "'!"
    ))
  }

  if (verbose == TRUE) {
    message(round(length(unique(cp$file_id)) / file_id_size, 2) * 100,
        " % of the spectra (", col_spectrum_id, ") in the peaklist (pl) will be calibrated.\n")
  }

# Add number of calibrants per spectrum
  cp[, n_calibrants := .N, get(col_spectrum_id)]

  # Calculate linear model ####
  cal_stats <-
    cp[, .(
      slope = coef(lm(m ~ m_cal))[[2]],
      intercept = coef(lm(m ~ m_cal))[[1]],
      median_ma_before = median(ppm),
      q975_before = round(quantile(ppm, .975), 2),
      q25_before = round(quantile(ppm, .025), 2)
    ), by = col_spectrum_id]

  # Add linear model stats to list of calibrants
  cols_stats <- c(col_spectrum_id, "slope", "intercept")
  cp <- cal_stats[,  mget(cols_stats)][cp, on = col_spectrum_id]

# Recalibrate mass for calibrant list ####
  cp[n_calibrants>1, m_recalib := (m - intercept) / slope]
  cp[n_calibrants==1, m_recalib := m - m_diff] # One point calibration

# Extrapolate calibration results for file_id's that were not covered by the procedure above
  # to do: Could be based on the calculation in line 334 ff

# Recalculate mass accuracy for calibrant list ####
  cp[, ppm_recal := round((m_recalib - m_cal) / (m_cal) * 1000000, 4)]
  tmp <-
    cp[, .(
      median_ma_after = median(ppm_recal, na.rm = TRUE),
      q975_after = round(quantile(ppm_recal, .975, na.rm = TRUE), 2),
      q25_after = round(quantile(ppm_recal, .025, na.rm = TRUE), 2),
      n_calibrants = unique(n_calibrants),
      m_diff = mean(m_diff)
    ), by = col_spectrum_id]

  cal_stats <- tmp[cal_stats, on = col_spectrum_id]

  setcolorder(
    cal_stats,
    neworder = c(
      col_spectrum_id,
      "n_calibrants",
      "median_ma_before",
      "median_ma_after",
      "q25_before",
      "q25_after",
      "q975_before",
      "q975_after",
      "slope",
      "intercept",
      "m_diff"
    )
  )

  # Plot calibration for calibrants before and after recalibration ####
  # A maximum of 10 analyses is plotted - otherwise things are too slow

  n_boxes <- min(10, nrow(cp[, .N, get(col_spectrum_id)]))
  box_files <- cp[, .N, by = col_spectrum_id][1:n_boxes, get(col_spectrum_id)]
  fig_box_before <-
    plotly::plot_ly(
      data = cp[get(col_spectrum_id) %in% box_files],
      y = ~ ppm,
      type = "box",
      color = ~ as.factor(get(col_spectrum_id))
    ) |>
    plotly::layout(title = "Mass accuracy before calibration (max. first 10 spectra)",
                   xaxis = list(title = "Spectrum ID"),
                   yaxis = list(title = "Mass accuracy (ppm)"))

  fig_box_after <-
    plotly::plot_ly(
      data = cp[get(col_spectrum_id) %in% box_files],
      y = ~ ppm_recal,
      type = "box",
      color = ~ as.factor(get(col_spectrum_id))
    ) |>
    plotly::layout(title = "Mass accuracy after calibration (max. first 10 spectra)",
                   xaxis = list(title = "Spectrum ID"),
                   yaxis = list(title = "Mass accuracy (ppm)"))

  fig_ma_before <-
    plotly::plot_ly(
    data = cp[get(col_spectrum_id) %in% box_files, .(file = get(col_spectrum_id), m, ppm)],
    x = ~ m,
    y = ~ ppm,
    type = "scatter",
    color = ~ as.factor(file)
  ) |> plotly::layout(title = "Mass accuracy before calibration (only first 10 spectra)",
              xaxis = list(title = "Neutral molecular mass (Da)"),
              yaxis = list(title = "Mass accuracy (ppm)"))

  fig_ma_after <-
    plotly::plot_ly(
      data = cp[get(col_spectrum_id) %in% box_files, .(file = get(col_spectrum_id), m_recalib, ppm_recal)],
      x = ~ m_recalib,
      y = ~ ppm_recal,
      type = "scatter",
      color = ~ as.factor(file)
    ) |> plotly::layout(title = "Mass accuracy after calibration (only first 10 spectra)",
                xaxis = list(title = "Neutral molecular mass (Da)"),
                yaxis = list(title = "Mass accuracy (ppm)"))

  # This takes extremely long and therefore is deactivated
  # if (nrow(unique(cp[, ..col_spectrum_id])) > 20) {
  #   fig_box_before <-
  #     fig_box_before |>
  #     plotly::style(p = fig_box_before,
  #           visible = "legendonly",
  #           traces = c(20:nrow(unique(cp[, ..col_spectrum_id]))))
  #   fig_box_after <-
  #     plotly::style(p = fig_box_after,
  #           visible = "legendonly",
  #           traces = c(20:nrow(unique(cp[, ..col_spectrum_id]))))
  # }

  fig_hist_before <-
    uplot_freq_ma(mfd = cp, ma_col = "ppm", logo = T
    )
  # layout(xaxis = list(range=c(-1,1))) # ume::uplot.freq_ma returns object of class NULL, layout requires plotly object
  fig_hist_after <-
    uplot_freq_ma(mfd = cp, ma_col = "ppm_recal")

# Recalibrate entire peaklist ####
  pl <- cal_stats[pl, on = col_spectrum_id]

  pl[, m_uncalib:=calc_neutral_mass(mz = mz, pol = pol, ma_dev = ma_dev)]
  pl[, m:= (m_uncalib - intercept) / slope]
  pl[is.na(m) & n_calibrants==1, m := m_uncalib-m_diff] # One point calibration

  if (nrow(pl[is.na(m)])) {
    warning("There were ", nrow(pl[is.na(m)]), " masses that could not be recalibrated.")
    if (insufficient_calibrants == "extrapolate") {
      message(
        "Trying to extrapolate calibration from other spectra having at least 2 calibrant masses."
      )
      if ("ret_time_min" %in% names(pl)) {
        if(verbose){message("Only spectra with identical retention time are considered.")}
        other_cal_curves <- cp[n_calibrants >= 2, .(
          intercept_interpol = median(intercept),
          slope_interpol = median(slope),
          .N
        ), .(ret_time_min)]

        pl <- other_cal_curves[pl, on = "ret_time_min"]
        pl[is.na(m), m := (m_uncalib - intercept_interpol) / slope_interpol]
      } else {
        other_cal_curves <-   cp[n_calibrants >= 2, .(
          intercept_interpol = median(intercept),
          slope_interpol = median(slope),
          .N
        )]
        pl[is.na(m), m := (m_uncalib - other_cal_curves$intercept_interpol) / other_cal_curves$slope_interpol]
      }
    }
    if (insufficient_calibrants == "remove") {
      message("Removing ", pl[is.na(m), .N], " that could not be calibrated because of missing calibrant masses.")
      pl <- pl[!(is.na(m) | n_calibrants < min_no_calibrants)]
    }
  }
  if (pol == "neg") {
    pl[, mz := m - 1.0072763]
  }
  if (pol == "pos") {
    pl[, mz := m + 1.0072763]
  }
  if (pol == "neutral") {
    pl[, mz := m]
  }

  # return all results as a list object ####
  cal_result_list <- list(
    "pl" = pl,
    "check" = check,
    "cal_peaks" = cp,
    "cal_stats" = cal_stats,
    "fig_hist_before" = fig_hist_before,
    "fig_hist_after" = fig_hist_after,
    "fig_box_before" = fig_box_before,
    "fig_box_after" = fig_box_after,
    "fig_ma_before" = fig_ma_before,
    "fig_ma_after" = fig_ma_after
  )
  names(pl)
  return(cal_result_list)
}
