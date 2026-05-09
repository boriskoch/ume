
# Calculate other evaluation parameters such as dbe, element ratios etc ####

#' @title Calculate UME Evaluation Parameters
#' @name calc_eval_params
#' @family Formula assignment
#' @family calculations
#' @description
#' This function calculates and adds several evaluation parameters as additional columns to the `mfd` data table.
#' These parameters are essential for evaluating the molecular structure and isotopic distribution, enabling further analysis.
#' For a detailed description of the output table, see `help(mf_data_demo)`.
#' @inheritParams main_docu
#' @return The original `data.table` `mfd` with additional evaluation columns:
#'   \describe{
#'     \item{`nm`}{Nominal molecular mass: Calculated if not already present.}
#'     \item{`dbe`}{Double Bond Equivalent (measure of unsaturation).}
#'     \item{`kmd`}{Kendrick mass defect for CH4 versus O exchange.}
#'     \item{`O/C`, `H/C`, `N/C`, `S/C`}{Element ratios for a molecular formula.}
#'     \item{`nsp_type`, `snp_check`}{Types of combinations of N, S, and P atoms in a formula.}
#'     \item{`nosc`}{Weighted average nominal oxidation state of carbon.}
#'     \item{`delG0_Cox`}{Weighted average Gibbs free energy (Cox) in kJ/mol.}
#'     \item{`ai`}{Aromaticity index.}
#'     \item{`ppm_filt`}{A mass accuracy threshold calculated for each spectrum.}
#'   }
#'
#' @references
#' Hughey C.A., Hendrickson C.L., Rodgers R.P., Marshall A.G., Qian K.N. (2001).
#' Kendrick mass defect spectrum: A compact visual analysis for ultrahigh-resolution
#' broadband mass spectra. *Analytical Chemistry*, **73**, 4676-4681.
#' \doi{10.1021/ac010560w}
#'
#' Koch B.P., Dittmar T. (2006). From mass to structure:
#' an aromaticity index for high-resolution mass data of natural organic matter.
#' *Rapid Communications in Mass Spectrometry*, **20**, 926-932.
#' \doi{10.1002/rcm.2386}
#'
#' LaRowe D.E., Van Cappellen P. (2011). Degradation of natural organic matter:
#' A thermodynamic analysis. *Geochimica et Cosmochimica Acta*, **75**, 2030-2042.
#' \doi{10.1016/j.gca.2011.01.020}
#'
#' @import data.table stats
#' @author Boris P. Koch
#' @examples
#' # Example usage with a demo molecular formula dataset
#' mfd_with_params <- calc_eval_params(mfd = mf_data_demo, verbose = TRUE)
#' @export

calc_eval_params <- function(mfd, verbose = FALSE, ...) {

# Identify column names that are isotopes or elements
# dt_names contains the original names of mfd (to be reset at the end of the column)
  isotope_info <- get_isotope_info(mfd)

# Set column names
  setnames(mfd, isotope_info$orig_name, isotope_info$label)

  new_name <- orig_name <- `12C` <- `14N` <- `16O` <- `1H` <- `31P` <- `32S` <- NULL

  if (verbose == TRUE){
    .msg("Calculating evaluation parameters...")
  }

  # Basic input checks
  if (!inherits(mfd, "data.table")) {
    stop("The 'mfd' argument must be a data.table object.")
  }

  # Ensure the data table is not empty
  if (nrow(mfd) == 0) {
    stop("The 'mfd' data.table is empty.")
  }

# Check if essential columns exist in the input data table
  required_columns <- c("12C", "1H", "16O", "14N", "32S", "31P")
  missing_columns <- setdiff(required_columns, colnames(mfd))

  if (length(missing_columns) > 0) {
    stop("The following required columns are not in 'mfd': ", paste(missing_columns, collapse = ", "))
  }

# Create column "n_occurrence_orig": in how many samples of the dataset does a formula occur?
  mfd[, n_occurrence_orig := .N, by = c(isotope_info[, label])] # calculate occurrence

# Create column for number of assignments per peak (based on raw data)
  mfd[, n_assignments_orig := length(peak_id), .(file_id, peak_id)]

# Calculate nominal mass (if not yet existing)
  if (!"nm" %in% colnames(mfd)) {
    mfd[, nm:=calc_nm(mfd = mfd)]
  }

# Calculate Double Bond Equivalents (DBE)
  # if not yet existing calculate dbe and add column
  if (!"dbe" %in% colnames(mfd)) {
    mfd[, dbe := calc_dbe(mfd = mfd)]
  }

  # Calculate element ratios
  mfd[`12C` > 0, c("oc", "hc", "nc", "sc") :=
        .(round(`16O` / `12C`, 3), round(`1H` / `12C`, 3), round(`14N` / `12C`, 4), round(`32S` / `12C`, 4))]

  mfd[, dbe_o := dbe - `16O`]

  # Calculate aromaticity index
  # Only consider those cases where CAI>0
  # DBEAI>=0 (i.e. '& (1+c-o-s-(0.5*(h+n+p))) >= 0') was not forced because only few values would remain...

  mfd[`12C` - `16O` - `32S` - `14N` - `31P` > 0 ,
      ai := round((1 + `12C` - `16O` - `32S` - (0.5 * (`1H` + `14N` + `31P`))) / (`12C` - `16O` - `32S` - `14N` - `31P`), 1)]

  mfd[`12C` - `16O` - `32S` - `14N` - `31P` <= 0, ai := NA]

  mfd[, wf := dbe - `16O` + (0.5 * (nm %% 14 - 14))]

  # z* and Kendrick mass defect for CH2 and other units
  mfd[, z := (nm %% 14) - 14]
  mfd[, kmd := round(nm - (m * 14 / 14.01565), 4)] # after Kendrick 1963
  #mfd[,kmd_ch4_o:=round((m*16/15.9949146)-(m*16/16.0313001),4)]
  #mfd[,z_coo:=(nm%%44)-44]
  #mfd[,kmd_coo:=round(nm-(m*44/43.98983),4)]
  #mfd[,z_h2:=(nm%%2)-2]
  #mfd[,kmd_h2:=round(nm-(m*2/2.0157),4)]
  #mfd[,z_o:=(nm%%16)-16]
  #mfd[,kmd_o:=round(nm-(m*16/15.994915),4)]
  #mfd[s>0, kmd_c3_sh4:=round(m-(m*36/36.003370818),4)]

  # Nominal oxidation state of carbon and delG0ox (see LaRowe & Van Capellen, 2011)
  mfd[, nosc := round(-1 * ((4 * (`12C`) + `1H` - (3 * `14N`) - (2 * `16O`) + (5 * `31P`) -
                               (2 * `32S`)) / `12C`) + 4, 2)]
  mfd[, delg0_cox := round(60.3 - 28.5 * (-1 * ((
    4 * (`12C`) + `1H` - (3 * `14N`) - (2 * `16O`) + (5 * `31P`) - (2 * `32S`)
  ) / `12C`) + 4), 2)]

  # Calculate theoretical intensity of isotope signals based on molecular formula
  mfd[, relint13c_calc := round(`12C` * 1.08, 4)]
  mfd[, int13c_calc := round(`12C` * 1.08 * i_magnitude / 100, 4)]
  mfd[, relint34s_calc := round(`32S` * 4.47, 4)]
  mfd[, int34s_calc := round(`32S` * 4.47 * i_magnitude / 100, 4)]

  # hr_id_ch2 <- paste0((nm%%14)-14, ' ', round(nm-(mass*14/14.01565),4),
  #                     " 13C", `13C`, " O", o, " N", n+`15N`, " S", `32S`+`34S`, " P", p)
  #mfd[, hr_id_ch2:=paste0(file_id, ' ', (nm%%14)-14, ' ', round(nm-(m_cal*14/14.01565),4),
  #                       " 13C", `13c`, " O", o, " N", n+`15n`, " S", s+`34s`, " P", p)]
  #mfd[, hr_id_ch2_n:= .N, by = .(hr_id_ch2)]

  #mfd[, hr_id_ch4_o:=paste0(file_id, ' ', nm, ' ', round((m_cal*16/15.9949146)-(m_cal*16/16.0313001),3),
  #                         " 13C", `13c`, " O", o, " N", n+`15n`, " S", s+`34s`, " P", p)]
  #mfd[, hr_id_ch4_o_n:= .N, by = .(hr_id_ch4_o)]

  # Assign different classes of heteroatom combinations
  mfd[`32S` > 0 & `14N` > 0 & `31P` > 0, snp_check := 'snp']
  mfd[`32S` > 0 & `14N` > 0 & `31P` == 0, snp_check := 'sn']
  mfd[`14N` > 0 & `31P` > 0 & `32S` == 0, snp_check := 'np']
  mfd[`32S` > 0 & `31P` > 0 & `14N` == 0, snp_check := 'sp']
  mfd[`32S` == 0 & `31P` == 0 & `14N` == 0, snp_check := '']

  # Assign all combinations heteroatoms
  mfd[, nsp_type := paste0("N", `14N`, " S", `32S`, " P", `31P`)]

  # Calculate total number of atoms
  mfd[, co_tot := `12C` + `16O`] # sum of carbon and oxygen atoms (required for island plots and dev_n_c plot)
  mfd[, nsp_tot := `14N` + `32S` + `31P`] # sum of nitrogen, sulfur and phosphorus atoms (required for island plots)

  # Calculate mass accuracy threshold for each analyses (should be based on an initial 1 ppm threshold)
  if (!"ppm_filt" %in% colnames(mfd)) {
    check_ppm <-
      mfd[`14N` == 0 &
            `32S` == 0 &
            `31P` == 0 &
            dbe_o <= 10, .(ppm_filt = round(max(
              abs(stats::quantile(ppm, .975)), abs(stats::quantile(ppm, .025))
            ), 2)), by = .(file_id)]
    mfd <- check_ppm[mfd, on = "file_id"] # add calculated threshold to mfd
  }

  if (verbose == TRUE) message("Done.")

  return(mfd[])
}
