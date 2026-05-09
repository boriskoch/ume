#' @title Molecular Formula Assignment
#' @description
#' Assigns molecular formulas to molecular masses using a predefined library.
#' Input of the peaklist (`pl`) is internally checked `as_peaklist()`,
#' converted to neutral masses [calc_neutral_mass()], and assigned with
#' molecular formulas based on the mass accuracy (`ma_dev`) provided [calc_ma_abs()].
#' The input can be either:
#' \itemize{
#'   \item A peaklist (`data.table`) containing m/z values or neutral masses
#'         and additional metadata
#'         .
#'   \item A numeric vector of m/z values or neutral masses without additional metadata
#'         (internally checked and standardized by `as_peaklist()`).
#' }
#'
#' @param pl Either a peaklist (`data.table`) with at least columns `mz`, `i_magnitude`,
#'        and `file_id`, or a numeric vector of masses. For numeric input, a minimal
#'        peaklist is constructed internally.
# @param ma_dev Mass accuracy in +/- parts per million (ppm)
# @param pol Polarity of the molecular ion ("neg", "pos", "neutral").
#' @inheritParams main_docu
#' @inheritDotParams calc_ma_abs
#' @inheritDotParams calc_neutral_mass
# @param pol Passed to [calc_neutral_mass()]
# @param ma_dev Passed to [calc_ma_abs()]
# @param memory_efficient (FALSE / TRUE) Splits large peaklists into chunks and
# calculates formulas in batches to be more memory efficient.
# @param chunk_n Number of peaks that are processed in one chunk
# when memory_efficient is st to TRUE (DEFAULT 5e5)
# This is slightly slower compared to formula assignment of the entire peaklist.
#' @import data.table
#' @details This function calculates the neutral mass of peaks in `pl` and
#' compares it to mass values in `formula_library`, assigning molecular formulas
#' based on mass accuracy thresholds. If 13C, 15N, or 34S isotope information
#' is missing, additional columns are added to the output table.
#' @return
#' A `data.table` where each row represents a molecular formula assigned to a
#' mass peak. The table contains:
#'
#' \itemize{
#'   \item All columns of the input peaklist `pl` (e.g. `mz`, `i_magnitude`, `file_id`).
#'   \item All columns of the input `formula_library` (e.g. `mf`, element counts).
#'   \item Calculated columns:
#'     \itemize{
#'       \item `m` — neutral mass.
#'       \item `m_cal` — exact mass of the assigned formula.
#'       \item `del` — absolute mass error (Da).
#'       \item `ppm` — mass error in parts per million.
#'       \item `mf_id` — unique ID for each (file_id, mf) combination.
#'     }
#'   \item Added isotope columns (`13C`, `15N`, `34S`) if missing in the library.
#' }
#'
#' One peak may receive zero, one, or multiple assigned formulas depending on the
#' mass accuracy threshold.
#' @author Boris P. Koch
#'
#' @examples
#' # Example using demo data and demo peak list:
#' assign_formulas(pl = peaklist_demo,
#'                 formula_library = ume::lib_demo,
#'                 pol = "neg",
#'                 ma_dev = 0.2,
#'                 verbose = FALSE)
#'
#'  # Example using a given mass and UME demo library:
#'  mfd <- assign_formulas(pl = 254.0426527, formula_library = ume::lib_demo,
#'  pol = "neutral", ma_dev = 0.5, verbose = TRUE)
#' @export

assign_formulas <- function(pl,
                            formula_library,
                            verbose = FALSE,
                            #memory_efficient = FALSE,
                            #chunk_n = 5e5,
                            ...) {

  if (!is_ume_peaklist(pl)) {
    pl <- as_peaklist(pl)
  }

# Capture additional arguments from ellipsis
  inargs <- list(...)

  '13C'  <- '15N' <- '34S' <- NULL

# If the formula library is simply a character vector of molecular formulas
  if(is.vector(formula_library) & is.character(formula_library)){
    formula_library <- create_custom_formula_library(formula_library)
  }

# Make sure that library has no missing values and that "vkey" exists
# to do: only do this if a library different from ume.formulas is used
  formula_library <- check_formula_library(formula_library)

# Ensure required arguments are provided
  required_args <- c("pol", "ma_dev")
  missing_args <- setdiff(required_args, names(inargs))

  if (length(missing_args) > 0) {
    stop("Missing required argument(s): ", paste(missing_args, collapse = ", "))
  }

# Check peaklist, calculate neutral mass and mass accuracy thresholds
  pl[, m := calc_neutral_mass(mz = mz, ...)]

  # m_val <- calc_neutral_mass(mz = pl$mz, ...)
  # set(pl, j = "m", value = m_val)

  pl[, c("m_min", "m_max") := calc_ma_abs(m = m, ...)]

  .msg("Column names of peaklist:\n %s", paste0(names(pl), collapse = ", "))

  .msg("Assigning molecular formulas to masses using:
       Mass accuracy threshold: +/- %0.1f ppm
       Charge state: %s \n",
       inargs[["ma_dev"]], inargs[["pol"]])

# Check if the range of masses in pl are within the range of mass values in the library
  if (pl[, min(m_min)] < formula_library[, min(mass)-1] | pl[, max(m_max)] > formula_library[, max(mass)+1]) {
    if (verbose){
      message("  Peaklist mass range: ",
              round(pl[, min(m)], 5), " - ", round(pl[, max(m)], 5),
              " Da (", pl[, .N], " peak(s))")
      message("   Library mass range: ",
        round(formula_library[, min(mass)], 5), " - ", round(formula_library[, max(mass)], 5),
        " Da (", formula_library[, .N], " formula(s))\n"
        )}
    a <- pl[, .N]

  # This step truncates the peaklist. The truncation takes the min and max mass
    # in the library +/- 1 Da mass buffer
    pl <- pl[m_min >= min(formula_library$mass)-1 &
               m_max <= max(formula_library$mass)+1, ]
    pl[, max(m)]
    warning(
      "Mass range in peaklist is not completely covered by formula library.\n  ",
      a - pl[, .N],
      " peak(s) truncated from the original peak list.\n"
      )

    if (nrow(pl) == 0) {
      stop(
        "None of the masses in the peaklist (pl) are covered by the selected formula library.
           Stopping formula assignment."
      )
    }
  }

# This step truncates the formula library for performance reasons.
# The truncation takes the min and max mass in the peaklist (considering ma_de)
# and selects the mass range in the library accordingly.
  formula_library <- formula_library[mass %between% c(min(pl$m_min)-1, max(pl$m_max)+1)]

# Prefilter Peaklist ####
# If the formula library is very small (e.g. n<=100) the peaklist can be prefiltered.
# This is for the purpose of a targeted search based on a custom library or a
# single molecular formula.

  if(nrow(formula_library)<=10){
    .msg("Prefiltering peaklist.")

    prefiltered_pl <- data.table()  # Initialize empty table to store prefiltered results

  # Loop through the formulas in 'formula_library' and filter the peak list
  # based on each formula's mass

  for (i in 1:nrow(formula_library)) {
  # Filter `pl` to get only peaks that fall within a range around the formula mass
    filtered_pl_chunk <- pl[m_min >= min(formula_library[i,mass])-1 & m_max <= max(formula_library[i, mass])+1]

  # Add the filtered peaks to the prefiltered peak list
    prefiltered_pl <- rbind(prefiltered_pl, filtered_pl_chunk)
  }
  # Remove duplicates if any (e.g., if a peak appears for both formulas)
  prefiltered_pl <- unique(prefiltered_pl)
  pl <- prefiltered_pl
  }

# sort peaklist by mass
  setkeyv(pl, c("m_min", "m_max"))

# sort library by mass
  formula_library[, c("m_min", "m_max"):=mass]
  setkeyv(formula_library, c("m_min", "m_max"))
  cols <- c("m_min","m_max")

  # if(memory_efficient == TRUE) {
  #   mfd <- .foverlaps_chunked_lapply(
  #     lib = formula_library,
  #     pl  = pl,
  #     cols = cols,
  #     chunk_n = chunk_n,
  #     progress = isTRUE(verbose),
  #     show_ram = isTRUE(verbose),
  #     prefix = "Overlaps"
  #     # , deduplicate = TRUE  # enable ONLY if you confirm duplicates are spurious
  #   )
  # } else {  }
    mfd <- data.table::foverlaps(formula_library, pl, by.x = cols, by.y = cols, nomatch = 0L)


# remove helper columns 'm_min' and 'm_max'
  mfd[, c("m_min", "m_max", "i.m_min", "i.m_max"):=NULL]

# Create a unique ID for each molecular formula in a file
  mfd[, mf_id := .GRP, by = .(file_id, mf)]

# Rename column of calculated mass
  data.table::setnames(mfd, old = "mass", new = "m_cal", skip_absent = T)

# Add columns for heavy isotopes if not yet existing
  x <- c("13C", "15N", "34S")
  for (i in x) {
    if(!(i %in% names(mfd))){
      .msg("New column %s added having value 0\n", i)
      mfd<-mfd[, (i):=0]
    }
  }

# Calculate absolute and relative mass error
  mfd[, del:=round(m - m_cal, 5)]
  mfd[, ppm:=ume::calc_ma(m = m, m_cal = m_cal)]

  if(verbose) message(mfd[, .N], " formulas assigned to ", pl[, .N], " peak(s).\n")

# Check if identical formulas are assigned for different peaks of one spectrum
# to do: pretty slow as well
  x <- mfd[, .N, list(file_id, mf, `13C`, `34S`, `15N`)][order(N)][N>1]
  setkeyv(x, c("N", "mf"))

  if(verbose == TRUE){
  if(nrow(x) > 1){
    print_and_capture <- function(x) {paste(utils::capture.output(head(x[order(-N)])), collapse = "\n")}
    warning(nrow(x)
            , " formulas occur more than ones in a spectrum!\n  Maybe mass accuracy threshold set too wide? \n  Examples:\n"
            , print_and_capture(x)
            )
  }}

  setkeyv(mfd, c("file_id", "mz"))

  return(mfd)
}
