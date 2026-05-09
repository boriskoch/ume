#' @title Evaluate isotope information
#' @name eval_isotopes
#' @description Add isotope information to the parent mass and optionally remove isotopoloques from mfd table.
#' Required for further data evaluation that considers isotope information.
#' @family Formula assignment
#' @family isotopes
#' @inheritParams main_docu
#' @param remove_isotopes If set to TRUE (default), all entries for isotopologues are removed from mfd.
#' The main isotope information for each parent ion is still maintained in the "intxy"-columns.
#' @author Boris P. Koch
#' @return A data.table with additional columns such as "int_13c" containing stable isotope abundance information.
#' @examples eval_isotopes(mfd = mf_data_demo)
#' @import data.table
#' @export

eval_isotopes <-
  function(mfd,
           remove_isotopes = TRUE,
           verbose = FALSE,
           ...) {

  mfd <- check_mfd(mfd)

  '12C' <- '13C'  <- '15N' <- '34S' <- NULL

# Make sure that isotope information is available
# Are the columns for isotopes present?
  iso_cols <- c("13C", "15N", "34S")
  missing_iso_cols <- iso_cols[!(iso_cols %in% names(mfd))]

  if (length(missing_iso_cols) >= 1) {
    message("\nMissing columns for isotope evaluation (required: 13C, 15N, 34S).\n",
    "Maybe isotopes are already evaluated?")
    return(mfd)
    }

# Does any of the isotope columns contain a value >0?
  if(nrow(mfd[`13C`>0 | `15N`>0 | `34S`>0, ])==0){
    warning("Molecular formula table does not contain isotope information!\n Evaluation of isotopes failed.")
    return(mfd)
    }

# Remove existing isotope evaluation columns
  int_cols <- c("int13c", "int15n", "int34s")
  existing_int_cols <- int_cols[(int_cols %in% colnames(mfd))]
  if (length(existing_int_cols) >= 1)
  mfd[, (existing_int_cols) := NULL]

# Select 13C-isotope signal
  mfd <-
    mfd[`13C` == 1 &
          `34S` == 0 &
          `15N` == 0, .(int13c = max(i_magnitude)), mf_id][mfd, on = "mf_id"]
  mfd[is.na(int13c), int13c := 0] # if isotope signal is not assigned: set to zero

# Select 34S-isotope signal
  mfd <-
    mfd[`13C` == 0 &
          `15N` == 0 &
          `34S` == 1, .(int34s = max(i_magnitude)), mf_id][mfd, on = "mf_id"]
  mfd[is.na(int34s), int34s := 0] # if isotope signal is not assigned: set to zero

# Select 15N-isotope signal
  if (mfd[, max(`15N`)] > 0) {
    mfd <-
      mfd[`13C` == 0 &
            `15N` == 1 &
            `34S` == 0, .(int15n = max(i_magnitude)), mf_id][mfd, on = "mf_id"]
    mfd[is.na(int15n), int15n := 0] # if isotope signal is not assigned: set to zero
  } else {
    mfd[, int15n := 0]
  }

# Remove all isotope daughters (information is still maintained in the "intxy"-columns of each parent ion)
  if (remove_isotopes == T) {
    mfd <- mfd[`13C` == 0 & `34S` == 0 & `15N` == 0, ]
    mfd[, c("13C", "15N", "34S") := NULL] # remove isotope columns

    if (verbose == T) {
      message(
        "Removing heavy isotope assignments (isotope information for each parent ion is still available in columns 'intxy').\n"
      )
      message("Number of formulas remaining: ", nrow(mfd), "\n")
    }
  }

# Calculate dev_n_c
  mfd[int13c > 0, dev_n_c := round((((
    int13c * 100 / i_magnitude
  ) / 1.08) - `12C`), 2)]
  mfd[int13c == 0, dev_n_c := 999] # Assign standard value for parent ions without daughter

  return(mfd[])
}
