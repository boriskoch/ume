#' @title Create a custom molecular formula library for UltraMassExplorer
#'
#' @description
#' Builds a library based on a list of molecular formulas.
#' The main stable isotope masses 13C1, 15N1, and 34S1 are automatically added.

#' @inheritParams main_docu
#' @import data.table
#' @family internal functions
#' @keywords misc internal
#' @author Boris Koch
#' @return
#' A `data.table` representing a fully constructed UME molecular formula
#' library. The returned table contains one row for each input molecular
#' formula and additional rows for its isotopologues (`13C`, `15N`, `34S`)
#' when applicable. Columns include:
#'
#' \itemize{
#'   \item `vkey` – unique integer identifier for each formula/isotopologue.
#'   \item `mf` – reconstructed molecular formula string.
#'   \item `mf_iso` – isotopologue formula string.
#'   \item `nm` – nominal mass.
#'   \item `mass` – exact mass.
#'   \item Element count columns (e.g., `12C`, `13C`, `1H`, `14N`, `15N`, `32S`, `34S`).
#' }
#'
#' The library is sorted by exact mass and includes all input formulas plus any
#' automatically constructed isotopologues.
#' @export

create_custom_formula_library <- function(mf) {

  lib <- NULL
  '13C'  <- '15N' <- '34S' <- mf_iso <- NULL

# Convert molecular formulas into a data.table:
  lib <- convert_molecular_formula_to_data_table(mf = mf)

# Convert element symbols to lower case (as in ume::masses)
  #names(lib) <- tolower(names(lib))

# Set column names
  setnames(lib, "formula", "mf", skip_absent = T)

# Define mandatory columns and add if missing
  columns_to_check <- c("13C", "15N", "34S")

# Build isotope definition once
  iso_map <- build_isotope_map(masses)

  # Ensure all isotope columns exist
  all_iso_cols <- unique(c(iso_map$parent_label, iso_map$daughter_label))
  for (col in all_iso_cols) {
    if (!is.na(col) && !col %in% names(lib)) {
      lib[, (col) := 0]
    }
  }

  # Add isotopologues
  lib <- create_isotope_expanded_table(lib)
  lib[mf == "C10Cl12"]

  tmp <- convert_data_table_to_molecular_formulas(lib)
  tmp[mf == "C10Cl12"]

# Loop through each column and check if it exists, if not, add it with default value 0
  for (col in columns_to_check) {
    if (!col %in% names(lib)) {
      lib[, (col) := 0]
    }
  }

# Add isotope masses
  # 13C
  lib_c13 <- copy(lib)[, `:=`(`13C` = `13C` + 1, `12C` = `12C` - 1)]
  lib <- rbindlist(list(lib, lib_c13), fill = TRUE)

  # 15N
  if("14N" %in% names(lib)){
  lib_n15 <- copy(lib)[, `:=`(`15N` = `15N` + 1, `14N` = `14N` - 1)]
  lib <- rbindlist(list(lib, lib_n15), fill = TRUE)
  }

  # 34S
  if("32S" %in% names(lib)){
  lib_s34 <- copy(lib)[, `:=`(`34S` = `34S` + 1, `32S` = `32S` - 1)]
  lib <- rbindlist(list(lib, lib_s34), fill = TRUE)
  }

# Renew vkey
  lib[, vkey:=.I]

# Replace NA values with 0
  # lib[is.na(lib)] <- 0

# Calculate nominal and exact mass
  lib[, nm:=calc_nm(lib)]
  lib[, mass:=calc_exact_mass(lib)]

# Re-create mf strings
  convert_data_table_to_molecular_formulas(mfd = lib)
  lib[, c("mf", "mf_iso"):=convert_data_table_to_molecular_formulas(lib, isotope_formulas = T)[, .(mf, mf_iso)]]

# Order rows by mass
  setkey(lib, "mass")

# Add a unique identifier for each isotopic formula
  lib[, vkey:=1:nrow(lib)]
  lib <- lib[nm != 0]

# Order columns
  setcolorder(lib, c("vkey", "mf", "nm", "mass"))
  return(lib)
}
