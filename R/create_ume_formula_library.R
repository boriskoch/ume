#' Create a molecular formula library for UME
#'
#' Generates all combinations of element / isotope counts between
#' `min_formula` and `max_formula`, filtered by mass, DBE, element ratios,
#' and heuristic rules (Kind & Fiehn 2007).
#'
#' @inheritParams main_docu
#' @param max_formula Character. Maximum element/isotope counts, e.g.
#'   "C20H40O10" or "C1000\\[13C1\\]H2000".
#' @param min_formula Character. Minimum element/isotope counts (default "C1H1").
#' @param max_mass Numeric. Maximum allowed exact mass.
#' @param lib_version Integer. Library version identifier (default 99).
#' @param ratio_filter Logical. Apply O/C, H/C, N/C, P/C, S/C filters.
#' @param heu_filter Logical. Apply Kind - Fiehn heuristic rules.
#' @param verbose Logical. Print progress messages.
#' @param max_oc Maximum oxygen / carbon ratio in a molecule; (UM_orig: 1.5;   7 rules: 1.2)
#' @param max_hc Maximum hydrogen / carbon ratio in a molecule; (UM_orig: ;   7 rules: 1.2)
#' @param max_nc Maximum nitrogen / carbon ratio in a molecule; (UM_orig: 0.5;   7 rules: 1.3)
#' @param max_pc Maximum phosphorus / carbon ratio in a molecule; (UM_orig: 3;   7 rules: 0.3)
#' @param max_sc Maximum sulfur / carbon ratio in a molecule; (UM_orig: 4;   7 rules: 0.8)
#' @references
#' Kind T., Fiehn O. (2007). Seven Golden Rules for heuristic filtering of
#' molecular formulas obtained by accurate mass spectrometry.
#' *BMC Bioinformatics*, **8**, 105.
#' \doi{10.1186/1471-2105-8-105}
#'
#' @references
#' Hill E.A. (1900). On a system of indexing chemical literature; adopted
#' by the classification division of the U. S. patent office.
#' *Journal of the American Chemical Society*, **22**, 478-494.
#' \doi{10.1021/ja02046a005}
#'
#' @return
#' A `data.table` containing the generated molecular formula library.
#' The returned object has class `"ume_library"` and includes one row per
#' molecular formula, with columns for:
#'
#' - elemental and isotopic counts (e.g., `12C`, `13C`, `1H`, `16O`, ...)
#' - double bond equivalent (`dbe`)
#' - exact mass (`mass`)
#' - molecular formula string (`mf`)
#' - a unique versioned key (`vkey`)
#'
#' Additional metadata is stored as attributes:
#'
#' - `"lib_version"`: numeric version identifier
#' - `"min_formula"`: user-supplied minimum formula
#' - `"max_formula"`: user-supplied maximum formula
#' - `"max_mass"`: maximum allowed exact mass
#' - `"filters"`: list describing applied ratio and heuristic filters
#' - `"call"`: the matched function call
#'
#' The object inherits from both `"ume_library"` and `"data.table"`.
#' @export

create_ume_formula_library <- function(max_formula,
                                       min_formula = "C1H1",
                                       lib_version = 99,
                                       masses = ume::masses,
                                       max_mass = 152,
                                       ratio_filter = TRUE,
                                       heu_filter = TRUE,
                                       max_oc = 1.2,
                                       max_hc = 3.1,
                                       max_nc = 1.3,
                                       max_pc = 0.3,
                                       max_sc = 0.8,
                                       verbose = FALSE) {


  ## Prepare environment

  old_opt <- options(digits = 10)
  on.exit(options(old_opt), add = TRUE)

  # NSE placeholders
  #`12C` <- `13C` <- `1H` <- `14N` <- `15N` <- `16O` <- `31P` <- `32S` <- `34S` <- NULL
  max_allowed <- `13C` <- `15N` <- `34S` <- NULL
  i2 <- min_count <- max_count <- range <- dbe <- mass <- vkey <- NULL

  if (missing(max_formula) || !nzchar(max_formula)) {
    stop("Argument 'max_formula' is required.")
  }


  ## Parse min and max formulas

  .msg("Parsing formulas...")

  max_dt <- convert_molecular_formula_to_data_table(max_formula, table_format = "long")
  #setDT(max_dt)
  setnames(max_dt, "count", "max_count",skip_absent = T)

  min_dt <- convert_molecular_formula_to_data_table(min_formula, table_format = "long")
  #setDT(min_dt)
  setnames(min_dt, "count", "min_count")

  ## Merge -> build element range table
  ranges <- min_dt[, .(i2, min_count)][max_dt, on = "i2"]
  ranges[is.na(min_count), min_count := 0L]

  ## Adjust max_count by mass limit
  ranges[, max_allowed := floor(max_mass / exact_mass)]
  ranges[max_allowed < max_count, max_count := max_allowed]
  ranges[, max_allowed := NULL]

  if (any(ranges$max_count < ranges$min_count)) {
    stop("min_formula is incompatible with max_mass or max_formula.")
  }

  ## Create integer ranges for CJ()
  ranges[, range := mapply(
    function(lo, hi) seq.int(lo, hi),
    min_count, max_count,
    SIMPLIFY = FALSE
  )]

  ## Remove isotopes with zero-length ranges
  ranges <- ranges[lengths(range) > 0L]

  if (verbose) {
    msg <- ranges[, paste0(i2, ": ", min_count, "-", max_count)]
    message("Element ranges: ", paste(msg, collapse = ", "))
  }


  ## Generate Cartesian product of all element counts -------------------

  .msg("Generating formula combinations...")

  CJlist <- setNames(ranges$range, ranges$i2)
  lib <- do.call(data.table::CJ, c(CJlist, list(sorted = FALSE)))

  .msg("Generated %i raw combinations.", nrow(lib))


  ## DBE + exact mass filtering -------------------

  .msg("Computing DBE and masses...")

  lib[, dbe := calc_dbe(.SD)]
  lib <- lib[dbe >= 0 & dbe %% 1 == 0]

  lib[, mass := calc_exact_mass(.SD)]
  lib <- lib[mass <= max_mass]

  .msg("%i formulas after DBE + mass filter.", nrow(lib))


  ## Convert to Hill-system formula strings -------------------

  .msg("Converting to molecular formula strings...")
  lib <- convert_data_table_to_molecular_formulas(lib, isotope_formulas = TRUE)


  ## Zero-fill for missing isotope columns -------------------

  req_cols <- c("12C","13C","1H","14N","15N","16O","31P","32S","34S")
  for (cl in req_cols) {
    if (!cl %in% names(lib)) lib[, (cl) := 0L]
    lib[is.na(get(cl)), (cl) := 0L]
  }


  ## Element ratio filters -------------------

  if (ratio_filter) {
    .msg("Applying O/C, H/C, N/C, P/C, S/C filters...")

    carbon <- lib[, `12C` + `13C`]

    lib <- lib[
      carbon > 0 &
        (`16O`               / carbon) <= max_oc &
        (`1H`                / carbon) <= max_hc &
        ((`14N` + `15N`)     / carbon) <= max_nc &
        (`31P`               / carbon) <= max_pc &
        ((`32S` + `34S`)     / carbon) <= max_sc
    ]

    .msg("%i formulas after ratio filters.", nrow(lib))
  }


  ## Heuristic filters (Kind & Fiehn)

  if (heu_filter) {
    .msg("Applying heuristic filters...")

    n_before <- nrow(lib)

    lib <- lib[
      !(
        # OPS
        ((`16O` > 1 & `31P` > 1 & `32S` > 1) &
           (`16O` >= 14 | `31P` >= 3 | `32S` >= 3)) |
          # PSN
          ((`14N` > 1 & `31P` > 1 & `32S` > 1) &
             (`14N` >= 4 | `31P` >= 3 | `32S` >= 3)) |
          # NOS
          ((`14N` > 6 & `16O` > 6 & `32S` > 6) &
             (`14N` >= 19 | `16O` >= 14 | `32S` >= 8)) |
          # NOP
          ((`14N` > 3 & `16O` > 3 & `31P` > 3) &
             (`14N` >= 11 | `16O` >= 22 | `31P` >= 6)) |
          # NOPS
          ((`14N` > 1 & `16O` > 1 & `31P` > 1 & `32S` > 1) &
             (`14N` >= 10 | `16O` >= 20 | `31P` >= 4 | `32S` >= 3))
      )
    ]

    if (verbose)
      message(n_before - nrow(lib), " removed by heuristic filters.")
  }


  ## Final sorting & library version keys

  setkey(lib, mass)

  lib[, vkey := lib_version * 10^12 + seq_len(.N)]


  ## Add class + metadata

  attr(lib, "lib_version") <- lib_version
  attr(lib, "min_formula") <- min_formula
  attr(lib, "max_formula") <- max_formula
  attr(lib, "max_mass") <- max_mass
  attr(lib, "filters") <- list(
    ratio_filter = ratio_filter,
    heu_filter   = heu_filter,
    max_oc       = max_oc,
    max_hc       = max_hc,
    max_nc       = max_nc,
    max_pc       = max_pc,
    max_sc       = max_sc
  )
  attr(lib, "call") <- match.call()

  class(lib) <- c("ume_library", class(lib))

  .msg("Done. Final library contains %i formulas.", nrow(lib))

  return(lib[])
}
