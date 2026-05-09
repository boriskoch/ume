#' @title Search database for target molecular formulas
#' @name search_mf_targets
#' @description
#' An internal function that searches the MarChem peaklist database for a table of target molecular
#' formulas or isotopologues and returns the raw peak hits while preserving
#' all columns of the input target table.
#'
#' @param target_table A `data.table` containing at least a formula column and
#'   an exact mass column (`mass`). Additional columns such as `mf`,
#'   `mf_iso`, `isotope_group_id`, `iso_role`, `iso_element`, `iso_from`,
#'   and `iso_to` are preserved in the output.
#' @param formula_col Name of the column in `target_table` containing the
#'   formula identifier to be tracked in the search result. Default is `"mf_iso"`.
#' @param ppm_window Mass accuracy window (+/-) used for the search
#'   (numeric; default: `0.5` ppm).
#' @param adduct_mass Mass of the proton to convert neutral mass to measured
#'   `m/z`. Default is `1.0072763`.
#'
#' @return A `data.table` containing raw peak hits from
#'   `"tab_ume_peaklists"` plus all columns from `target_table`.
#' @family internal functions
#' @keywords internal


search_mf_targets <- function(target_table,
                              formula_col = "mf_iso",
                              ppm_window = 0.5,
                              adduct_mass = 1.0072763) {

  mz <- abs_dev <- lower_limit <- upper_limit <- NULL

  if (!data.table::is.data.table(target_table)) {
    target_table <- data.table::as.data.table(target_table)
  }

  if (!formula_col %in% names(target_table)) {
    stop("Column '", formula_col, "' not found in 'target_table'.")
  }

  if (!"mass" %in% names(target_table)) {
    stop("Column 'mass' not found in 'target_table'.")
  }

  if (any(is.na(target_table[[formula_col]]))) {
    stop("Column '", formula_col, "' contains missing values.")
  }

  if (any(target_table[[formula_col]] %like% "D")) {
    stop(
      "Column '", formula_col,
      "' contains a formula with 'D'. Consider revising as '[2H]'."
    )
  }

  lib <- data.table::copy(target_table)

  lib[, mz := mass - adduct_mass]
  lib[, abs_dev := mz * ppm_window / 1e6]
  lib[, lower_limit := mz - abs_dev]
  lib[, upper_limit := mz + abs_dev]

  res_list <- vector("list", nrow(lib))

  for (i in seq_len(nrow(lib))) {

    sql <- paste0(
      "mz >= ", lib$lower_limit[i],
      " AND mz <= ", lib$upper_limit[i]
    )

    hits <- do.call(
      getFromNamespace("f_sam_get_data", "sam"),
      list(
        "tab_ume_peaklists",
        sql_where = sql
      )
    )

    if (nrow(hits) > 0) {
      # attach full target row to every hit
      target_row <- lib[i]

      # remove search helper columns if not wanted downstream
      target_row[, c("mz", "abs_dev", "lower_limit", "upper_limit") := NULL]

      hits <- cbind(hits, target_row[rep(1, nrow(hits))])
      res_list[[i]] <- hits
    }
  }

  res <- data.table::rbindlist(res_list, fill = TRUE, use.names = TRUE)
  res[]
}
