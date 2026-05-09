#' @title Convert Data Table with Element Counts to Molecular Formulas
#'
#' @description
#' Creates standardized molecular formula strings from isotope or element count
#' columns and adds them to the input `data.table`.
#'
#' @details
#' The function extracts element or isotope counts from a table with one column
#' per isotope or element. Valid isotope columns are detected using
#' [get_isotope_info()] and the reference table `ume::masses`.
#'
#' The standard molecular formula `mf` is created by summing isotopes belonging
#' to the same element and arranging elements according to Hill order.
#'
#' If `isotope_formulas = TRUE`, an additional `mf_iso` column is created that
#' keeps isotope-specific information, for example `[12C5][13C][1H12][16O6]`.
#'
#' The function preserves the original row order and keeps duplicate rows.
#'
#' @inheritParams main_docu
#' @param isotope_formulas Logical. If `TRUE`, an additional isotope-specific
#'   molecular formula string `mf_iso` is created.
#' @param keep_element_sums Logical. If `TRUE`, additional columns with total
#'   atom counts per element are returned, for example `C_tot`.
#' @param verbose Logical. If `TRUE`, progress messages are printed.
#' @param ... Additional arguments passed to [get_isotope_info()].
#'
#' @return
#' The original table `mfd` as a `data.table` with additional columns:
#' \describe{
#'   \item{mf}{Standardized molecular formula following Hill order.}
#'   \item{mf_iso}{If `isotope_formulas = TRUE`, isotope-specific molecular
#'   formula.}
#'   \item{C_tot}{If `keep_element_sums = TRUE`, total count of all carbon
#'   isotopes. Equivalent `*_tot` columns are created for other elements.}
#' }
#'
#' @section Notes:
#' - Isotopic columns such as `13C` are formatted as `[13C]` in `mf_iso`.
#' - The output follows Hill order: `C`, `H`, then all other elements
#'   alphabetically.
#' - Single-element counts, e.g. `C1H4`, are formatted without explicit `1`.
#'
#' @references
#' Hill E.A. (1900). On a system of indexing chemical literature; adopted
#' by the classification division of the U. S. patent office.
#' *Journal of the American Chemical Society*, **22**, 478-494.
#' \doi{10.1021/ja02046a005}
#'
#' @examples
#' convert_data_table_to_molecular_formulas(
#'   mf_data_demo[, .(`12C`, `1H`, `14N`, `16O`, `31P`, `32S`)]
#' )
#'
#' @family molecular formula functions
#' @keywords chemistry molecular-formula
#' @import data.table
#' @export

convert_data_table_to_molecular_formulas <- function(mfd,
                                                     isotope_formulas = FALSE,
                                                     keep_element_sums = FALSE,
                                                     verbose = FALSE,
                                                     ...) {

  keep_cols <- new_name <- orig_name <- mf_iso <- NULL
  .ume_row_id <- label <- count <- hill_order <- symbol <- NULL
  count_element <- mf <- NULL

  # --- input handling -------------------------------------------------------
  if (!data.table::is.data.table(mfd)) {
    mfd <- data.table::as.data.table(mfd)
  } else {
    mfd <- data.table::copy(mfd)
  }

  # preserve original row order and duplicates
  mfd[, .ume_row_id := .I]

  # --- save existing output columns ----------------------------------------
  cols_to_rename <- character()

  if ("mf" %in% names(mfd)) {
    cols_to_rename <- c(cols_to_rename, "mf")
  }

  if (isTRUE(isotope_formulas) && "mf_iso" %in% names(mfd)) {
    cols_to_rename <- c(cols_to_rename, "mf_iso")
  }

  if (isTRUE(keep_element_sums)) {
    tot_cols <- grep("_tot$", names(mfd), value = TRUE)
    cols_to_rename <- c(cols_to_rename, tot_cols)
  }

  cols_to_rename <- unique(cols_to_rename)

  if (length(cols_to_rename) > 0L) {
    data.table::setnames(
      mfd,
      old = cols_to_rename,
      new = paste0(cols_to_rename, "_old")
    )
  }

  # --- verify isotope columns ----------------------------------------------
  iso_cols <- get_isotope_info(mfd, ...)

  if (nrow(iso_cols) == 0L) {
    stop(
      "No valid isotope or element count columns were detected in 'mfd'.",
      call. = FALSE
    )
  }

  # Rename isotope columns in mfd to official labels
  data.table::setnames(
    mfd,
    iso_cols$orig_name,
    iso_cols$label,
    skip_absent = TRUE
  )

  # Make sure all isotope columns are integer type
  mfd[
    ,
    (iso_cols$label) := lapply(.SD, as.integer),
    .SDcols = iso_cols$label
  ]

  # --- reshape to long format ----------------------------------------------
  dt_long <- data.table::melt(
    mfd,
    measure.vars = iso_cols$label,
    variable.name = "label",
    value.name = "count",
    id.vars = ".ume_row_id",
    variable.factor = FALSE
  )

  dt_long <- dt_long[!is.na(count) & count > 0L]

  # Join isotope metadata
  dt_long <- iso_cols[
    ,
    .(hill_order, label, symbol)
  ][
    dt_long,
    on = "label"
  ]

  data.table::setorder(
    dt_long,
    .ume_row_id,
    hill_order,
    symbol,
    label
  )

  # --- build standard molecular formula ------------------------------------
  df_mf <- dt_long[
    ,
    .(
      count_element = sum(count),
      hill_order = min(hill_order)
    ),
    by = .(.ume_row_id, symbol)
  ]

  data.table::setorder(
    df_mf,
    .ume_row_id,
    hill_order,
    symbol
  )

  df_mf[
    count_element == 1L,
    mf := symbol
  ]

  df_mf[
    count_element > 1L,
    mf := paste0(symbol, count_element)
  ]

  if (isTRUE(keep_element_sums)) {
    df_mf_sums <- data.table::dcast(
      df_mf,
      .ume_row_id ~ symbol,
      value.var = "count_element",
      fill = 0
    )

    data.table::setnames(
      df_mf_sums,
      names(df_mf_sums)[-1],
      paste0(names(df_mf_sums)[-1], "_tot")
    )
  }

  if (isTRUE(verbose)) {
    message("Creating molecular formula string...")
  }

  df_mf <- df_mf[
    ,
    .(mf = paste0(mf, collapse = "")),
    by = .(.ume_row_id)
  ]

  # --- build isotope-specific molecular formula ----------------------------
  if (isTRUE(isotope_formulas)) {

    df_mf_iso <- dt_long[
      ,
      .(
        count_element = sum(count),
        hill_order = min(hill_order)
      ),
      by = .(.ume_row_id, label)
    ]

    df_mf_iso <- iso_cols[
      ,
      .(label, symbol, hill_order)
    ][
      df_mf_iso,
      on = "label"
    ]

    data.table::setorder(
      df_mf_iso,
      .ume_row_id,
      hill_order,
      symbol,
      label
    )

    df_mf_iso[
      count_element == 1L,
      mf_iso := paste0("[", label, "]")
    ]

    df_mf_iso[
      count_element > 1L,
      mf_iso := paste0("[", label, count_element, "]")
    ]

    if (isTRUE(verbose)) {
      message("Creating molecular formula string with isotope information...")
    }

    df_mf_iso <- df_mf_iso[
      ,
      .(mf_iso = paste0(mf_iso, collapse = "")),
      by = .(.ume_row_id)
    ]

    df_mf <- df_mf_iso[df_mf, on = ".ume_row_id"]
  }

  # --- join results back to original table ---------------------------------
  mfd <- df_mf[mfd, on = ".ume_row_id"]

  if (isTRUE(keep_element_sums)) {
    mfd <- df_mf_sums[mfd, on = ".ume_row_id"]
  }

  data.table::setorder(mfd, .ume_row_id)
  mfd[, .ume_row_id := NULL]

  if (isTRUE(verbose)) {
    message("Molecular formula strings created.")
  }

  mfd[]
}
