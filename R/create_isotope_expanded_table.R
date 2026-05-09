#' @title Create an Expanded Table of Parent and Isotope Daughter Formulas
#' @name create_isotope_expanded_table
#' @description
#' Creates a new molecular formula table containing the original parent formulas
#' and their corresponding single-isotope daughter formulas.
#'
#' @details
#' The output includes annotation columns that facilitate isotope validation in
#' downstream workflows:
#' \itemize{
#'   \item `iso_role` indicates whether a row represents a `"parent"` or
#'   `"daughter"` isotopologue.
#'   \item `iso_element` stores the element symbol for which the isotope
#'   substitution was generated (e.g. `"C"`, `"N"`, `"S"`).
#'   \item `iso_from` and `iso_to` store the parent and daughter isotope labels
#'   (e.g. `"12C"` and `"13C"`).
#' }
#'
#' @param mfd A `data.table` containing molecular formula information in wide
#'   format, including isotope count columns, or a character vector of molecular
#'   formulas. Character input is first converted with
#'   [convert_molecular_formula_to_data_table()].
#' @param id_col Name of the column in `mfd` used to define isotope groups.
#'   Default is `"peak_id"`.
#' @param allow_duplicates Logical. If `TRUE` (default), isotope daughter formulas
#'   are created for each input row using `id_col` as group identifier. If
#'   `FALSE`, the result is based on unique isotope compositions only.
#' @param elements Optional character vector of element symbols
#'   (matching `masses$symbol`) to restrict isotope expansion. If `NULL`
#'   (default), all eligible elements detected in `mfd` are used.
#'
#' @return A `data.table` containing parent and daughter formulas, including
#'   isotope annotation columns for downstream validation.
#' @family isotopes
#' @keywords isotopes
#' @import data.table
#' @export

create_isotope_expanded_table <- function(mfd,
                                          id_col = "peak_id",
                                          allow_duplicates = TRUE,
                                          elements = NULL) {

  isotope_group_id <- mf_iso <- element <- iso_role <- NULL
  iso_element <- iso_from <- iso_to <- NULL

# if mfd is a character vector
  if (is.factor(mfd)) {
    mfd <- as.character(mfd)
  }

  if (is.character(mfd)) {
    mfd <- convert_molecular_formula_to_data_table(mf = mfd)

    # character input gets vkey from convert_molecular_formula_to_data_table()
    if (identical(id_col, "peak_id") && !"peak_id" %in% names(mfd)) {
      id_col <- "vkey"
    }
  }

  if (!data.table::is.data.table(mfd)) {
    mfd <- data.table::as.data.table(mfd)
  }

  empty_res <- data.table(
    isotope_group_id = integer(),
    iso_role = character(),
    iso_element = character(),
    iso_from = character(),
    iso_to = character(),
    mf = character(),
    mf_iso = character(),
    nm = numeric(),
    mass = numeric()
  )

  isotope_map <- build_isotope_map(mfd)

  if (!is.null(elements)) {
    elements <- unique(elements)

    if (!all(elements %in% masses$symbol)) {
      stop(
        "Unknown element(s) in 'elements': ",
        paste(setdiff(elements, masses$symbol), collapse = ", ")
      )
    }

    isotope_map <- isotope_map[element %in% elements]
  }

  if (nrow(isotope_map) == 0) {
    return(empty_res)
  }

  iso_cols <- intersect(names(mfd), masses$label)

  if (!length(iso_cols)) {
    return(empty_res)
  }

  id_available <- id_col %in% names(mfd)

  if (allow_duplicates && !id_available) {
    message(
      "Column '", id_col, "' was not found in 'mfd'. ",
      "The isotope table will therefore be generated from unique isotope compositions only."
    )
    allow_duplicates <- FALSE
  }

  if (allow_duplicates) {
    keep_cols <- unique(c(id_col, iso_cols))
    lib <- copy(mfd[, keep_cols, with = FALSE])
    lib[, isotope_group_id := get(id_col)]
  } else {
    lib <- unique(copy(mfd[, iso_cols, with = FALSE]))
    lib[, isotope_group_id := .I]
  }

  needed_cols <- unique(c(isotope_map$parent_label, isotope_map$daughter_label))
  needed_cols <- needed_cols[!is.na(needed_cols)]

  missing_cols <- setdiff(needed_cols, names(lib))
  if (length(missing_cols)) {
    for (cc in missing_cols) {
      lib[, (cc) := 0L]
    }
  }

  # Parent table
  parent_tbl <- copy(lib)
  parent_tbl[, `:=`(
    iso_role = "parent",
    iso_element = NA_character_,
    iso_from = NA_character_,
    iso_to = NA_character_
  )]

  parent_tbl[, nm := calc_nm(parent_tbl)]
  parent_tbl[, mass := calc_exact_mass(parent_tbl)]

  tmp_parent <- convert_data_table_to_molecular_formulas(parent_tbl, isotope_formulas = TRUE)
  parent_tbl[, c("mf", "mf_iso") := tmp_parent[, .(mf, mf_iso)]]

  out <- vector("list", nrow(isotope_map))
  k <- 0L

  for (i in seq_len(nrow(isotope_map))) {

    parent <- isotope_map$parent_label[i]
    daughter <- isotope_map$daughter_label[i]
    elem <- isotope_map$element[i]

    if (is.na(daughter)) next
    if (!parent %in% names(lib)) next
    if (!daughter %in% names(lib)) next

    dt <- lib[get(parent) > 0]
    if (nrow(dt) == 0) next

    iso_dt <- copy(dt)[
      , c(parent, daughter) := .(
        get(parent) - 1L,
        get(daughter) + 1L
      )
    ]

    iso_dt[, `:=`(
      iso_role = "daughter",
      iso_element = elem,
      iso_from = parent,
      iso_to = daughter
    )]

    k <- k + 1L
    out[[k]] <- iso_dt
  }

  if (k > 0L) {
    daughter_tbl <- rbindlist(out[seq_len(k)], fill = TRUE, use.names = TRUE)

    daughter_tbl[, nm := calc_nm(daughter_tbl)]
    daughter_tbl[, mass := calc_exact_mass(daughter_tbl)]

    tmp_daughter <- convert_data_table_to_molecular_formulas(daughter_tbl, isotope_formulas = TRUE)
    daughter_tbl[, c("mf", "mf_iso") := tmp_daughter[, .(mf, mf_iso)]]

    res <- rbindlist(list(parent_tbl, daughter_tbl), fill = TRUE, use.names = TRUE)
  } else {
    res <- parent_tbl
  }

  first_cols <- c("isotope_group_id", "iso_role", "iso_element", "iso_from", "iso_to")
  if (allow_duplicates && id_available) {
    first_cols <- c(first_cols, id_col)
  }

  keep_cols <- unique(c(
    first_cols,
    "mf", "mf_iso", "nm", "mass",
    intersect(names(res), masses$label)
  ))

  res <- res[, keep_cols, with = FALSE]

  if (!allow_duplicates) {
    res <- unique(res)
  }

  setcolorder(
    res,
    c("isotope_group_id",
      "iso_role", "iso_element", "iso_from", "iso_to",
      intersect(id_col, names(res)),
      "mf", "mf_iso", "nm", "mass",
      setdiff(names(res), c(
        "isotope_group_id", "iso_role", "iso_element", "iso_from", "iso_to",
        id_col, "mf", "mf_iso", "nm", "mass"
      )))
  )

  res[]
}
