#' Validate Molecular Formulas by Presence of Isotope Daughter Signals
#'
#' @title Validate isotope presence
#' @name validate_isotope_presence
#'
#' @description
#' Validates parent molecular formulas based on the presence or absence of
#' corresponding isotope daughter signals within the same file.
#'
#' @details
#' The function is designed to work on matched target-search results derived
#' from an isotope-expanded target table created with
#' [create_isotope_expanded_table()].
#'
#' Validation is based on co-occurrence of parent and daughter isotope signals
#' within the same `file_id`.
#'
#' For each combination of `file_id` and `isotope_group_id`, the function:
#' \itemize{
#'   \item checks whether the parent formula was found,
#'   \item determines which requested isotope systems are expected for that
#'     isotope group,
#'   \item determines which of those expected isotope systems were found,
#'   \item and assigns an isotope validation class.
#' }
#'
#' The actual validation is performed within each `file_id`. However, the list
#' of expected isotope daughter signals can be derived from `dt_expected`, which
#' should ideally be the complete isotope-expanded target table. This prevents
#' missing daughter isotope signals from being incorrectly ignored when
#' validating a subset of files.
#'
#' If `elements = c("C", "N", "S")` is requested, a formula is only required
#' to match those daughter isotope elements that are actually expected for its
#' isotope group. Thus, formulas lacking sulfur or nitrogen are not penalized
#' for missing S or N daughter signals.
#'
#' @param dt_target_results A `data.table` containing matched target-search
#'   results. It must contain at least the columns `file_id`,
#'   `isotope_group_id`, `mf`, `iso_role`, and `iso_element`.
#' @param elements Character vector of element symbols to be considered for
#'   isotope presence validation, for example `c("C", "S")`.
#' @param require_all Logical. If `TRUE` (default), all expected isotope
#'   daughter systems for a given isotope group must be present for the result
#'   to be classified as `"validated_all"`. If `FALSE`, presence of at least
#'   one expected daughter isotope is sufficient for classification as
#'   `"validated_partial"`.
#' @param dt_expected Optional `data.table` used to determine which isotope
#'   daughter systems are expected for each `isotope_group_id`. By default,
#'   `dt_target_results` is used. For robust validation of subsets, this should
#'   ideally be the complete isotope-expanded target table or an unfiltered
#'   matched result table.
#'
#' @return A `data.table` with one row per combination of `file_id` and
#'   `isotope_group_id`, containing:
#' \describe{
#'   \item{file_id}{File identifier.}
#'   \item{isotope_group_id}{Identifier linking parent and daughter isotopologues.}
#'   \item{mf}{Parent molecular formula.}
#'   \item{parent_found}{Logical indicating whether the parent formula was found.}
#'   \item{n_elements_requested}{Number of user-requested isotope elements.}
#'   \item{n_isotopes_expected}{Number of requested isotope daughter systems
#'     that are actually expected for the isotope group.}
#'   \item{n_isotopes_found}{Number of expected isotope daughter systems found.}
#'   \item{isotopes_expected}{Comma-separated list of expected isotope daughter
#'     elements.}
#'   \item{isotopes_found}{Comma-separated list of found isotope daughter elements.}
#'   \item{isotope_validation}{Validation class based on isotope presence.}
#' }
#'
#' @section Isotope validation classes:
#' \itemize{
#'   \item `validated_all` -- parent found and all expected daughter isotope
#'     systems found.
#'   \item `validated_partial` -- parent found and at least one expected daughter
#'     isotope system found.
#'   \item `parent_only` -- parent found, but none of the expected daughter
#'     isotope systems found.
#'   \item `daughter_only` -- daughter isotope signal found without parent.
#' }
#'
#' @family internal functions
#' @keywords internal isotopes
#' @import data.table
#' @author Boris Koch
validate_isotope_presence <- function(dt_target_results,
                                      elements,
                                      require_all = TRUE,
                                      dt_expected = dt_target_results) {

  file_id <- isotope_group_id <- mf <- iso_role <- iso_element <- NULL
  parent_found <- n_isotopes_found <- isotope_validation <- NULL
  isotopes_expected <- n_isotopes_expected <- n_elements_requested <- NULL

  required_cols <- c("file_id", "isotope_group_id", "mf", "iso_role", "iso_element")

  if (!data.table::is.data.table(dt_target_results)) {
    dt_target_results <- data.table::as.data.table(dt_target_results)
  }

  if (!data.table::is.data.table(dt_expected)) {
    dt_expected <- data.table::as.data.table(dt_expected)
  }

  if (!all(required_cols %in% names(dt_target_results))) {
    stop(
      "Input table 'dt_target_results' must contain the following columns: ",
      paste(required_cols, collapse = ", "), "."
    )
  }

  if (!all(required_cols %in% names(dt_expected))) {
    stop(
      "Input table 'dt_expected' must contain the following columns: ",
      paste(required_cols, collapse = ", "), "."
    )
  }

  if (missing(elements) || length(elements) == 0) {
    stop("Argument 'elements' must contain at least one element symbol.")
  }

  elements <- unique(as.character(elements))

  x <- dt_target_results[
    iso_role == "parent" |
      (iso_role == "daughter" & iso_element %in% elements)
  ]

  if (nrow(x) == 0) {
    return(data.table::data.table(
      file_id = integer(),
      isotope_group_id = integer(),
      mf = character(),
      parent_found = logical(),
      n_elements_requested = integer(),
      n_isotopes_expected = integer(),
      n_isotopes_found = integer(),
      isotopes_expected = character(),
      isotopes_found = character(),
      isotope_validation = character()
    ))
  }

  expected_by_group <- unique(
    dt_expected[
      iso_role == "daughter" & iso_element %in% elements,
      .(isotope_group_id, iso_element)
    ]
  )[
    ,
    .(
      n_isotopes_expected = data.table::uniqueN(iso_element),
      isotopes_expected = paste(sort(unique(iso_element)), collapse = ",")
    ),
    by = isotope_group_id
  ]

  val <- x[
    ,
    .(
      mf = {
        tmp <- unique(mf[!is.na(mf)])
        if (length(tmp) == 0) NA_character_ else tmp[1]
      },
      parent_found = any(iso_role == "parent"),
      n_isotopes_found = data.table::uniqueN(
        iso_element[iso_role == "daughter" & iso_element %in% elements]
      ),
      isotopes_found = {
        tmp <- sort(unique(
          iso_element[iso_role == "daughter" & iso_element %in% elements]
        ))
        if (length(tmp) == 0) "" else paste(tmp, collapse = ",")
      }
    ),
    by = .(file_id, isotope_group_id)
  ]

  val <- expected_by_group[val, on = "isotope_group_id"]

  val[is.na(n_isotopes_expected), n_isotopes_expected := 0L]
  val[is.na(isotopes_expected), isotopes_expected := ""]

  val[, n_elements_requested := length(elements)]

  if (isTRUE(require_all)) {
    val[
      ,
      isotope_validation := data.table::fifelse(
        !parent_found & n_isotopes_found > 0, "daughter_only",
        data.table::fifelse(
          parent_found & n_isotopes_expected == 0, "parent_only",
          data.table::fifelse(
            parent_found & n_isotopes_found == n_isotopes_expected,
            "validated_all",
            data.table::fifelse(
              parent_found & n_isotopes_found > 0,
              "validated_partial",
              "parent_only"
            )
          )
        )
      )
    ]
  } else {
    val[
      ,
      isotope_validation := data.table::fifelse(
        !parent_found & n_isotopes_found > 0, "daughter_only",
        data.table::fifelse(
          parent_found & n_isotopes_found > 0,
          "validated_partial",
          "parent_only"
        )
      )
    ]
  }

  data.table::setcolorder(
    val,
    c(
      "file_id", "isotope_group_id", "mf", "parent_found",
      "n_elements_requested",
      "n_isotopes_expected", "n_isotopes_found",
      "isotopes_expected", "isotopes_found",
      "isotope_validation"
    )
  )

  val[]
}
